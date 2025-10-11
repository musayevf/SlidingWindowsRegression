# Helper function: Compute KL divergence between two vectors P and Q.
compute_kl_divergence <- function(P, Q, epsilon = 1e-10) {
  max_len <- max(length(P), length(Q))
  P <- c(rep(0, max_len - length(P)), P)
  Q <- c(rep(0, max_len - length(Q)), Q)
  
  # Normalize
  if (sum(P) > 0) P <- P / sum(P)
  if (sum(Q) > 0) Q <- Q / sum(Q)
  
  # Only compute over P > 0
  nonzero_idx <- which(P > 0)
  Pnz <- P[nonzero_idx]
  Qnz <- Q[nonzero_idx]
  
  # Smooth Q only
  Qnz <- ifelse(Qnz == 0, epsilon, Qnz)
  
  divergence <- sum(Pnz * log(Pnz / Qnz))
  divergence <- pmax(divergence, 0)
  
  return(divergence)
}

#Creates and adds spline interpolation to dataset and normalization of values
spline_interpolation <- function(df, seasons, normalize_values = TRUE) {
  # --- Step 1: Ensure the date column is Date class ---
  df$date <- as.Date(df$date)
  
  # --- Step 2: Create lookup vector from seasonal values (exclude gridcode) ---
  season_lookup <- unlist(seasons[, setdiff(names(seasons), "gridcode")])
  
  # --- Step 3: Extract day and month abbreviation ---
  df <- df %>%
    mutate(day = lubridate::day(date),
           month_abbr = format(date, "%b"))
  
  # --- Step 4: Assign seasonal value only for 15th day of each month ---
  df <- df %>%
    mutate(seasonal_value = if_else(day == 15,
                                    as.numeric(season_lookup[month_abbr]),
                                    NA_real_))
  
  # --- Step 5: Spline interpolation to fill NAs ---
  numeric_dates <- as.numeric(df$date)
  x_known <- numeric_dates[!is.na(df$seasonal_value)]
  y_known <- df$seasonal_value[!is.na(df$seasonal_value)]
  
  spline_result <- spline(x = x_known,
                          y = y_known,
                          xout = numeric_dates)
  
  # --- Step 6: Add interpolated values ---
  df$seasonal_value_filled <- spline_result$y
  
  # --- Step 7: Normalize (optional) ---
  if (normalize_values) {
    rng <- range(df$seasonal_value_filled, na.rm = TRUE)
    df$seasonal_value_normalized <- (df$seasonal_value_filled - rng[1]) / (rng[2] - rng[1])
  }
  
  # --- Step 8: Remove helper columns ---
  df <- df %>% select(-day, -month_abbr)
  
  return(df)
}

# Helper function to build kernels from a trained Sliding Window Regression model
build_kernels <- function(model, kernel = 3, weighted = TRUE) {
  # 1. Compute each raw kernel
  kern_list <- SlidingWindowReg:::get_kernel(model$param_hist[[kernel]]$param,
                                             model$param_hist[[kernel]]$mix,
                                             weighted = weighted)
  return(kern_list)
}

#Compare two SWR models using KL divergence of their kernels.
compare_models_kld <- function(model1, model2, n_kernels = 3) {
  # 1) build the kernel vectors
  K1 <- build_kernels(model1, kernel = n_kernels)
  K2 <- build_kernels(model2, kernel = n_kernels)
  
  # 2) check how many we really have
  n <- min(nrow(K1), nrow(K2))
  
  if (n < n_kernels) {
    warning("Only found ", n, " kernels to compare (requested ", n_kernels, ").")
  }
  
  # 3) subset everything to the first n
  K1 <- K1[1:n, , drop = FALSE]
  K2 <- K2[1:n, , drop = FALSE]
  P1 <- model1$param_hist[[n_kernels]]$param
  P2 <- model2$param_hist[[n_kernels]]$param
  B1 <- model1$param_hist[[n_kernels]]$mix
  B2 <- model2$param_hist[[n_kernels]]$mix
  
  kernels1 <- rownames(K1)
  kernels2 <- rownames(K2)
  
  # 4) compute the n×n KL matrix
  D <- outer(
    seq_len(n), seq_len(n),
    Vectorize(function(i, j) compute_kl_divergence(K1[i,], K2[j,]))
  )
  dimnames(D) <- list(kernels1, kernels2)
  
  # 5) best‐matches and reorder model2’s params/mixes(greedy one‐to‐one)
  best_idx   <- integer(n)
  taken      <- rep(FALSE, n)
  for (i in seq_len(n)) {
    # mask out already‐taken columns
    d_i <- D[i, ]
    d_i[taken] <- Inf
    # pick the closest free kernel
    j_min <- which.min(d_i)
    best_idx[i] <- j_min
    taken[j_min] <- TRUE
  }
  
  P2_matched <- P2[best_idx, , drop = FALSE]
  B2_matched <- B2[best_idx]
  
  # 6) pack side‐by‐side parameter+mix table
  params <- cbind(
    delta1 = P1[,1], sigma1 = P1[,2], beta1 = B1,
    delta2 = P2_matched[,1], sigma2 = P2_matched[,2], beta2 = B2_matched
  )
  rownames(params) <- kernels1
  
  # 7) return everything
  list(
    divergence_matrix = D,
    best_matches       = kernels2[best_idx],
    param_matrix       = params
  )
}

#Combines parameters of two different SWR() models
combine_params <- function(param_matrix, alpha = 0.5) {
  # param_matrix: output$param_matrix from compare_models_kld()
  # columns must be named delta1, sigma1, beta1, delta2, sigma2, beta2
  
  # sanity check:
  stopifnot(all(c("delta1","sigma1","beta1","delta2","sigma2","beta2")
                %in% colnames(param_matrix)))
  stopifnot(alpha >= 0 && alpha <= 1)
  
  # compute blended values
  delta_new <- alpha * param_matrix[,"delta1"] +
    (1 - alpha) * param_matrix[,"delta2"]
  sigma_new <- alpha * param_matrix[,"sigma1"] +
    (1 - alpha) * param_matrix[,"sigma2"]
  beta_new  <- alpha * param_matrix[,"beta1"]  +
    (1 - alpha) * param_matrix[,"beta2"]
  
  # build output matrix
  result <- cbind(
    delta = delta_new,
    sigma = sigma_new,
    beta  = beta_new
  )
  rownames(result) <- rownames(param_matrix)
  result
}

#Adding new predictions using parameters and SWR object
add_new_predictions <- function(df_high) {
  N <- nrow(df_high)
  
  if (!is.numeric(df_high$rain) || length(df_high$rain) != N) {
    stop("`df_high$rain` must be a numeric column of length nrow(df_high).")
  }
  if (!is.list(df_high$swr) || length(df_high$swr) != N) {
    stop("`df_high$swr` must be a list-column of SWR() objects, one per row.")
  }
  
  new_pred <- rep(NA_real_, N)
  highest_covered_idx <- 0L
  rain_vec <- as.numeric(df_high$rain)
  
  # Find the first usable end_idx (where start_idx ≥ 1)
  first_valid_end_idx <- NA_integer_
  
  for (i in seq_len(N)) {
    model <- df_high$swr[[i]]
    window_length <- ncol(coef(model))
    start_idx <- i - window_length + 1
    
    if (start_idx >= 1) {
      first_valid_end_idx <- i
      break
    }
  }
  
  if (is.na(first_valid_end_idx)) {
    warning("No usable models found with valid window lengths.")
    df_high$new_pred <- new_pred
    return(df_high)
  }
  
  # Only start from first usable end_idx
  for (end_idx in first_valid_end_idx:N) {
    model <- df_high$swr[[end_idx]]
    window_length <- ncol(coef(model))
    start_idx <- end_idx - window_length + 1
    
    if (start_idx < 1) {
      next  # Skip again, just in case
    }
    
    rain_slice <- rain_vec[start_idx:end_idx]
    preds_all <- predict(model, newdata = rain_slice)
    
    global_indices <- seq.int(start_idx, end_idx)
    is_new <- global_indices > highest_covered_idx
    
    new_indices <- global_indices[is_new]
    new_values <- preds_all[is_new]
    
    new_pred[new_indices] <- new_values
    highest_covered_idx <- max(highest_covered_idx, end_idx)
  }
  
  df_high$new_pred <- new_pred
  return(df_high)
}
