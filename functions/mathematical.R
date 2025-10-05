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
