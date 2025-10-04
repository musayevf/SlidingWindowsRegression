# packages.R

# ---- CRAN packages ----
cran_packages <- c("dplyr", "ggplot2", "lubridate")

# Install missing CRAN packages
installed <- cran_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(cran_packages[!installed])
}

# ---- GitHub package: SlidingWindowReg ----
if (!requireNamespace("SlidingWindowReg", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("sschrunner/SlidingWindowReg", 
                          build_manual = TRUE, 
                          build_vignettes = TRUE)
}

# ---- Load everything ----
invisible(lapply(c(cran_packages, "SlidingWindowReg"), library, character.only = TRUE))
