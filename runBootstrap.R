runBootstrap <- function(x, B = 10000, estimator = mean, CI = .95) {
  
  # Generates samples with replacement from the original sample
  # to form a bootstrap distribution of a point estimator
  #
  # Args:
  #   x: Vector of sample data
  #   B: Number of bootstrap samples to generate (default = 1000)
  #   estimator: Estimator of interest (default = mean)
  #   CI: Confidence interval
  #
  # Returns:
  #   - The resampled estimator of interest (mean, median, sd, var)
  #   - The standard error of the bootstrapped estimator of interest
  #   - User-supplied confidence interval for bootstrapped estimator of interest
  
  n <- length(x)
  boot.samples <- matrix(sample(x = x, size = n*B, replace = TRUE), B, n)
  boot.statistics <- apply(boot.samples, 1, estimator)
  se <- sqrt(var(boot.statistics))
  interval <- unname(quantile(boot.statistics, c((1-CI)/2, 1-((1-CI)/2))))
  return(list(boot.statistics = boot.statistics, se = se, interval = interval))   
}