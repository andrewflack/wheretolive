library(ggplot2)
library(dplyr)
library(ggmap)

cary <- read.csv("cary.csv", stringsAsFactors = FALSE) 
cary_median <- median(cary$PRICE)

dmv <- read.csv("dmv.csv", stringsAsFactors = FALSE)
dmv <- dmv %>% mutate(from = paste(ADDRESS, CITY, STATE, ZIP, sep = " "))

drive_times <- bind_rows(apply(subset(dmv, select = c("from")), 1, function(x) mapdist(from = x[1], to = "4850 Mark Center Dr Alexandria VA", mode = "driving")))

dmv <- left_join(dmv, drive_times[,c("from", "minutes")])

p <- get_map("Washington DC")

ggmap(p) + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE, colour = minutes), data = dmv[dmv$PRICE <= cary_median,], size = 3) + 
  scale_colour_gradient(low = "green", high = "red")

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

out <- runBootstrap(dmv[which(!is.na(dmv$minutes) & dmv$PRICE <= cary_median), "minutes"])
qplot(dmv[which(!is.na(dmv$minutes) & dmv$PRICE <= cary_median), "minutes"])
qplot(out$boot.statistics)
