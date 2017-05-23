# simulate amber's average salary over next n years if in DMV

salnyr_AMR <- NULL

yr1weds <- NULL
n_weddings <- matrix(rep(NA, sims*time_horizon), nrow=sims, ncol=time_horizon)
sal_AMR <- matrix(rep(NA, sims*time_horizon), nrow=sims, ncol=time_horizon)

# max number of weddings you would do in a year
max_per_year <- 30

# takehome for one wedding
mu_takehome <- 2500
sd_takehome <- 1000
location <- log(mu_takehome^2 / sqrt(sd_takehome^2 + mu_takehome^2))
shape <- sqrt(log(1 + (sd_takehome^2 / mu_takehome^2)))

# per wedding take home increase with experience
experience_factor <- seq(from = .9, to = 1.10, length.out = time_horizon)

for(i in 1:sims){
  
  yr1weds[i] <- round(rgamma(1, shape = 4, scale = 1)) # mean 4, sd 2
  ifelse(yr1weds[i] == 0, yr1weds[i] <- 1, yr1weds[i] <- yr1weds[i])
  
  n_weddings[i, 1] <- yr1weds[i]
  
  growth <- 2 + rnorm(time_horizon-1, .20, .02) # constant growth in n_weddings ~ 20% YoY
  
  for (j in 2:time_horizon) {n_weddings[i, j] = round(n_weddings[i, j-1]*growth[j-1])}
  
  over_max <- which(n_weddings[i,] > max_per_year)
  n_weddings[i, over_max] <- round(rnorm(length(over_max), max_per_year, .1*max_per_year))
  
  for (k in 1:time_horizon){
    sal_AMR[i,k] <- sum(rlnorm(n_weddings[i, k], location, shape))*experience_factor[k]
  }
  
  salnyr_AMR[i] <- mean(sal_AMR[i,])
  
}

qplot(x = salnyr_AMR) + geom_histogram()
summary(salnyr_AMR)

bootsalnyr_AMR <- runBootstrap(salnyr_AMR)
qplot(x = bootsalnyr_AMR$boot.statistics) + geom_histogram()

n_weddings %>% data.frame() %>% melt() %>% ggplot(aes(x = variable, y = value)) + geom_boxplot()
sal_AMR %>% data.frame() %>% melt() %>% ggplot(aes(x = variable, y = value)) + geom_boxplot()
