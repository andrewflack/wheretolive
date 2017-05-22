# simulate amber's average salary over next n years if in DMV

sal_AMR <- NULL

yr1weds <- NULL
n_weddings <- matrix(rep(NA, sims*time_horizon), nrow=sims, ncol=time_horizon)
per_wedding_takehome <- matrix(rep(NA, sims*time_horizon), nrow=sims, ncol=time_horizon)

# per_wedding_takehome for wedding
mu_takehome <- 2500
sd_takehome <- 1000
location <- log(mu_takehome^2 / sqrt(sd_takehome^2 + mu_takehome^2))
shape <- sqrt(log(1 + (sd_takehome^2 / mu_takehome^2)))

for(i in 1:sims){
  
  yr1weds[i] <- round(rgamma(1, shape = 4, scale = 1)) # mean 4, sd 2
  # print(yr1weds)
  
  n_weddings[i, 1] <- yr1weds[i]
  
  growth <- 1 + rnorm(time_horizon-1, .20, .02) # constant growth in n_weddings ~ 20% YoY
  # print(growth)
  
  for (j in 2:time_horizon) {n_weddings[i, j] = round(n_weddings[i, j-1]*growth[j-1])}
  # print(n_weddings)
  
  for (k in 1:time_horizon){
    per_wedding_takehome[i,k] <- sum(rlnorm(n_weddings[i, k], location, shape))
  }
  # print(per_wedding_takehome)
  
  sal_AMR[i] <- mean(per_wedding_takehome[i,])
  # print(sal)
}

qplot(x = sal_AMR) + geom_histogram()
summary(sal_AMR)
# qplot(x = seq_along(colMeans(per_wedding_takehome)), y = colMeans(per_wedding_takehome), geom = "line")

bootsalnyr_AMR <- runBootstrap(sal_AMR)
qplot(x = bootsalnyr_AMR$boot.statistics) + geom_histogram()
