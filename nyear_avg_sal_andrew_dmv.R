# simulate andrew's average salary over next n years if in DMV

salnyr_ACF <- NULL

sal_ACF <- matrix(rep(NA, sims*time_horizon), nrow=sims, ncol=time_horizon)
sal_ACF[, 1] <- # [your input here]

raise_mu <- .03
raise_sd <- .02
raise_params <- estBetaParams(raise_mu, raise_sd^2)

for (i in 1:sims){
  
  raises <- 1 + rbeta(time_horizon-1, raise_params$alpha, raise_params$beta)
  for (j in 2:time_horizon) {sal_ACF[i, j] = sal_ACF[i, j-1]*raises[j-1]}
  
  salnyr_ACF[i] <- mean(sal_ACF[i,])
}

qplot(x = salnyr_ACF) + geom_histogram()
sal_ACF %>% data.frame() %>% melt() %>% ggplot(aes(x = variable, y = value)) + geom_boxplot()

bootsalnyr_ACF <- runBootstrap(salnyr_ACF)
qplot(x = bootsalnyr_ACF$boot.statistics) + geom_histogram()
