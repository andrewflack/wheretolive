library(ggplot2)

source("runBootstrap.R")
source("estBetaParams.R")

time_horizon <- 5
sims <- 5000

source("nyear_avg_sal_andrew_dmv.R")
source("nyear_avg_sal_amber_dmv.R")

qplot(x = sal_AMR + sal_ACF) + geom_histogram()
qplot(x = bootsalnyr_AMR$boot.statistics + bootsalnyr_ACF$boot.statistics) + geom_histogram()