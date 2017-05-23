library(ggplot2)
library(reshape2)
library(dplyr)

source("runBootstrap.R")
source("estBetaParams.R")

time_horizon <- 5
sims <- 5000

source("nyear_avg_sal_andrew_dmv.R")
source("nyear_avg_sal_amber_dmv.R")

qplot(x = salnyr_AMR + salnyr_ACF) + geom_histogram()
qplot(x = bootsalnyr_AMR$boot.statistics + bootsalnyr_ACF$boot.statistics) + geom_histogram()

m <- sal_ACF + sal_AMR 
m %>% data.frame() %>% melt() %>% ggplot(aes(x = variable, y = value)) + geom_boxplot()