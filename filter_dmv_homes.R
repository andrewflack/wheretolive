# filter dmv dataset to only "close" properties

library(ggmap)
library(dplyr)

max_drive_time <- 20

dmv <- read.csv("data/dmv.csv", stringsAsFactors = FALSE)
dmv <- dmv %>% mutate(from = paste(ADDRESS, CITY, STATE, ZIP, sep = " "))

drive_times <- bind_rows(apply(subset(dmv, select = c("from")), 1, function(x) mapdist(from = x[1], to = "4850 Mark Center Dr Alexandria VA", mode = "driving")))

dmv_close <- drive_times %>% filter(minutes <= max_drive_time) %>% inner_join(dmv, drive_times[,c("from", "minutes")])