# blind rating of exterior
library(stringr)

cary <- read.csv("data/cary.csv", stringsAsFactors = FALSE)
dmv <- read.csv("data/dmv.csv", stringsAsFactors = FALSE)
lees <- read.csv("data/leesburg.csv", stringsAsFactors = FALSE)




samp_size <- 30

cary_samp <- sample_n(cary, samp_size, replace = FALSE) %>% 
  select(MLS.) %>% 
  mutate(first_pic = paste0("https://ssl.cdn-redfin.com/photo/102/bigphoto/", str_sub(.$MLS,-3,-1), "/", .$MLS, "_1.jpg"))

dmv_samp <- sample_n(dmv, samp_size, replace = FALSE) %>% 
  select(MLS.) %>% 
  mutate(first_pic = paste0("https://ssl.cdn-redfin.com/photo/102/bigphoto/", str_sub(.$MLS,-3,-1), "/", .$MLS, "_1.jpg"))

lees_samp <- sample_n(lees, samp_size, replace = FALSE) %>% 
  select(MLS.) %>% 
  mutate(first_pic = paste0("https://ssl.cdn-redfin.com/photo/102/bigphoto/", str_sub(.$MLS,-3,-1), "/", .$MLS, "_1.jpg"))

write.csv(cary_samp, "tmp.csv", row.names = FALSE)
