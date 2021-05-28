library(mixpanel)

dir <- "mp"
raw_data <- get_mixpanel_data(event = "Browse page")  
md <- parse_mixpanel_data(raw_data, dir)
unlink(dir, TRUE, TRUE)

sample_data <- md

transformation_map <- mapusage::build_transformation_map() %>% 
  mapusage:::construct_map("transformation")

usethis::use_data(sample_data, transformation_map, overwrite = TRUE)
