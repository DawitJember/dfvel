## code to prepare `DATASET` dataset goes here

sample_gdat<-read.csv("data-raw/sample_gdat.csv")
usethis::use_data(sample_gdat, overwrite = TRUE)
whoheadcv09<-read.csv("data-raw/whoheadcv09.csv")
usethis::use_data(whoheadcv09, overwrite = TRUE)
wholengthv09<-read.csv("data-raw/wholengthv09.csv")
usethis::use_data(wholengthv09, overwrite = TRUE)
whoweightv09<-read.csv("data-raw/whoweightv09.csv")
usethis::use_data(whoweightv09, overwrite = TRUE)
