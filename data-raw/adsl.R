## code to prepare `adsl` dataset goes here

demo <- 'inst/extdata/demo ADaM.Rdata'
load(demo)

usethis::use_data(adsl, overwrite = TRUE)
