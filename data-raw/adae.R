## code to prepare `adae` dataset goes here

demo <- 'inst/extdata/demo ADaM.Rdata'
load(demo)

usethis::use_data(adae, overwrite = TRUE)
