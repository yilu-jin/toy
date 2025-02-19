## code to prepare `inst/extdata/demo ADaM.Rdata` dataset goes here
demo <- "inst/extdata/demo ADaM.Rdata"
load(demo)
demo_adsl <- adsl
demo_adae <- adae

usethis::use_data(demo_adsl, overwrite = TRUE)
usethis::use_data(demo_adae, overwrite = TRUE)
