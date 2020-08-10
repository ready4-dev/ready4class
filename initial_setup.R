## FIX LIST
## A. Delete Collate List in Description File Each Time MAKE CLASSES IS CALLED
## B. Implement multiple inheritence (1-2 days work)
## C. Implement multiple signatures for methods.
## D. Add setOldClass call for each S3 child of tibble objects
## E. Rectify problems with display output from show method.
##
#usethis::create_package(getwd())
#usethis::use_data_raw()
devtools::load_all()
ready4fun::write_documented_fns(tmp_fn_dir_chr = "data-raw/fns",
                                R_dir_chr = "R")
source("data-raw/DATASET.R")
