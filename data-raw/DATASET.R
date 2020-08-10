##
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Specify package name
pkg_nm_chr <- "ready4class"
#
# 3. Create a "fns" sub-directory.
fns_dir_chr <-"data-raw/fns"
if(!dir.exists(fns_dir_chr))
  dir.create(fns_dir_chr)
## MAKE THIS A FUNCTION
#
# 4. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 5. Read all undocumented functions in the temporary "fns" directory.
fns_path_chr_vec <- ready4fun::read_fns(fns_dir_chr) # Can be moved into make_fn_dmt_tbl_tb function.
#
# 6. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
ready4fun::make_abbr_lup_tb(url_chr = NA_character_,
                            pkg_nm_chr = pkg_nm_chr)

# 7. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
ready4fun::make_fn_type_lup_tb() %>%
  dplyr::bind_rows(tibble::tibble(fn_type_nm_chr = c("Create", "Delete", "Gen", "Resolve","Set","Simplify","Validate"),
                                  fn_type_desc_chr = c("Creates a new R object.",
                                                       "Deletes a file from a specified location.",
                                                       "Generates values for an object.",
                                                       "Resolves inconsistencies between two or more objects.",
                                                       "Sets the value of an object.",
                                                       "Simplifies and object.",
                                                       "Validates an object."),
                                  first_arg_desc_chr = NA_character_,
                                  second_arg_desc_chr = NA_character_)) %>%
  dplyr::arrange(fn_type_nm_chr) %>%
ready4fun::make_and_doc_fn_type_R(pkg_nm_chr = pkg_nm_chr,
                       url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/")
data("fn_type_lup_tb")
#
# 8. Create a table of all undocumented functions
fns_dmt_tb <- ready4fun::make_fn_dmt_tbl_tb(fns_path_chr_vec,
                                 fns_dir_chr = fns_dir_chr,
                                 custom_dmt_ls = list(details_ls = NULL,
                                                      export_ls = list(force_true_chr_vec = c("make_and_update"),
                                                                       force_false_chr_vec = NA_character_),
                                                      args_ls_ls = NULL),
                                 append_lgl = T,
                                 fn_type_lup_tb = fn_type_lup_tb)
# NOTE: To update, make call to update_fns_dmt_tb_tb
#
# 9. Write documented functions to R directory.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls_R(fns_dmt_tb,
                       r_dir_chr = "R",
                       make_pdfs_lgl = F)
#
# 10. Update Description file with imported packages.
ready4fun::write_ns_imps_to_desc()
#
##
## 11. Pre-requisites
## In addition to the pre-requisites specified in the MAKE_CLASSES_1.R and MAKE_CLASSES_2.R files in the data-raw directory, the following files are required to be in the package's R folder:
## mthd_add_class.R
## mthd_make_and_update.R
## mthd_make_lup.R
## mthd_order_tb.R
## mthd_update_for_ns.R
##
##
## 12. Run script to make package classes.
source("data-raw/MAKE_CLASSES.R")
prototype_lup <- prototype_lup %>%
  ready4_class_pt_lup()
usethis::use_data(prototype_lup,overwrite = T, internal = T)
## 31. Remake the classes we previously created, this time using the new, preferred make_and_update method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
prototype_lup <- make_and_update(classes_to_make_tb,
                                 dev_pckg_namespace = dev_pckg_namespace,
                                 name_prefix = name_prefix,
                                 output_dir = "R",
                                 file_exists_logic = "overwrite")
## 14. Update the internal system data.
usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
## 15. Manual Step (Currently): Add methods scripts to R directory.
##
## 16. Write and document methods.
# REPEAT OF PREVIOUS STEP. Need to streamline to one step.
# 16.1 Create a "fns" sub-directory.
mthds_dir_chr <-"data-raw/mthds"
if(!dir.exists(mthds_dir_chr))
  dir.create(mthds_dir_chr)
# 16.2. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 16.3 Read all undocumented functions in the temporary "fns" directory.
mthds_path_chr_vec <- ready4fun::read_fns(mthds_dir_chr) # Can be moved into make_fn_dmt_tbl_tb function.
#
# Skip repeat of abbreviations_lup creation
# Ammend fn_type_lup_tb step as this step is now about methods rather than function type.
# 16.4 Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
# fn_type_lup_tb %>%
#   dplyr::bind_rows(
#     tibble::tibble(fn_type_nm_chr = c("Order", "Remake"),
#                                   fn_type_desc_chr = c("Orders and object using a specified algorithm.",
#                                                        "Restores and object to a prior format."),
#                                   first_arg_desc_chr = NA_character_,
#                                   second_arg_desc_chr = NA_character_
#                    #)
#     ) %>%
#   dplyr::arrange(fn_type_nm_chr) %>%
#   ready4fun::make_and_doc_fn_type_R(pkg_nm_chr = pkg_nm_chr,
#                                     url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/")
# data("fn_type_lup_tb")
#
# 16.5. Create a table of all undocumented functions
## PICK UP HERE.
fns_dmt_tb <- ready4fun::make_fn_dmt_tbl_tb(mthds_path_chr_vec,
                                            fns_dir_chr = mthds_dir_chr,
                                            custom_dmt_ls = list(details_ls = NULL,
                                                                 export_ls = list(force_true_chr_vec = c("make_and_update"),
                                                                                  force_false_chr_vec = NA_character_),
                                                                 args_ls_ls = NULL),
                                            append_lgl = T,
                                            fn_type_lup_tb = fn_type_lup_tb)
# NOTE: To update, make call to update_fns_dmt_tb_tb
#
## 16.6 Document.
# Write documented functions to R directory.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls_R(fns_dmt_tb,
                                  r_dir_chr = "R")
#
# Update Description file with imported packages.
ready4fun::write_ns_imps_to_desc()
# 17. Create vignettes
usethis::use_vignette("ready4class")
devtools::document()




