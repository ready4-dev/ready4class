##
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Write standardised ready4 package set up.
ready4fun::write_pkg_setup_fls_R(make_tmpl_vignette_lgl = T, # CHANGE TO FALSE
                                 incr_ver_lgl = F)
#
# 3. MANUAL STEP: WRITE FUNCTION & METHODS FILES
#
# 4. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4fun")
ready4fun::make_abbr_lup_tb(short_name_chr_vec = c("col","inst", "ready4_class_make_tb","ready4_class_pt_lup"),
                            long_name_chr_vec = c("column","instance",
                                                  "Class Make Table readyforwhatsnext S3 class",
                                                  "Class Prototype Lookup Table readyforwhatsnext S3 class"),
                            no_plural_chr_vec = c("ready4_class_make_tb","ready4_class_pt_lup"),
                            url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            #pkg_nm_chr = pkg_nm_chr,
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
# 6. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("fn_type_lup_tb")
## Get below working and add arrange.
fn_type_lup_tb %>%
ready4fun::add_rows_to_fn_type_lup_tb(fn_type_nm_chr = c("Create", "Delete",
                                              "Gen", # Needs to be complete word - update functions to reflect this.
                                              "Resolve","Set","Simplify","Validate"),
                           fn_type_desc_chr = c("Creates a new R object.",
                                                "Deletes a file from a specified location.",
                                                "Generates values for an object.",
                                                "Resolves inconsistencies between two or more objects.",
                                                "Sets the value of an object.",
                                                "Simplifies and object.",
                                                "Validates an object.")) %>%
  ready4fun::add_rows_to_fn_type_lup_tb(fn_type_nm_chr = c("Add Class","Make and Update",
                                                "Make Classes", # Should be "write" titled
                                                "Make Lookup Table","Order Tibble","Remake List Columns","Update Lookup Table for Namespace"),
                             fn_type_desc_chr = c("Adds information about a class.",
                                                  "Applies a Make method and then updates the output of that method.",
                                                  "Writes new classes.", # Should be "write" titled
                                                  "Makes a lookup table.",
                                                  "Orders a tibble.",
                                                  "Remakes list columns.",
                                                  "Updates a lookup table with namespace data."),
                             is_generic_lgl = T,
                             is_method_lgl = T) %>%
  ready4fun::make_and_doc_fn_type_R(#pkg_nm_chr = pkg_nm_chr,
                       url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                       abbreviations_lup = abbreviations_lup)
data("fn_type_lup_tb")
# 7. Create a table of all functions to document
all_fns_dmt_tb <- ready4fun::make_all_fns_dmt_tb(custom_dmt_ls = list(details_ls = NULL,
                                                           export_ls = list(force_true_chr_vec = c("make_and_update"),
                                                                            force_false_chr_vec = NA_character_),
                                                           args_ls_ls = NULL),
                                      fn_type_lup_tb = fn_type_lup_tb,
                                      abbreviations_lup = abbreviations_lup)

## 8. Write and document.
# 8.1 Write documented methods to R directory.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls_R(all_fns_dmt_tb,
                                  r_dir_chr = "R")
#
# 8.2 Update Description file with imported packages.
ready4fun::write_ns_imps_to_desc(incr_ver_lgl = F)
##
## 9. Run script to make package classes.
source("data-raw/MAKE_CLASSES.R")
prototype_lup <- prototype_lup %>%
  ready4_class_pt_lup()
usethis::use_data(prototype_lup,overwrite = T, internal = T)
## 10. Remake the classes we previously created, this time using the new, preferred make_and_update method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
prototype_lup <- make_and_update(classes_to_make_tb,
                                 dev_pckg_namespace = dev_pckg_namespace,
                                 name_prefix = name_prefix,
                                 output_dir = "R",
                                 file_exists_logic = "overwrite")
## 11. Update the internal system data.
usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
# 12. MANUAL STEP - WRITE vignettes
# ENSURE that ready4fun::write_pkg_setup_fls_R function argument make_tmpl_vignette_lgl is not set to TRUE earlier in this script.
#
# 13. Update documentation

ready4fun::write_ns_imps_to_desc()
#
# Push changes



