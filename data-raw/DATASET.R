##
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Specify package name
pkg_nm_chr <- "ready4class"
#
# 3. Create "fns", "gnrcs" and "mthds" sub-directories.
write_fn_type_dirs <- function(path_1L_chr = "data-raw"){
  undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr)
  paths_ls <- undocumented_fns_dir_chr %>% purrr::walk(~{
    if(!dir.exists(.x))
      dir.create(.x)
  })
}
make_undmtd_fns_dir_chr <- function(path_1L_chr = "data-raw"){
  undocumented_fns_dir_chr <- paste0(path_1L_chr,"/",make_fns_type_chr())
  return(undocumented_fns_dir_chr)
}
make_fns_type_chr <- function(){
  fns_type_chr <- c("fns","gnrcs","mthds")
  return(fns_type_chr)
}
make_fns_chr_ls <- function(path_1L_chr = "data-raw"){
  fns_chr_ls <- make_undmtd_fns_dir_chr(path_1L_chr) %>%
    purrr::map(~ready4fun::read_fns(.x)) %>%
    stats::setNames(make_fns_type_chr())
  fns_chr_ls <- fns_chr_ls %>% purrr::discard(~ identical(.x,character(0)))
  return(fns_chr_ls)
}
write_fn_type_dirs(path_1L_chr)
paths_ls <- make_fns_chr_ls()
#
ready4fun::read_fns("data-raw/test")

# 4. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4fun")
ready4fun::make_abbr_lup_tb(short_name_chr_vec = c("col","inst", "ready4_class_make_tb","ready4_class_pt_lup"),
                            long_name_chr_vec = c("column","instance",
                                                  "Class Make Table readyforwhatsnext S3 class",
                                                  "Class Prototype Lookup Table readyforwhatsnext S3 class"),
                            no_plural_chr_vec = c("ready4_class_make_tb","ready4_class_pt_lup"),
                            url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            pkg_nm_chr = pkg_nm_chr,
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
# 5. Create function types and generics look-up tables
# 5.1 Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
ready4fun::make_fn_type_lup_tb() %>%
  dplyr::bind_rows(tibble::tibble(fn_type_nm_chr = c("Create", "Delete",
                                                     "Gen", # Needs to be complete word - update functions to reflect this.
                                                     "Resolve","Set","Simplify","Validate"),
                                  fn_type_desc_chr = c("Creates a new R object.",
                                                       "Deletes a file from a specified location.",
                                                       "Generates values for an object.",
                                                       "Resolves inconsistencies between two or more objects.",
                                                       "Sets the value of an object.",
                                                       "Simplifies and object.",
                                                       "Validates an object."),
                                  first_arg_desc_chr = NA_character_,
                                  second_arg_desc_chr = NA_character_,
                                  is_generic_lgl = F,
                                  is_method_lgl = F)) %>% # Add to ready4fun template.
  dplyr::bind_rows(tibble::tibble(fn_type_nm_chr = c("Add Class","Make and Update",
                                                     "Make Classes", # Should be "write" titled
                                                     "Make Lookup Table","Order Tibble","Remake List Columns","Update Lookup Table for Namespace"),
                                  fn_type_desc_chr = c("Adds information about a class.",
                                                       "Applies a Make method and then updates the output of that method.",
                                                       "Writes new classes.", # Should be "write" titled
                                                       "Makes a lookup table.",
                                                       "Orders a tibble.",
                                                       "Remakes list columns.",
                                                       "Updates a lookup table with namespace data."),
                                  first_arg_desc_chr = NA_character_,
                                  second_arg_desc_chr = NA_character_,
                                  is_generic_lgl = T,
                                  is_method_lgl = T)) %>%
  dplyr::arrange(fn_type_nm_chr) %>%
  ready4fun::make_and_doc_fn_type_R(pkg_nm_chr = pkg_nm_chr,
                       url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                       abbreviations_lup = abbreviations_lup)
data("fn_type_lup_tb")
# 6. Create a table of all functions to document
all_fns_dmt_tb <- ready4fun::make_all_fns_dmt_tb(paths_ls = paths_ls,
                                      undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(), # MAKE FN DEFAULT
                                      custom_dmt_ls = list(details_ls = NULL,
                                                           export_ls = list(force_true_chr_vec = c("make_and_update"),
                                                                            force_false_chr_vec = NA_character_),
                                                           args_ls_ls = NULL),
                                      fn_type_lup_tb = fn_type_lup_tb,
                                      abbreviations_lup = abbreviations_lup)

## 7 Document.
# Write documented methods to R directory.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls_R(all_fns_dmt_tb,
                                  r_dir_chr = "R")
#
# Update Description file with imported packages.
ready4fun::write_ns_imps_to_desc()
##
## 8. Run script to make package classes.
source("data-raw/MAKE_CLASSES.R")
prototype_lup <- prototype_lup %>%
  ready4_class_pt_lup()
usethis::use_data(prototype_lup,overwrite = T, internal = T)
## 9. Remake the classes we previously created, this time using the new, preferred make_and_update method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
prototype_lup <- make_and_update(classes_to_make_tb,
                                 dev_pckg_namespace = dev_pckg_namespace,
                                 name_prefix = name_prefix,
                                 output_dir = "R",
                                 file_exists_logic = "overwrite")
## 10. Update the internal system data.
usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
# 11. Create vignettes
usethis::use_vignette("ready4class")
devtools::document()
ready4fun::write_ns_imps_to_desc()




