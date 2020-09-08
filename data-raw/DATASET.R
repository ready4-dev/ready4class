#
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Create "fns", "gnrcs" and "mthds" sub-directories.
ready4fun::write_fn_type_dirs()
#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Set-up package structure
options(usethis.description = list(
  Package = ready4fun::get_dev_pkg_nm(),
  Title =  "Readyforwhatsnext Tools For Creating And Extending Classes.",
  Description = "ready4class provides classes and methods that are designed to standardise and streamline the process for extending the readyforwhatsnext data synthesis and simulation framework with new classes.",
  `Authors@R` = c(utils::person(
    given = "Matthew",family = "Hamilton", email =
      "matthew.hamilton@orygen.org.au",role = c("aut",
                                                "cre"),comment = c(ORCID = "0000-0001-7407-9194")
  ),
  utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
                role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
  #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
  #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
  #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
  #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
  utils::person("Orygen", role = c("cph", "fnd")),
  utils::person("VicHealth",role = c("fnd")),
  utils::person("Victoria University", role =c("fnd"))
  ),
  License = usethis::use_gpl3_license()
))
# Deletes contents of R directory and resets DESCRIPTION and NAMESPACE files.
ready4fun::write_pkg_setup_fls(#make_tmpl_vignette_lgl = T, First time script is run this should be un-commented then switched off again.
  incr_ver_1L_lgl = F,
  delete_contents_of_R_dir = T)
# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP: WRITE FUNCTION & METHODS FILES
#
# 6. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4fun")
ready4fun::write_abbr_lup(short_name_chr = c("alg","cdn","chkr","cls","col","curr","dif","gen","gnrc","inc","inhtc","inst","lnt","mk","mthd","ptrn",
                                             "ready4_constructor_tbl","ready4_class_pt_lup",
                                             "ref","tf","tfd","unvd","val","vld","vldd"),
                            long_name_chr = c("algorithm","condition","checker","class","column","current","different","generate","generic","include","inheritance","instance","length","make","method","pattern",
                                                  "CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class",
                                                  "CLASS PROTOTYPE LOOKUP TABLE readyforwhatsnext S3 class",
                                              "reference","transform","transformed","unvalidated","value","valid","validated"),
                            no_plural_chr = c("Class Constructor Table readyforwhatsnext S3 class",
                                              "Class Prototype Lookup Table readyforwhatsnext S3 class",
                                              "transformed","unvalidated","valid","validated"),
                          custom_plural_ls = list(class = "classes"),
                            url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
# 7. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("fn_type_lup_tb",package = "ready4fun")
## Get below working and add arrange.
fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = c("Add Class",
                                                "Make Lookup Table",
                                                "Order Tibble",
                                                "Remake List Columns",
                                                "Update Lookup Table for Namespace",
                                                "Write Classes",
                                                "Write Classes and Make Lookup Table"
                                                ),
                             fn_type_desc_chr = c("Adds information about a class.",
                                                  "Applies a Make method and then updates the output of that method.",
                                                  "Orders a tibble.",
                                                  "Remakes list columns.",
                                                  "Updates a lookup table with namespace data.",
                                                  "Writes new classes.",
                                                  "Makes new classes and creates or updates a class prototype lookup table."),
                             is_generic_lgl = T,
                             is_method_lgl = T) %>%
  ready4fun::write_dmtd_fn_type_lup(url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                       abbreviations_lup = abbreviations_lup)
data("fn_type_lup_tb")
# 8. Create a table of all functions to document
all_fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(custom_dmt_ls = list(details_ls = NULL,
                                                                       inc_for_main_user_lgl_ls = list(force_true_chr = c("remake_ls_cols",
                                                                                                                          "remake_ls_cols.ready4_constructor_tbl",
                                                                                                                          "write_classes_and_make_lup",
                                                                                                                          "write_classes_and_make_lup.ready4_constructor_tbl"),
                                                                            force_false_chr = NA_character_),
                                                           args_ls_ls = NULL),
                                      fn_type_lup_tb = fn_type_lup_tb,
                                      abbreviations_lup = abbreviations_lup)

## 9. Write and document.
## Note files to be rewritten cannot be open in RStudio.
# ready4fun::write_and_doc_fn_fls(all_fns_dmt_tb,
#                      r_dir_1L_chr = "R",
#                      dev_pkgs_chr = "ready4fun")
##
## 10. Run script to make package classes.
source("data-raw/WRITE_CLASSES.R")
prototype_lup <- prototype_lup %>%
  ready4_class_pt_lup()
usethis::use_data(prototype_lup,overwrite = T, internal = T)
## 11. Remake the classes we previously created, this time using the new, preferred write_classes_and_make_lup method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
prototype_lup <- write_classes_and_make_lup(classes_to_make_tb,
                                 dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                 name_pfx_1L_chr = name_pfx_1L_chr,
                                 output_dir_1L_chr = "R",
                                 file_exists_cdn_1L_chr = "overwrite",
                                 abbreviations_lup = abbreviations_lup)
## 12. Update the internal system data.
usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
# 13. MANUAL STEP - WRITE vignettes
# ENSURE that ready4fun::write_pkg_setup_fls function argument make_tmpl_vignette_lgl is not set to TRUE earlier in this script.
#
# 14. Update documentation
ready4fun::write_and_doc_fn_fls(all_fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = "ready4fun")
#ready4fun::write_ns_imps_to_desc()
#
# 15. Manual step: Push changes



