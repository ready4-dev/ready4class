#
# Prior to running this script, the gh-pages orphan branch should be set up using instructions at:
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
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
ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Developer Tools For Creating And Extending Classes For Use As Part of The Ready4 Suite",
                 pkg_desc_1L_chr = "ready4class provides classes and methods that are designed to standardise and streamline the process for extending the readyforwhatsnext data synthesis and simulation framework with new classes.
  This development version of the ready4class package has been made available as part of the process of testing and documenting the package. That means this should be regarded as UNTESTED software, which is provided for free WITHOUT ANY WARRANTY. Importantly, the tools contained in this test release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  While we welcome and appreciate anyone who takes the time to provide us with feedback on this test release, we caution you that you should only test this software if you feel confident you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                 authors_prsns = c(utils::person(
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
                 urls_chr = c("https://ready4-dev.github.io/ready4class/",
                              "https://github.com/ready4-dev/ready4class",
                              "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
                      delete_contents_of_R_dir = T,
                      copyright_holders_chr = "Orygen",
                      check_type_1L_chr = "gh",
                      path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4class-logo/default.png",
                      github_repo = "ready4-dev/ready4class",
                      lifecycle_stage_1L_chr = "experimental",
                      badges_lup = ready4fun::badges_lup,
                      addl_badges_ls = list(ready4 = "development"))
# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP: WRITE FUNCTION & METHODS FILES
#
# 6. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4fun")
ready4fun::write_abbr_lup(short_name_chr = c("alg","cdn","chkr","cls","col","curr","dif","gen","gnrc","idx","inc","inhtc","inst","lnt","mk","mthd","ptrn",
                                             "ready4_constructor_tbl","ready4_class_pt_lup",
                                             "ref","ret","tf","tfd","unvd","val","vld","vldd"),
                            long_name_chr = c("algorithm","condition","checker","class","column","current","different","generate","generic","index","include","inheritance","instance","length","make","method","pattern",
                                                  "readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE",
                                                  "readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE",
                                              "reference","return","transform","transformed","unvalidated","value","valid","validated"),
                            no_plural_chr = c("Class Constructor Table readyforwhatsnext S3 class",
                                              "Class Prototype Lookup Table readyforwhatsnext S3 class",
                                              "transformed","unvalidated","valid","validated"),
                          custom_plural_ls = list(class = "classes", index = "indices"),
                            url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
# 7. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("fn_type_lup_tb",package = "ready4fun")
## Get below working and add arrange.
fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
                                                                                  fn_type_lup_tb = fn_type_lup_tb),
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
usethis::use_data(prototype_lup, overwrite = T, internal = T)
## 11. Remake the classes we previously created, this time using the new, preferred write_classes_and_make_lup method, which appends the metadata on the new classes to our instance of the ready4_class_pt_lup class.
prototype_lup <- write_classes_and_make_lup(classes_to_make_tb,
                                 dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                 name_pfx_1L_chr = name_pfx_1L_chr,
                                 output_dir_1L_chr = "R",
                                 file_exists_cdn_1L_chr = "overwrite",
                                 abbreviations_lup = abbreviations_lup)
## 12. Update the internal system data.
ready4fun::write_and_doc_ds(prototype_lup,
                            db_1L_chr = "prototype_lup",
                            title_1L_chr = "Class prototype lookup table",
                            desc_1L_chr = "Metadata on classes used in readyforwhatsnext suite")
#
#usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
# 13. MANUAL STEP - WRITE vignettes
# ENSURE that ready4fun::write_pkg_setup_fls function argument make_tmpl_vignette_lgl is not set to TRUE earlier in this script.
#
# 14. Update documentation
ready4fun::write_and_doc_fn_fls(fns_dmt_tb = all_fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = "ready4fun",
                                update_pkgdown_1L_lgl = T)
ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://ready4-dev.github.io/ready4/pdfs/ready4class_0.0.0.9138.pdf",
                          developer_manual_url_1L_chr = "https://ready4-dev.github.io/ready4/pdfs/ready4class_0.0.0.9138_dev.pdf")

#pkgdown::build_site()
# 15. Manual step: Push changes
## NOTE TO SELF: Need to implement variant of local git step outlined here: https://pkgdown.r-lib.org/reference/deploy_site_github.html



