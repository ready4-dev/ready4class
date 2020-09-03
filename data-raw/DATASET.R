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
ready4fun::write_to_reset_pkg_files("R") # Deletes contents of R directory and resets DESCRIPTION and NAMESPACE files.
# PAUSE FOR INTERACTIVE
ready4fun::write_pkg_setup_fls(#make_tmpl_vignette_1L_lgl = T, # UN-COMMENT THIS ARGUMENT THE FIRST TIME THIS SCRIPT IS RUN, THEN CHANGE BACK
                                 incr_ver_1L_lgl = F)
#
# 3. MANUAL STEP: WRITE FUNCTION & METHODS FILES
#
# 4. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4fun")
ready4fun::write_abbr_lup(short_name_chr = c("alg","cls","col","curr","gnrc","inhtc","inst","mk","mthd","ptrn",
                                             "ready4_class_make_tb","ready4_class_pt_lup",
                                             "tf","tfd","unvd","val","vld","vldd"),
                            long_name_chr = c("algorithm","class","column","current","generic","inheritance","instance","make","method","pattern",
                                                  "Class Make Table readyforwhatsnext S3 class",
                                                  "Class Prototype Lookup Table readyforwhatsnext S3 class",
                                              "transform","transformed","unvalidated","value","valid","validated"),
                            no_plural_chr = c("ready4_class_make_tb","ready4_class_pt_lup","transformed","unvalidated","valid","validated"),
                          custom_plural_ls = list(class = "classes"),
                            url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
# 6. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("fn_type_lup_tb")
## Get below working and add arrange.
fn_type_lup_tb %>%
ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = c("Create", "Delete",
                                              "Gen", # Needs to be complete word - update functions to reflect this.
                                              "Resolve","Set","Simplify","Validate"),
                           fn_type_desc_chr = c("Creates a new R object.",
                                                "Deletes a file from a specified location.",
                                                "Generates values for an object.",
                                                "Resolves inconsistencies between two or more objects.",
                                                "Sets the value of an object.",
                                                "Simplifies and object.",
                                                "Validates an object.")) %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = c("Add Class","Make and Update",
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
  ready4fun::write_dmtd_fn_type_lup(url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                       abbreviations_lup = abbreviations_lup)
data("fn_type_lup_tb")
# 7. Create a table of all functions to document
all_fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(custom_dmt_ls = list(details_ls = NULL,
                                                                       inc_for_main_user_lgl_ls = list(force_true_chr = c("make_and_update"),
                                                                            force_false_chr = NA_character_),
                                                           args_ls_ls = NULL),
                                      fn_type_lup_tb = fn_type_lup_tb,
                                      abbreviations_lup = abbreviations_lup)

## 8. Write and document.
# 8.1 Write documented methods to R directory.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls(all_fns_dmt_tb,
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
# ENSURE that ready4fun::write_pkg_setup_fls function argument make_tmpl_vignette_lgl is not set to TRUE earlier in this script.
#
# 13. Update documentation

ready4fun::write_ns_imps_to_desc()
#
# 14. Manual step: Push changes



