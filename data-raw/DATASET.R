library(ready4fun)
# One off tidy-up of abbreviations lup and prototype_lup
# abbreviations_lup <- get_rds_from_dv("abbreviations_lup",
#                                     dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9")
# abbreviations_lup <- abbreviations_lup %>%
#   dplyr::filter(short_name_chr %>% purrr::map_lgl(~!(startsWith(.x,"ready4_")|(.x %in% c("unvd", "..._ls", "..._r4",
#                                                                                          "ready4class_class_pt_lup")))))
# write_env_objs_to_dv(list(abbreviations_lup = abbreviations_lup),
#                      descriptions_chr = "Abbreviations lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
# prototype_lup <- get_rds_from_dv("prototype_lup")
# prototype_lup <- prototype_lup %>%
#   dplyr::filter(type_chr %>% purrr::map_lgl(~!startsWith(.x,"ready4_")))
# class(prototype_lup) <- class(prototype_lup)[3:5]
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
ready4fun::write_fn_type_dirs()
pkg_desc_ls <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Developer Tools For Creating And Extending Classes For Use As Part of The Ready4 Suite",
                            pkg_desc_1L_chr = "ready4class provides classes and methods that are designed to standardise and streamline the process for extending the ready4 data synthesis and simulation framework with new classes.
  This development version of the ready4class package has been made available as part of the process of testing and documenting the package. The tools contained in this test release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  Therefore, you should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                            authors_prsn = c(utils::person(
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
                                         "https://www.ready4-dev.com/"))
classes_to_make_tb <- tibble::tribble(
  ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
  TRUE, "constructor_tbl", list("tibble"), list("is_"), list("tibble"), list(make_s3_lgl = "logical(0)",
                                                                             name_stub_chr = "character(0)",
                                                                             pt_ls = "list()",
                                                                             pt_chkr_pfx_ls = "list()",
                                                                             pt_ns_ls = "list()",
                                                                             vals_ls = "list()",
                                                                             allowed_vals_ls = "list()",
                                                                             min_max_vals_ls = "list()",
                                                                             start_end_vals_ls = "list()",
                                                                             class_desc_chr = "character(0)",
                                                                             parent_class_chr = "character(0)",
                                                                             slots_ls = "list()",
                                                                             meaningful_nms_ls = "list()",
                                                                             inc_clss_ls = "list()",
                                                                             asserts_ls = "list()"), NULL, NULL, NULL, "ready4 S3 class Constructor Table of metadata required to make new classes.", NA_character_, NULL, NULL, NULL, NULL,
  TRUE, "pt_lup", list("tibble"), list("is_"), list("tibble"), list(type_chr = "character(0)",
                                                                          val_chr = "character(0)",
                                                                          pt_ns_chr = "character(0)",
                                                                          fn_to_call_chr = "character(0)",
                                                                          default_val_chr = "character(0)",
                                                                          old_class_lgl = "logical(0)"), NULL, NULL, NULL, "ready4 S3 class Prototype Lookup Table of class metadata.", NA_character_, NULL, NULL, NULL, NULL)
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
pkg_setup_ls <- pkg_desc_ls %>%
  ready4fun::make_pkg_setup_ls(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = "rmarkdown"),
                               build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
                               check_type_1L_chr = "standard",
                               cls_fn_ls = list(fn = fns_env_ls$fns_env$write_classes_and_make_lup.ready4class_constructor_tbl,# Add fn
                                                args_ls = list(x = classes_to_make_tb)),
                               copyright_holders_chr = "Orygen",
                               dev_pkgs_chr = "ready4fun",
                               lifecycle_stage_1L_chr = "experimental",
                               path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4class-logo/default.png",
                               pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                      "https://doi.org/10.7910/DVN/2Y9VF9"),
                               ready4_type_1L_chr = "authoring",
                               user_manual_fns_chr = NA_character_)
#pkg_ds_ls_ls <- NULL
pkg_setup_ls$subsequent_ls$prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup") # Add to pkg_set_up logic (inc validation)
pkg_setup_ls <- ready4fun::validate_pkg_setup(pkg_setup_ls)
# pkg_setup_ls <- write_new_abbrs(pkg_setup_ls)
# pkg_setup_ls <- update_msng_abbrs(pkg_setup_ls,
#                                   are_words_chr = c("constructor", "validator"))
# pkg_setup_ls <- write_new_abbrs(pkg_setup_ls,
#                                 long_name_chr = c("initial","ready4class R package","require","unvalidated"),
#                                                    custom_plural_ls = NULL,
#                                                    no_plural_chr = c("ready4class package","unvalidated"))
rlang::exec(write_pkg_setup_fls, !!!pkg_setup_ls$initial_ls)
dss_records_ls <- write_pkg_dss(pkg_setup_ls,
                                pkg_url_1L_chr = pkg_setup_ls$initial_ls$pkg_desc_ls$URL %>%
                                  strsplit(",") %>%
                                  unlist() %>%
                                  purrr::pluck(1),
                                dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2])
##
# class(prototype_lup) <- class(prototype_lup)[3:5]
# name_pfx_1L_chr <- "ready4class_"
second_step_classes_tb <- classes_to_make_tb[-1,]
classes_to_make_tb <- classes_to_make_tb[1,]
fns_env_ls$fns_env$write_scripts_to_mk_r3_cls(name_stub_1L_chr = classes_to_make_tb$name_stub_chr,
                                              name_pfx_1L_chr = paste0(pkg_desc_ls$Package,"_"),
                                              output_dir_1L_chr = "R",
                                              class_desc_1L_chr = classes_to_make_tb$class_desc_chr,
                                              type_1L_chr = classes_to_make_tb$pt_ls[[1]][[1]],
                                              pt_chkr_pfx_1L_chr = classes_to_make_tb$pt_chkr_pfx_ls[[1]][[1]],
                                              pt_ns_1L_chr = ifelse(classes_to_make_tb$pt_ns_ls[[1]][[1]] %in% c("base"),
                                                                    "",
                                                                    classes_to_make_tb$pt_ns_ls[[1]][[1]]),
                                              vals_ls = classes_to_make_tb$vals_ls[[1]],
                                              allowed_vals_ls = classes_to_make_tb$allowed_vals_ls[[1]],
                                              min_max_vals_dbl = classes_to_make_tb$min_max_vals_ls[[1]][[1]],
                                              start_end_vals_dbl = classes_to_make_tb$start_end_vals_ls[[1]][[1]],
                                              prototype_lup = pkg_setup_ls$subsequent_ls$prototype_lup,
                                              file_exists_cdn_1L_chr = "overwrite",
                                              abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                              asserts_ls = classes_to_make_tb$asserts_ls[[1]],
                                              object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
classes_to_make_tb <- classes_to_make_tb %>%
  ready4class_constructor_tbl() %>%
  dplyr::bind_rows(second_step_classes_tb)
rm(second_step_classes_tb)
fns_env_ls$fns_env$write_classes.ready4class_constructor_tbl(classes_to_make_tb %>% dplyr::filter(name_stub_chr == "pt_lup"),
                                                             name_pfx_1L_chr = paste0(pkg_desc_ls$Package,"_"),
                                                             output_dir_1L_chr = "R",
                                                             prototype_lup = pkg_setup_ls$subsequent_ls$prototype_lup,
                                                             file_exists_cdn_1L_chr = "overwrite",
                                                             abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                                             object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
pkg_setup_ls$subsequent_ls$prototype_lup <- pkg_setup_ls$subsequent_ls$prototype_lup %>%
  ready4class_pt_lup()
write_clss(dss_records_ls = dss_records_ls,
           pkg_setup_ls = pkg_setup_ls,
           dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
           key_1L_chr = key_1L_chr,
           self_serve_1L_lgl = self_serve_1L_lgl,
           server_1L_chr = server_1L_chr)
# pkg_setup_ls <- ready4fun::write_new_obj_types(pkg_setup_ls,
#                                                long_name_chr = c("additional arguments"))
##
# 6. Add content to and document the package
##
## 6. Expand the ready4class_constructor_tbl instance to include information to make a new class of the class prototype lookup.
## 7. Call the write_classes method that we have defined in our R directory to create the scripts that will create scripts to make the classes in this table and save these scripts to the package R directory.
##    Note: In addition to creating a new class (ready4class_pt_lup) the call to the below method recreates the script to make the ready4class_constructor_tbl class. That duplication is of no importance in this step,
##    but having all the classes we have made summarised in one table (classes_to_make_tb) is necessary for a subsequent step (see DATASET.R in the data-raw directory.)


# Prior to running this script, the gh-pages orphan branch should be set up using instructions at:
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
#
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.

#
# 2. Create "fns", "gnrcs" and "mthds" sub-directories.

#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Set-up package structure

# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP: WRITE FUNCTION & METHODS FILES
#
# 6. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
# object_type_lup <- ready4fun::get_rds_from_dv("object_type_lup")
# pkg_dss_tb <- ready4fun::get_rds_from_dv("abbreviations_lup") %>%
#   ready4fun::write_abbr_lup(object_type_lup = object_type_lup)
# utils::data("abbreviations_lup")
# # 7. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
# pkg_dss_tb <- ready4fun::get_rds_from_dv("fn_types_lup") %>%
#   ready4fun::write_dmtd_fn_type_lup(abbreviations_lup = abbreviations_lup,
#                                     object_type_lup = object_type_lup,
#                                     pkg_dss_tb = pkg_dss_tb)
# utils::data("fn_types_lup")
# 8. Create a table of all functions to document
# fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(custom_dmt_ls = list(details_ls = NULL,
#                                                                        inc_for_main_user_lgl_ls = list(force_true_chr = c(#get_class_fl_nms"#"remake_ls_cols",
#                                                                                                                           #"remake_ls_cols.ready4class_constructor_tbl",
#                                                                                                                           #"write_classes_and_make_lup"#, "write_classes_and_make_lup.ready4class_constructor_tbl"
#                                                                                                                           ),
#                                                                                                        force_false_chr = NA_character_),
#                                                                        args_ls_ls = NULL),
#                                                   fn_types_lup = fn_types_lup,
#                                                   abbreviations_lup = abbreviations_lup,
#                                               object_type_lup = object_type_lup)
## 9. Write and document.
# pkg_dss_tb <- fns_dmt_tb %>%
#   ready4fun::write_and_doc_ds(db_1L_chr = "fns_dmt_tb",
#                               title_1L_chr = "ready4class function documentation table",
#                               desc_1L_chr = "Meta-data on each ready4class function used to create package documentation",
#                               url_1L_chr = "https://ready4-dev.github.io/ready4class/",
#                               abbreviations_lup = abbreviations_lup,
#                               object_type_lup = object_type_lup)

##
## 10. Run script to make package classes.
#source("data-raw/WRITE_CLASSES.R")
# prototype_lup <- prototype_lup %>%
#   ready4class_pt_lup()
# usethis::use_data(prototype_lup, overwrite = T, internal = T)
## 11. Remake the classes we previously created, this time using the new, preferred write_classes_and_make_lup method, which appends the metadata on the new classes to our instance of the ready4class_pt_lup class.
# prototype_lup <- write_classes_and_make_lup(classes_to_make_tb,
#                                  dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
#                                  name_pfx_1L_chr = name_pfx_1L_chr,
#                                  output_dir_1L_chr = "R",
#                                  file_exists_cdn_1L_chr = "overwrite",
#                                  abbreviations_lup = abbreviations_lup,
#                                  object_type_lup = object_type_lup)
## 12. Update the internal system data.
# ready4fun::write_and_doc_ds(prototype_lup,
#                             db_1L_chr = "prototype_lup",
#                             title_1L_chr = "Class prototype lookup table",
#                             desc_1L_chr = "Metadata on classes used in ready4 suite",
#                             abbreviations_lup = abbreviations_lup,
#                             object_type_lup = object_type_lup)
#
#usethis::use_data(prototype_lup,overwrite = T, internal = T)
##
# 13. MANUAL STEP - WRITE vignettes
# ENSURE that ready4fun::write_pkg_setup_fls function argument make_tmpl_vignette_lgl is not set to TRUE earlier in this script.
#
# 14. Update documentation
# usethis::use_build_ignore("initial_setup.R")
# usethis::use_package("rmarkdown", type = "Suggests")
readLines(".github/workflows/R-CMD-check.yaml")[-28] %>%
  writeLines(".github/workflows/R-CMD-check.yaml")
# ready4fun::write_and_doc_fn_fls(fns_dmt_tb = fns_dmt_tb,
#                                 r_dir_1L_chr = "R",
#                                 dev_pkgs_chr = "ready4fun",
#                                 path_to_dvpr_dmt_dir_1L_chr = "../../../../../Documentation/Code/Developer",
#                                 path_to_user_dmt_dir_1L_chr = "../../../../../Documentation/Code/User",
#                                 update_pkgdown_1L_lgl = T)
## PAUSE FOR INTERACTIVE
## PART 4
# prototype_lup %>%
#   ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "prototype_lup",
#                             desc_1L_chr = "Class prototypes lookup table")
##
# Remember to review publish updated dataverse dataset
##
# ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://github.com/ready4-dev/ready4class/releases/download/v0.0.0.9189/ready4class_user_0.0.0.9189.pdf",
#                           developer_manual_url_1L_chr = "https://github.com/ready4-dev/ready4class/releases/download/v0.0.0.9189/ready4class_developer_0.0.0.9189.pdf",
#                           project_website_url_1L_chr = "https://www.ready4-dev.com/")
# 15. Manual step: Push changes
## NOTE TO SELF: Need to implement variant of local git step outlined here: https://pkgdown.r-lib.org/reference/deploy_site_github.html



