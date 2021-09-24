library(ready4fun)
ready4fun::write_fn_type_dirs()
pkg_desc_ls <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Create, Extend And Document Classes And Methods For Open And Modular Mental Health Simulations",
                            pkg_desc_1L_chr = "ready4class provides tools to standardise and streamline the process for implementing object oriented approaches to developing open and modular mental health models.
  This development version of the ready4class package has been made available as part of the process of testing and documenting the package.  You should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
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
                                                                             asserts_ls = "list()"), NULL, NULL, NULL, "ready4 S3 class Constructor Table.", NA_character_, NULL, NULL, NULL, NULL,
  TRUE, "pt_lup", list("tibble"), list("is_"), list("tibble"), list(type_chr = "character(0)",
                                                                          val_chr = "character(0)",
                                                                          pt_ns_chr = "character(0)",
                                                                          fn_to_call_chr = "character(0)",
                                                                          default_val_chr = "character(0)",
                                                                          old_class_lgl = "logical(0)"), NULL, NULL, NULL, "ready4 S3 class Prototype Lookup Table.", NA_character_, NULL, NULL, NULL, NULL,
  TRUE, "manifest", list("list"), list("is."), list("base"), list(pkg_setup_r3 = "ready4fun::ready4fun_pkg_setup()",
                                                                  constructor_tbl_r3 = "ready4class_constructor_tbl()"), NULL, NULL, NULL, "ready4 S3 class Manifest.", NA_character_, NULL, NULL, NULL, NULL )
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
pkg_setup_ls <- pkg_desc_ls %>%
  ready4fun::make_pkg_setup_ls(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(suggests_chr = "rmarkdown"),
                               build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
                               check_type_1L_chr = "ready4",
                               cls_fn_ls = ready4fun::make_pt_ready4fun_fn_ls(args_ls = list(x = classes_to_make_tb),
                                                                              fn =  fns_env_ls$fns_env$author.ready4class_constructor_tbl) %>%
                                 ready4fun::ready4fun_fn_ls(),
                               copyright_holders_chr = "Orygen",
                               dev_pkgs_chr = "ready4fun",
                               lifecycle_stage_1L_chr = "experimental",
                               path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4class-logo/default.png",
                               pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                      "https://doi.org/10.7910/DVN/2Y9VF9"),
                               ready4_type_1L_chr = "authoring",
                               user_manual_fns_chr = NA_character_)
#pkg_ds_ls_ls <- NULL
pkg_setup_r3 <- ready4fun::author(pkg_setup_ls,
                                  self_serve_1L_lgl = T,
                                  self_serve_fn_ls = list(fn = fns_env_ls$fns_env$write_self_srvc_clss,
                                                      args_ls = NULL))
# pkg_setup_ls$subsequent_ls$prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup") # Add to pkg_set_up logic (inc validation)
# pkg_setup_ls <- ready4fun::validate_pkg_setup(pkg_setup_ls)
# pkg_setup_ls <- update_msng_abbrs(pkg_setup_ls,
#                                   are_words_chr = c("constructor", "validator"))
# pkg_setup_ls <- write_new_abbrs(pkg_setup_ls,
#                                 long_name_chr = c("initial","ready4class R package","require","unvalidated"),
#                                                    custom_plural_ls = NULL,
#                                                    no_plural_chr = c("ready4class package","unvalidated"))
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
# Prior to running this script, the gh-pages orphan branch should be set up using instructions at:
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
# 15. Manual step: Push changes
## NOTE TO SELF: Need to implement variant of local git step outlined here: https://pkgdown.r-lib.org/reference/deploy_site_github.html



