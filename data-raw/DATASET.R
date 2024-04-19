library(ready4)
library(ready4fun)
library(ready4show)
library(ready4use)
#ready4fun::write_fn_type_dirs()
pkg_desc_ls <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Author and Document Classes That Make Health Economic Models Modular"  %>% tools::toTitleCase(),
                            pkg_desc_1L_chr = "ready4class provides tools to standardise and streamline the process for authoring modules for models developed with the ready4 framework (https://www.ready4-dev.com/).
  This development version of the ready4class package has been made available as part of the process of testing and documenting the package. You should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                            authors_prsn = c(utils::person(
                              given = "Matthew",family = "Hamilton", email =
                                "matthew.hamilton1@monash.edu",role = c("aut",
                                                                          "cre", "cph"),comment = c(ORCID = "0000-0001-7407-9194")
                            ),
                            utils::person("Glen", "Wiesner", #email = "Glen.Wiesner@vu.edu.au",
                                          role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                            #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                            #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                            #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                            #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                            utils::person("Orygen", role = c("cph", "fnd")),
                            utils::person("Australian Government Research Training Program", role =c("fnd")),
                            utils::person("VicHealth",role = c("fnd")),
                            utils::person("Victoria University", role =c("fnd"))
                            ),
                            urls_chr = c("https://ready4-dev.github.io/ready4class/",
                                         "https://github.com/ready4-dev/ready4class",
                                         "https://www.ready4-dev.com/"))
constructor_tb <- tibble::tribble(
  ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
  TRUE, "constructor", list("tibble"), list("is_"), list("tibble"), list(make_s3_lgl = "logical(0)",
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
                                                                             asserts_ls = "list()"), NULL, NULL, NULL, "Class constructor table", NA_character_, NULL, NULL, NULL, NULL,
  TRUE, "pt_lup", list("tibble"), list("is_"), list("tibble"), list(type_chr = "character(0)",
                                                                          val_chr = "character(0)",
                                                                          pt_ns_chr = "character(0)",
                                                                          fn_to_call_chr = "character(0)",
                                                                          default_val_chr = "character(0)",
                                                                          old_class_lgl = "logical(0)"), NULL, NULL, NULL, "Class prototype lookup table", NA_character_, NULL, NULL, NULL, NULL,
  TRUE, "manifest", list("list"), list("is."), list("base"), list(x_ready4fun_manifest = "ready4fun::ready4fun_manifest()",
                                                                  constructor_r3 = "ready4class_constructor()"), NULL, NULL, NULL, "Class creation manifest", NA_character_, NULL, NULL, NULL, NULL)
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- pkg_desc_ls %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "ready4",
                                                                       suggests_chr = c("rmarkdown","ready4show","ready4use")),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
                           check_type_1L_chr = "ready4",
                           cls_fn_ls = ready4fun::make_pt_ready4fun_executor(args_ls = list(x = constructor_tb),#
                                                                             fn =  fns_env_ls$fns_env$author.ready4class_constructor) %>%
                             ready4fun::ready4fun_executor(),
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(),
                           copyright_holders_chr = "Matthew Hamilton and Orygen",
                           dev_pkgs_chr = c(#"ready4",
                                            "ready4fun","ready4show","ready4use"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4class-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "authoring",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5640313.svg)](https://doi.org/10.5281/zenodo.5640313)") #  zenodo_badge_1L_chr =
x <- ready4::author(x,
                    self_serve_1L_lgl = T,
                    self_serve_fn_ls = list(fn = fns_env_ls$fns_env$write_self_srvc_clss,
                                            args_ls = NULL))
#ready4::write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows")
write_to_edit_workflow("pkgdown.yaml") # In other packages, run for "test-coverage.yaml" as well.
readme_chr <- readLines("README.md")
readme_chr[-which(readme_chr %>% purrr::map_lgl(~startsWith(.x, "[![test-coverage]")))] %>%
  writeLines(con = "README.md")
write_to_tidy_pkg(manifest_ls, build_vignettes_1L_lgl = TRUE,
                  clean_license_1L_lgl = TRUE, consent_1L_chr = "Y",
                  examples_chr = character(0), project_1L_chr = "Framework", suggest_chr = "pkgload")
