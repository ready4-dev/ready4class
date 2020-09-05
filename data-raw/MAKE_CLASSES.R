## This script creates the initial S3 class exported with this package.
##
## 1. Pre-requisites
## The following script to make the initial class was run once the following items were already in the package's data-raw/mthds/ folder:
## fn_write_scripts_to_mk_r3_cls.R
## fn_write_scripts_to_mk_r4_cls.R
## import_methods.R
## import_pipe.R
## mthd_make_classes.R
## pckg_ready4class.R
##
## 2. Document and load all package functions.
# devtools::document()
# devtools::load_all()
#ready4fun::read_fns("data-raw/mthds/")
##
## 3. Specify details about this package and the prefix we will use on all classes we create in this package.
name_pfx_1L_chr <- "ready4_"
dev_pkg_ns <- "ready4class"
## 2. Create a table with metadata about frequently used class prototypes
prototype_lup <- tibble::tibble(type_chr = c("character", "list", "logical", "numeric", "POSIXt", "sf","tbl_df"), ## CHANGED FROM POSIXt
                                val_chr = c("NA_character_", "list(list())","NA","NA_real_",".POSIXct(NA_character_)", "st_sf(sf::st_sfc())","tibble::tibble()"),
                                pt_ns_chr = c("base", "base", "base", "base", "base", "sf", "tibble"),
                                fn_to_call_chr = c("", "list", "", "", ".POSIXct", "st_sf", "tibble"),
                                default_val_chr = c("NA_character_", "list()", "NA", "NA_real_", "NA_character_", "sf::st_sfc()", ""),
                                old_class_lgl = FALSE)
## 3. Create a table with the metadata for the ready4_class_make_tb class that we wish to create.
classes_to_make_tb <- tibble::tribble(
  ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~inc_clss_ls,
  TRUE, "class_make_tb", list("tibble"), list("is_"), list("tibble"), list(make_s3_lgl = "logical(0)",
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
                                                                           inc_clss_ls = "list()"), NULL, NULL, NULL, "MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.", NA_character_, NULL, NULL, NULL)
## 4. Convert the metadata table into a new S3 class called ready4_class_make_tb
write_scripts_to_mk_r3_cls(name_stub_1L_chr = classes_to_make_tb$name_stub_chr,
              name_pfx_1L_chr = name_pfx_1L_chr,
              output_dir_1L_chr = "R",
              class_desc_chr = classes_to_make_tb$class_desc_chr,
              type_1L_chr = classes_to_make_tb$pt_ls[[1]],
              pt_chkr_pfx_1L_chr = classes_to_make_tb$pt_chkr_pfx_ls[[1]],
              pt_ns_1L_chr = ifelse(classes_to_make_tb$pt_ns_ls[[1]] %in% c("base"),
                                      "",
                                      classes_to_make_tb$pt_ns_ls[[1]]),
              vals_ls = classes_to_make_tb$values[[1]],
              allowed_vals_ls = classes_to_make_tb$allowed_vals_ls[[1]],
              min_max_vals_dbl = classes_to_make_tb$min_max_vals_ls[[1]],
              start_end_vals_dbl = classes_to_make_tb$start_end_vals_ls[[1]],
              prototype_lup = prototype_lup,
              file_exists_cdn_1L_chr = "overwrite")
## 5. Convert the classes_to_make_tb tibble we created  into an instance of the ready4_class_make_tb class also created in that step.
classes_to_make_tb <- classes_to_make_tb %>%
  ready4_class_make_tb()
## 6. Expand the ready4_class_make_tb instance to include information to make a new class of the class prototype lookup.
classes_to_make_tb <- classes_to_make_tb %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~inc_clss_ls,
    TRUE, "class_pt_lup", list("tibble"), list("is_"), list("tibble"), list(type_chr = "character(0)",
                                                                            value = "character(0)",
                                                                            pt_ns_chr = "character(0)",
                                                                            fn_to_call_chr = "character(0)",
                                                                            default_val_chr = "character(0)",
                                                                            old_class_lgl = "logical(0)"), NULL, NULL, NULL, "PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.", NA_character_, NULL, NULL, NULL))
## 7. Call the make_classes method that we have defined in our R directory to create the scripts that will create scripts to make the classes in this table and save these scripts to the package R directory.
##    Note: In addition to creating a new class (ready4_class_pt_lup) the call to the below method recreates the script to make the ready4_class_make_tb class. That duplication is of no importance in this step,
##    but having all the classes we have made summarised in one table (classes_to_make_tb) is necessary for a subsequent step (see DATASET.R in the data-raw directory.)
make_classes(classes_to_make_tb %>% dplyr::filter(name_stub_chr == "class_pt_lup"),
             name_pfx_1L_chr = name_pfx_1L_chr,
             output_dir = "R",
             prototype_lup = prototype_lup,
             file_exists_cdn_1L_chr = "overwrite")


