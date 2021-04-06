## This script creates the initial S3 class exported with this package.
##
## 1. Pre-requisites
## The following script to make the initial class was run once methods scripts were already in the package's data-raw/mthds/ folder.
##
## 2. Specify details about this package and the prefix we will use on all classes we create in this package.
name_pfx_1L_chr <- "ready4_"
#dev_pkg_ns_1L_chr <- ready4fun::get_dev_pkg_nm()
## 3. Import a table with metadata about frequently used class prototypes
prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup")

## 4. Create a table with the metadata for the ready4_constructor_tbl class that we wish to create.
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
                                                                           asserts_ls = "list()"), NULL, NULL, NULL, "ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.", NA_character_, NULL, NULL, NULL, NULL)
## 5. Convert the metadata table into a new S3 class called ready4_constructor_tbl
write_scripts_to_mk_r3_cls(name_stub_1L_chr = classes_to_make_tb$name_stub_chr,
              name_pfx_1L_chr = name_pfx_1L_chr,
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
              prototype_lup = prototype_lup,
              file_exists_cdn_1L_chr = "overwrite",
              abbreviations_lup = abbreviations_lup,
              asserts_ls = classes_to_make_tb$asserts_ls[[1]])
## 5. Convert the classes_to_make_tb tibble we created  into an instance of the ready4_constructor_tbl class also created in that step.
classes_to_make_tb <- classes_to_make_tb %>%
  ready4_constructor_tbl()
## 6. Expand the ready4_constructor_tbl instance to include information to make a new class of the class prototype lookup.
classes_to_make_tb <- classes_to_make_tb %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
    TRUE, "class_pt_lup", list("tibble"), list("is_"), list("tibble"), list(type_chr = "character(0)",
                                                                            val_chr = "character(0)",
                                                                            pt_ns_chr = "character(0)",
                                                                            fn_to_call_chr = "character(0)",
                                                                            default_val_chr = "character(0)",
                                                                            old_class_lgl = "logical(0)"), NULL, NULL, NULL, "ready4 S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.", NA_character_, NULL, NULL, NULL, NULL))
## 7. Call the write_classes method that we have defined in our R directory to create the scripts that will create scripts to make the classes in this table and save these scripts to the package R directory.
##    Note: In addition to creating a new class (ready4_class_pt_lup) the call to the below method recreates the script to make the ready4_constructor_tbl class. That duplication is of no importance in this step,
##    but having all the classes we have made summarised in one table (classes_to_make_tb) is necessary for a subsequent step (see DATASET.R in the data-raw directory.)
write_classes(classes_to_make_tb %>% dplyr::filter(name_stub_chr == "class_pt_lup"),
             name_pfx_1L_chr = name_pfx_1L_chr,
             output_dir_1L_chr = "R",
             prototype_lup = prototype_lup,
             file_exists_cdn_1L_chr = "overwrite",
             abbreviations_lup = abbreviations_lup)


