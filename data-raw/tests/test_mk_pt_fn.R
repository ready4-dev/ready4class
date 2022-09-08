pt_1 <- make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                            name_stub_chr = "dataverses",
                                            pt_ls = list(list("tibble")),
                                            pt_chkr_pfx_ls = list(list("is_")),
                                            pt_ns_ls = list(list("tibble")),
                                            vals_ls = list(list(file_type_chr = "character(0)",
                                                                file_name_chr = "character(0)",
                                                                data_repo_chr = "character(0)",
                                                                data_repo_ui_chr = "character(0)",
                                                                data_repo_db_ui_chr = "character(0)",
                                                                data_repo_file_ext_chr = "character(0)",
                                                                data_repo_save_type_chr = "character(0)")),
                                            class_desc_chr = "ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.")
pt_2 <- make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                            name_stub_chr = "imports",
                                            pt_ls = list(list("tibble")),
                                            pt_chkr_pfx_ls = list(list("is_")),
                                            pt_ns_ls = list(list("tibble")),
                                            vals_ls = list(list(local_file_src_chr = "character(0)",
                                                                path_to_make_script_chr = "character(0)",
                                                                download_url_chr = "character(0)",
                                                                inc_file_main_chr = "character(0)",
                                                                inc_fls_to_rename_ls = "list()",
                                                                new_nms_for_inc_fls_ls = "list()")),
                                            class_desc_chr = "ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.",
                                            parent_class_chr = "ready4class_dataverses")

make_fn_pt_to_make_r3_cls_pt(type_1L_chr = pt_1$pt_ls[[1]][[1]],
                             pt_ns_1L_chr = pt_1$pt_ns_ls[[1]][[1]],
                             vals_ls = pt_1$vals_ls[[1]],
                             ordered_1L_lgl = T,
                             class_nm_1L_chr = "test_class",
                             parent_cls_nm_1L_chr = NULL,
                             dev_pkg_ns_1L_chr = "test_pkg",
                             prototype_lup = ready4use::prototype_lup) %>%
  purrr::pluck(2) %>%
  writeLines()
make_fn_pt_to_make_r3_cls_pt(type_1L_chr = pt_2$pt_ls[[1]][[1]],
                             pt_ns_1L_chr = pt_2$pt_ns_ls[[1]][[1]],
                             vals_ls = pt_2$vals_ls[[1]],
                             ordered_1L_lgl = T,
                             class_nm_1L_chr = "test_class",
                             parent_cls_nm_1L_chr = pt_2$parent_class_chr,
                             dev_pkg_ns_1L_chr = "test_pkg",
                             prototype_lup = ready4use::prototype_lup) %>%
  purrr::pluck(2) %>%
  writeLines()
