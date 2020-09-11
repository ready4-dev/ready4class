# write_scripts_to_mk_clss <- function(new_classes_ls,
#                              pckg_name_chr,
#                              class_pfx_chr,
#                              R_dir_chr = "R",
#                              pt_lup,
#                              description_ls = NULL,
#                              ignore_ns_chr = NA_character_,
#                              required_pckg_chr_vec = NA_character_){
#   reset_pkg_files_R(pckg_name_chr,
#                    description_ls = description_ls)
#   pt_lup <- make_class_pts_tb(new_classes_ls) %>%
#     make_and_update(dev_pckg_namespace = pckg_name_chr,
#                                  name_prefix = class_pfx_chr,
#                                  output_dir = R_dir_chr,
#                                  file_exists_logic = "overwrite",
#                                  init_class_pt_lup =  pt_lup,
#                                  ignore_ns_chr = ignore_ns_chr,
#                                  required_pckg_chr_vec = required_pckg_chr_vec, ## Need to implement new delete package logic now documenting and loading package with each new class.
#                                  class_in_cache_logic_chr = "overwrite")
#   usethis::use_data(pt_lup,overwrite = T)
#   write_pt_lup_db_R()
#   devtools::document()
#   devtools::load_all()
#   pt_lup
# }
# transform_pt_ls_for_new_clss <- function(new_classes_ls){
#   s3_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s3_pars")
#   s4_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s4_pars")
#   list(s3_ls = new_classes_ls %>% purrr::keep(s3_idx),
#        s4_ls = new_classes_ls %>% purrr::keep(s4_idx))
# }
