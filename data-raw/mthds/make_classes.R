make_classes.ready4_class_make_tb <- function(x,
                                              name_prefix,
                                              output_dir,
                                              file_exists_logic = NULL,
                                              prototype_lup = NULL,
                                              nss_to_ignore_chr,
                                              req_pkgs_chr = NA_character_,
                                              class_in_cache_logic_chr = "stop"){
  purrr::pwalk(x %>% dplyr::filter(make_s3 == T),
               ~ write_scripts_to_mk_r3_cls(name_stub = ..2,
                               name_prefix = name_prefix,
                               output_dir_1L_chr = output_dir,
                               class_desc = ..10,
                               parent = if(is.na(..11)){
                                 NULL}else{
                                   ..11},
                               type = ..3[[1]],
                               type_checker_prefix = ..4[[1]],
                               pt_ns_1L_chr = ifelse(..5[[1]] %in% c("base"),"",..5[[1]]), ## THIS MAY NEED UPDATING
                               values = ..6,
                               allowed_vals_ls = ..7,
                               min_max_vals = ..8,
                               start_end_vals = ..9,
                               file_exists_logic = file_exists_logic,
                               prototype_lup = prototype_lup,
                               nss_to_ignore_chr = nss_to_ignore_chr))
  purrr::pwalk(x %>% dplyr::filter(make_s3 != T),
               ~ write_scripts_to_mk_r4_cls(name_stub = ..2,
                               name_prefix = name_prefix,
                               output_dir_1L_chr = output_dir,
                               class_desc = ..10,
                               parent = if(is.na(..11)){
                                 NULL}else{
                                   ..11},
                               class_slots = ..12[[1]],
                               type = ..3[[1]],
                               meaningful_names = ..13,
                               values = ..6[[1]],
                               allowed_vals_ls = ..7[[1]],
                               include_classes = ..14[[1]],
                               prototype_lup = prototype_lup,
                               nss_to_ignore_chr = nss_to_ignore_chr,
                               req_pkgs_chr = req_pkgs_chr,
                               class_in_cache_logic_chr = class_in_cache_logic_chr))
}


