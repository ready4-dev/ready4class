add_class.ready4_class_pt_lup <- function(x,
                                          row_idx_1L_int,
                                          make_tb,
                                          dev_pkg_ns_1L_chr,
                                          name_pfx_1L_chr,
                                          output_dir_1L_chr,
                                          file_exists_cdn_1L_chr,
                                          nss_to_ignore_chr = NA_character_,
                                          req_pkgs_chr = NA_character_,
                                          class_in_cache_cdn_1L_chr = "stop"){
  make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
  write_classes(make_tb,
               name_pfx_1L_chr = name_pfx_1L_chr,
               output_dir_1L_chr = output_dir_1L_chr,
               file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
               prototype_lup = x,
               nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, nss_to_ignore_chr),
               req_pkgs_chr = req_pkgs_chr,
               class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr)
  new_pt_lup <- make_lup(make_tb,
                         dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr,
                         prefix = name_pfx_1L_chr)
  classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
  inst_of_ready4_class_pt_lup <- x %>%
    dplyr::filter(!type_chr %in% classes_to_add_chr)  %>%
    dplyr::bind_rows(new_pt_lup)
  return(inst_of_ready4_class_pt_lup)
}
