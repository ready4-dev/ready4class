add_class.ready4_class_pt_lup <- function(x,
                                          tb_row_idx,
                                          make_tb,
                                          dev_pkg_ns,
                                          name_pfx_1L_chr,
                                          output_dir,
                                          file_exists_cdn_1L_chr,
                                          nss_to_ignore_chr = NA_character_,
                                          req_pkgs_chr = NA_character_,
                                          class_in_cache_cdn_1L_chr = "stop"){
  make_tb <- make_tb %>% dplyr::slice(tb_row_idx)
  make_classes(make_tb,
               name_pfx_1L_chr = name_pfx_1L_chr,
               output_dir = output_dir,
               file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
               prototype_lup = x,
               nss_to_ignore_chr = c(dev_pkg_ns, nss_to_ignore_chr),
               req_pkgs_chr = req_pkgs_chr,
               class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr)
  new_pt_lup <- make_lup(make_tb,
                         dev_pkg_ns = dev_pkg_ns,
                         prefix = name_pfx_1L_chr)
  classes_to_add_vec <- new_pt_lup %>% dplyr::pull(type_chr)
  inst_of_ready4_class_pt_lup <- x %>%
    dplyr::filter(!type_chr %in% classes_to_add_vec)  %>%
    dplyr::bind_rows(new_pt_lup)
  return(inst_of_ready4_class_pt_lup)
}
