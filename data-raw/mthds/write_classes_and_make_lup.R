write_classes_and_make_lup.ready4_constructor_tbl <- function(x,
                                                              dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                                              name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(),"_"),
                                                              output_dir_1L_chr = "R",
                                                              delete_cdn_ptrn_chr = NA_character_,
                                                              file_exists_cdn_1L_chr = "overwrite",
                                                              init_class_pt_lup = NULL,
                                                              nss_to_ignore_chr = NA_character_,
                                                              req_pkgs_chr = NA_character_,
                                                              class_in_cache_cdn_1L_chr = "stop",
                                                              abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup", package = "ready4class",
         envir = environment())
  if(is.null(init_class_pt_lup))
    init_class_pt_lup <- prototype_lup
  x <- order_tb(x, name_pfx_1L_chr)
  if(file_exists_cdn_1L_chr == "overwrite"){
    write_to_delete_gnrc_fn_fls(x,output_dir_1L_chr = output_dir_1L_chr)
    purrr::walk(delete_cdn_ptrn_chr,
                ~ write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr,
                                                pattern_1L_chr = .x))
  }
  inst_of_ready4_class_pt_lup <- purrr::reduce(1:nrow(x),
                                               .init = init_class_pt_lup %>% update_lup_for_ns(dev_pkg_ns_1L_chr),
                                               ~ add_class(.x,
                                                           row_idx_1L_int = .y,
                                                           make_tb = x,
                                                           dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr,
                                                           name_pfx_1L_chr = name_pfx_1L_chr,
                                                           output_dir_1L_chr = output_dir_1L_chr,
                                                           file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
                                                           nss_to_ignore_chr = nss_to_ignore_chr,
                                                           req_pkgs_chr = req_pkgs_chr,
                                                           class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr,
                                                           abbreviations_lup = abbreviations_lup))
  return(inst_of_ready4_class_pt_lup)
}


