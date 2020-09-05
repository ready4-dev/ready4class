make_and_update.ready4_class_make_tb  <- function(x,
                                                  dev_pkg_ns = ready4fun::get_dev_pkg_nm(),
                                                  name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(),"_"),
                                                  output_dir = "R",
                                                  delete_cdn_ptrn_chr = NA_character_,
                                                  file_exists_cdn_1L_chr = "overwrite",
                                                  init_class_pt_lup = NULL,
                                                  nss_to_ignore_chr = NA_character_,
                                                  req_pkgs_chr = NA_character_,
                                                  class_in_cache_cdn_1L_chr = "stop"){
  if(is.null(init_class_pt_lup))
    init_class_pt_lup <- prototype_lup
  x <- order_tb(x, name_pfx_1L_chr)
  if(file_exists_cdn_1L_chr == "overwrite"){
    write_to_delete_gnrc_fn_fls(x,output_dir = output_dir)
    purrr::walk(delete_cdn_ptrn_chr,
                ~ write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir,
                              pattern_1L_chr = .x))
  }
  purrr::reduce(1:nrow(x),
                .init = init_class_pt_lup %>% update_lup_for_ns(dev_pkg_ns),
                ~ add_class(.x,
                            tb_row_idx = .y,
                            make_tb = x,
                            dev_pkg_ns = dev_pkg_ns,
                            name_pfx_1L_chr = name_pfx_1L_chr,
                            output_dir = output_dir,
                            file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
                            nss_to_ignore_chr = nss_to_ignore_chr,
                            req_pkgs_chr = req_pkgs_chr,
                            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr))
}


