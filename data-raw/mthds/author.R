author.ready4class_manifest <- function(x,
                                        dv_url_pfx_1L_chr = NULL,
                                        init_class_pt_lup = NULL,
                                        key_1L_chr = NULL,
                                        list_generics_1L_lgl = F,
                                        nss_to_ignore_chr = NA_character_,
                                        req_pkgs_chr = NA_character_,
                                        self_serve_1L_lgl = F,
                                        self_serve_fn_ls = NULL,
                                        server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  x$pkg_setup_ls$subsequent_ls$cls_fn_ls <- ready4fun::make_pt_ready4fun_fn_ls(args_ls = list(x = x$constructor_tbl_r3,
                                                                                              dev_pkg_ns_1L_chr = x$initial_ls$pkg_desc_ls$Package,
                                                                                              name_pfx_1L_chr = paste0(x$initial_ls$pkg_desc_ls$Package,"_"),
                                                                                              output_dir_1L_chr = paste0(x$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                                                                                              delete_cdn_ptrn_chr = NA_character_,
                                                                                              file_exists_cdn_1L_chr = "overwrite",
                                                                                              init_class_pt_lup = init_class_pt_lup,
                                                                                              nss_to_ignore_chr = nss_to_ignore_chr,
                                                                                              req_pkgs_chr = req_pkgs_chr,
                                                                                              class_in_cache_cdn_1L_chr = "stop",
                                                                                              abbreviations_lup = x$subsequent_ls$abbreviations_lup,
                                                                                              object_type_lup = x$subsequent_ls$object_type_lup),
                                                                               fn =  write_classes_and_make_lup.ready4class_constructor_tbl) %>%
    ready4fun::ready4fun_fn_ls()
  x_ready4fun_pkg_setup <- ready4fun::author(x$pkg_setup_r3,
                                             dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                             key_1L_chr = key_1L_chr,
                                             list_generics_1L_lgl = list_generics_1L_lgl,
                                             self_serve_1L_lgl = self_serve_1L_lgl,
                                             self_serve_fn_ls = self_serve_fn_ls,
                                             server_1L_chr = server_1L_chr)
  return(x_ready4fun_pkg_setup)
  }


