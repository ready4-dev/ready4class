author.ready4class_manifest <- function(x,
                                        constructor_r3,
                                        dv_url_pfx_1L_chr = NULL,
                                        key_1L_chr = NULL,
                                        list_generics_1L_lgl = F,
                                        self_serve_1L_lgl = F,
                                        self_serve_fn_ls = NULL,
                                        server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  x$pkg_setup_ls$subsequent_ls$cls_fn_ls <- ready4fun::make_pt_ready4fun_fn_ls(args_ls = list(x = constructor_r3),
                                                                               fn =  write_classes_and_make_lup.ready4class_constructor_tbl) %>%
    ready4fun::ready4fun_fn_ls()
  x_ready4fun_pkg_setup <- ready4fun::author(x$pkg_setup_ls,
                                             dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                             key_1L_chr = key_1L_chr,
                                             list_generics_1L_lgl = list_generics_1L_lgl,
                                             self_serve_1L_lgl = self_serve_1L_lgl,
                                             self_serve_fn_ls = self_serve_fn_ls,
                                             server_1L_chr = server_1L_chr)
  return(x_ready4fun_pkg_setup)
  }


