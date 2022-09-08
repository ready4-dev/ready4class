renew.ready4class_constructor <- function(x,
                                              type_1L_chr = "listify",
                                              name_pfx_1L_chr = NULL){
  if(type_1L_chr == "listify"){
    x <- x %>%
      dplyr::mutate_at(dplyr::vars(pt_ls,
                                   pt_chkr_pfx_ls,
                                   pt_ns_ls,
                                   slots_ls,
                                   inc_clss_ls),
                       ~ purrr::map(., ~ list(.x)))
  }
  if(type_1L_chr == "order"){
    ordering_tb <- x %>%
      dplyr::select(name_stub_chr,pt_ls,parent_class_chr) %>%
      dplyr::mutate(class_name_chr = paste0(name_pfx_1L_chr,name_stub_chr)) %>%
      dplyr::mutate(preceeded_by = purrr::map2(pt_ls,parent_class_chr,
                                               ~ if(is.na(.y)){
                                                 unlist(.x)[unlist(.x) %in% class_name_chr]
                                               }else{
                                                 c(.y[.y %in% class_name_chr],unlist(.x)[unlist(.x) %in% class_name_chr])
                                               })) %>%
      dplyr::mutate(sequence = purrr::map2(preceeded_by,
                                           class_name_chr,
                                           ~ c(.x,.y)))
    ordering_chr <- purrr::reduce(ordering_tb %>%
                                    dplyr::pull(sequence),
                                  ~ append(.x,.y[!.y %in% .x])) %>%
      stringr::str_remove(name_pfx_1L_chr)
    x <- x[match(ordering_chr, x$name_stub_chr),]

  }
  return(x)
}
renew.ready4class_pt_lup <- function(x,
                                     attached_nss_chr,
                                     type_1L_chr = "namespace"){
  if(type_1L_chr == "namespace"){
    attached_nss_chr <- c("base",attached_nss_chr) %>% unique()
    x <- x %>%
      dplyr::mutate(val_chr = purrr::pmap_chr(dplyr::select(x,
                                                            pt_ns_chr,
                                                            fn_to_call_chr,
                                                            default_val_chr),
                                              ~ make_alg_to_get_pt_val(pt_ns_1L_chr = ..1,
                                                                       fn_to_call_1L_chr = ..2,
                                                                       default_val_1L_chr = ..3,
                                                                       attached_nss_chr = attached_nss_chr))
      )
  }
  return(x)
}
