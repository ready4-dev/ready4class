update_lup_for_ns.ready4class_pt_lup <- function(x,
                                                attached_nss_chr){
  attached_nss_chr <- c("base",attached_nss_chr) %>% unique()
  inst_ready4class_pt_lup <- x %>%
    dplyr::mutate(val_chr = purrr::pmap_chr(dplyr::select(x,
                                                        pt_ns_chr,
                                                        fn_to_call_chr,
                                                        default_val_chr),
                                          ~ make_alg_to_get_pt_val(pt_ns_1L_chr = ..1,
                                                             fn_to_call_1L_chr = ..2,
                                                             default_val_1L_chr = ..3,
                                                             attached_nss_chr = attached_nss_chr))
    )
  return(inst_ready4class_pt_lup)
}


