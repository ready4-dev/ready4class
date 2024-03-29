manufacture.ready4class_constructor <- function(x,
                                                dev_pkg_ns_1L_chr,
                                                prefix_1L_chr){
  inst_ready4class_pt_lup <- x %>%
    dplyr::mutate(pfx_chr = ifelse(make_s3_lgl,
                                   prefix_1L_chr,
                                   stringr::str_sub(prefix_1L_chr,
                                                    end = -2) %>%
                                     Hmisc::capitalize())) %>%
    dplyr::mutate(type_chr = paste0(pfx_chr,name_stub_chr),
                  pt_ns_chr = dev_pkg_ns_1L_chr,
                  val_chr = "",
                  fn_to_call_chr = type_chr,
                  default_val_chr = "",
                  old_class_lgl = make_s3_lgl) %>%
    dplyr::select(type_chr,  val_chr, pt_ns_chr, fn_to_call_chr,default_val_chr,old_class_lgl) %>%
    ready4class_pt_lup() %>%
    ready4::renew(attached_nss_chr = dev_pkg_ns_1L_chr)
  return(inst_ready4class_pt_lup)
}

