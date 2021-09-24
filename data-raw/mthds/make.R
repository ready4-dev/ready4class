make.ready4class_constructor_tbl <- function(x,
                                             dev_pkg_ns_1L_chr,
                                             prefix_1L_chr){
  inst_ready4class_pt_lup <- x %>%
    dplyr::mutate(type_chr = paste0(prefix_1L_chr,name_stub_chr),
                  pt_ns_chr = dev_pkg_ns_1L_chr,
                  val_chr = "",
                  fn_to_call_chr = type_chr,
                  default_val_chr = "",
                  old_class_lgl = make_s3_lgl) %>%
    dplyr::select(type_chr,  val_chr, pt_ns_chr, fn_to_call_chr,default_val_chr,old_class_lgl) %>%
    ready4class_pt_lup() %>%
    update_lup_for_ns(attached_nss_chr = dev_pkg_ns_1L_chr)
  return(inst_ready4class_pt_lup)
}

