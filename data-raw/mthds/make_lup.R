make_lup.ready4_class_make_tb <- function(x,
                                          dev_pckg_namespace,
                                          prefix){
  x %>%
    dplyr::mutate(type = paste0(prefix,name_stub),
                  pt_ns_chr = dev_pckg_namespace,
                  value = "",
                  fn_to_call_chr = type,
                  default_val_chr = "",
                  old_class = make_s3) %>%
    dplyr::select(type,  value, pt_ns_chr, fn_to_call_chr,default_val_chr,old_class) %>%
    ready4_class_pt_lup() %>%
    update_lup_for_ns(attached_nss_chr = dev_pckg_namespace)

}

