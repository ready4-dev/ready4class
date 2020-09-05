#' Make lookup table method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_lup.ready4_class_make_tb() is a Make Lookup Table method that makes a lookup table. This method is implemented for the Class Make Table readyforwhatsnext S3 class.The function returns inst of ready4 class prototype (a lookup table).
#' @param x PARAM_DESCRIPTION
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one)
#' @return Inst of ready4 class prototype (a lookup table)
#' @rdname make_lup.ready4_class_make_tb
#' @export 
#' @importFrom dplyr mutate select
make_lup.ready4_class_make_tb <- function (x, dev_pkg_ns_1L_chr, prefix_1L_chr) 
{
    inst_of_ready4_class_pt_lup <- x %>% dplyr::mutate(type_chr = paste0(prefix_1L_chr, 
        name_stub_chr), pt_ns_chr = dev_pkg_ns_1L_chr, val_chr = "", 
        fn_to_call_chr = type_chr, default_val_chr = "", old_class_lgl = make_s3_lgl) %>% 
        dplyr::select(type_chr, val_chr, pt_ns_chr, fn_to_call_chr, 
            default_val_chr, old_class_lgl) %>% ready4_class_pt_lup() %>% 
        update_lup_for_ns(attached_nss_chr = dev_pkg_ns_1L_chr)
    return(inst_of_ready4_class_pt_lup)
}
