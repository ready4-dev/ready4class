#' Make lookup table method applied to readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE.
#' @description make_lup.ready4_constructor_tbl() is a Make Lookup Table method that applies a Make method and then updates the output of that method. This method is implemented for the readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE. The function returns Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE).
#' @param x An instance of readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one)
#' @return Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE)
#' @rdname make_lup.ready4_constructor_tbl
#' @export 
#' @importFrom dplyr mutate select
make_lup.ready4_constructor_tbl <- function (x, dev_pkg_ns_1L_chr, prefix_1L_chr) 
{
    inst_ready4_class_pt_lup <- x %>% dplyr::mutate(type_chr = paste0(prefix_1L_chr, 
        name_stub_chr), pt_ns_chr = dev_pkg_ns_1L_chr, val_chr = "", 
        fn_to_call_chr = type_chr, default_val_chr = "", old_class_lgl = make_s3_lgl) %>% 
        dplyr::select(type_chr, val_chr, pt_ns_chr, fn_to_call_chr, 
            default_val_chr, old_class_lgl) %>% ready4_class_pt_lup() %>% 
        update_lup_for_ns(attached_nss_chr = dev_pkg_ns_1L_chr)
    return(inst_ready4_class_pt_lup)
}
methods::setMethod("make_lup", "ready4_constructor_tbl", make_lup.ready4_constructor_tbl)
