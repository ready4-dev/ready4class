#' Manufacture method applied to ready4 S3 class Constructor Table..
#' @description manufacture.ready4class_constructor() is a Manufacture method that creates a novel R object. This method is implemented for the ready4 S3 class Constructor Table. The function returns Instance (ready4 S3 class Prototype Lookup Table of class metadata.).
#' @param x An instance of ready4 S3 class Constructor Table.
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one)
#' @return Instance (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub
#' @importFrom Hmisc capitalize
#' @importFrom ready4 manufacture
manufacture.ready4class_constructor <- function (x, dev_pkg_ns_1L_chr, prefix_1L_chr) 
{
    inst_ready4class_pt_lup <- x %>% dplyr::mutate(pfx_chr = ifelse(make_s3_lgl, 
        prefix_1L_chr, stringr::str_sub(prefix_1L_chr, end = -2) %>% 
            Hmisc::capitalize())) %>% dplyr::mutate(type_chr = paste0(pfx_chr, 
        name_stub_chr), pt_ns_chr = dev_pkg_ns_1L_chr, val_chr = "", 
        fn_to_call_chr = type_chr, default_val_chr = "", old_class_lgl = make_s3_lgl) %>% 
        dplyr::select(type_chr, val_chr, pt_ns_chr, fn_to_call_chr, 
            default_val_chr, old_class_lgl) %>% ready4class_pt_lup() %>% 
        renew(attached_nss_chr = dev_pkg_ns_1L_chr)
    return(inst_ready4class_pt_lup)
}
#' @rdname manufacture-methods
#' @aliases manufacture,ready4class_constructor-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("ready4class_constructor", package = "ready4class"), manufacture.ready4class_constructor)
