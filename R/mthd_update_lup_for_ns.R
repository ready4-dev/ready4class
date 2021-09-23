#' Update lookup table for namespace method applied to ready4 S3 class Prototype Lookup Table of class metadata..
#' @description update_lup_for_ns.ready4class_pt_lup() is an Update Lookup Table for Namespace method that updates a lookup table with namespace data. This method is implemented for the ready4 S3 class Prototype Lookup Table of class metadata. The function returns Instance (ready4 S3 class Prototype Lookup Table of class metadata.).
#' @param x An instance of ready4 S3 class Prototype Lookup Table of class metadata.
#' @param attached_nss_chr Attached namespaces (a character vector)
#' @return Instance (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname update_lup_for_ns-methods
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap_chr
update_lup_for_ns.ready4class_pt_lup <- function (x, attached_nss_chr) 
{
    attached_nss_chr <- c("base", attached_nss_chr) %>% unique()
    inst_ready4class_pt_lup <- x %>% dplyr::mutate(val_chr = purrr::pmap_chr(dplyr::select(x, 
        pt_ns_chr, fn_to_call_chr, default_val_chr), ~make_alg_to_get_pt_val(pt_ns_1L_chr = ..1, 
        fn_to_call_1L_chr = ..2, default_val_1L_chr = ..3, attached_nss_chr = attached_nss_chr)))
    return(inst_ready4class_pt_lup)
}
#' @rdname update_lup_for_ns-methods
#' @aliases update_lup_for_ns,ready4class_pt_lup-method
methods::setMethod("update_lup_for_ns", "ready4class_pt_lup", update_lup_for_ns.ready4class_pt_lup)
