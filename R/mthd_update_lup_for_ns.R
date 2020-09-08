#' Update lookup table for namespace method applied to readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE.
#' @description update_lup_for_ns.ready4_class_pt_lup() is an Update Lookup Table for Namespace method that updates a lookup table with namespace data. This method is implemented for the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE. The function returns Instance of (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE).
#' @param x An instance of readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param attached_nss_chr Attached namespaces (a character vector)
#' @return Instance of (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE)
#' @rdname update_lup_for_ns.ready4_class_pt_lup
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap_chr
update_lup_for_ns.ready4_class_pt_lup <- function (x, attached_nss_chr) 
{
    attached_nss_chr <- c("base", attached_nss_chr) %>% unique()
    inst_of_ready4_class_pt_lup <- x %>% dplyr::mutate(val_chr = purrr::pmap_chr(dplyr::select(x, 
        pt_ns_chr, fn_to_call_chr, default_val_chr), ~make_alg_to_get_pt_val(pt_ns_1L_chr = ..1, 
        fn_to_call_1L_chr = ..2, default_val_1L_chr = ..3, attached_nss_chr = attached_nss_chr)))
    return(inst_of_ready4_class_pt_lup)
}
