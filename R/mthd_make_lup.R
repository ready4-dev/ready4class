#' Make lookup table method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_lup.ready4_class_make_tb() is a Make Lookup Table method that makes a lookup table. This method is implemented for the Class Make Table readyforwhatsnext S3 class.NA
#' @param x PARAM_DESCRIPTION
#' @param dev_pckg_namespace PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_lup.ready4_class_make_tb
#' @export 
#' @importFrom dplyr mutate select
make_lup.ready4_class_make_tb <- function (x, dev_pckg_namespace, prefix) 
{
    x %>% dplyr::mutate(type = paste0(prefix, name_stub), type_namespace = dev_pckg_namespace, 
        value = "", function_to_call = type, default_value = "", 
        old_class = make_s3) %>% dplyr::select(type, value, type_namespace, 
        function_to_call, default_value, old_class) %>% ready4_class_pt_lup() %>% 
        update_lup_for_ns(namespace_contexts = dev_pckg_namespace)
}
