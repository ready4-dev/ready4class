#' Update lookup table for namespace method applied to Class Prototype Lookup Table readyforwhatsnext S3 class.
#' @description update_lup_for_ns.ready4_class_pt_lup() is an Update Lookup Table for Namespace method that updates a lookup table with namespace data. This method is implemented for the Class Prototype Lookup Table readyforwhatsnext S3 class. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param namespace_contexts PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_lup_for_ns.ready4_class_pt_lup
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap_chr
#' @keywords internal
update_lup_for_ns.ready4_class_pt_lup <- function (x, namespace_contexts) 
{
    namespace_contexts <- c("base", namespace_contexts) %>% unique()
    x %>% dplyr::mutate(value = purrr::pmap_chr(dplyr::select(x, 
        type_namespace, function_to_call, default_value), ~gen_pt_value_chr(type_namespace = ..1, 
        function_to_call = ..2, default_value = ..3, namespace_contexts = namespace_contexts)))
}
