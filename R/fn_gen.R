#' Gen prototype value
#' @description gen_pt_value_chr() is a Gen function that generates values for an object. Specifically, this function implements an algorithm to gen a prototype value.NA
#' @param type_namespace PARAM_DESCRIPTION, Default: ''
#' @param function_to_call PARAM_DESCRIPTION, Default: ''
#' @param default_value PARAM_DESCRIPTION, Default: ''
#' @param namespace_contexts PARAM_DESCRIPTION, Default: c("base")
#' @return NULL
#' @rdname gen_pt_value_chr
#' @export 

#' @keywords internal
gen_pt_value_chr <- function (type_namespace = "", function_to_call = "", default_value = "", 
    namespace_contexts = c("base")) 
{
    paste0(ifelse(type_namespace %in% namespace_contexts, "", 
        paste0(type_namespace, "::")), function_to_call, ifelse(function_to_call == 
        "", "", "("), default_value, ifelse(function_to_call == 
        "", "", ")"))
}
