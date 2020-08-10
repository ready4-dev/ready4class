#' Simplify class name
#' @description simplify_class_name() is a Simplify function that simplifies and object. Specifically, this function implements an algorithm to simplify class a name. The function is called for its side effects and does not return a value.
#' @param st_class_fn_chr St class function (a character vector of length 1)
#' @param package_chr Package (a character vector of length 1)
#' @return NULL
#' @rdname simplify_class_name
#' @export 
#' @importFrom stringr str_replace
#' @keywords internal
simplify_class_name <- function (st_class_fn_chr, package_chr) 
{
    st_class_fn_chr %>% stringr::str_replace("methods::className\\(", 
        "") %>% stringr::str_replace(paste0(",\"", package_chr, 
        "\"\\)"), "")
}
