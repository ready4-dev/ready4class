#' Transform class namespace
#' @description transform_class_ns_chr() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform class a namespace. Function argument class_ns_chr specifies the object to be updated. Argument dev_pckg_ns_chr provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param class_ns_chr Class namespace (a character vector of length 1)
#' @param dev_pckg_ns_chr Dev pckg namespace (a character vector of length 1)
#' @return NULL
#' @rdname transform_class_ns_chr
#' @export 

#' @keywords internal
transform_class_ns_chr <- function (class_ns_chr, dev_pckg_ns_chr) 
{
    ifelse(class_ns_chr %in% c("base", dev_pckg_ns_chr), "", 
        class_ns_chr)
}
#' Transform new classes
#' @description transform_new_classes_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform new classes. Function argument new_classes_ls specifies the object to be updated. Argument NA provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param new_classes_ls New classes (a list)
#' @return NULL
#' @rdname transform_new_classes_ls
#' @export 
#' @importFrom purrr map_lgl keep
#' @keywords internal
transform_new_classes_ls <- function (new_classes_ls) 
{
    s3_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s3_pars")
    s4_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s4_pars")
    list(s3_ls = new_classes_ls %>% purrr::keep(s3_idx), s4_ls = new_classes_ls %>% 
        purrr::keep(s4_idx))
}
