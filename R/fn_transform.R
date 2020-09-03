#' Transform algorithm to reference class name
#' @description transform_alg_to_ref_cls_nm() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to a transform alg to ref cls a name. Function argument st_class_fn_chr specifies the object to be updated. Argument package_chr provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param st_class_fn_chr St class function (a character vector)
#' @param package_chr Package (a character vector)
#' @return NULL
#' @rdname transform_alg_to_ref_cls_nm
#' @export 
#' @importFrom stringr str_replace
transform_alg_to_ref_cls_nm <- function (st_class_fn_chr, package_chr) 
{
    st_class_fn_chr %>% stringr::str_replace("methods::className\\(", 
        "") %>% stringr::str_replace(paste0(",\"", package_chr, 
        "\"\\)"), "")
}
#' Transform class namespace
#' @description transform_class_ns() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to a transform class namespace. Function argument class_ns_chr specifies the object to be updated. Argument dev_pckg_ns_chr provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param class_ns_chr Class namespace (a character vector)
#' @param dev_pckg_ns_chr Development pckg namespace (a character vector)
#' @return NULL
#' @rdname transform_class_ns
#' @export 

transform_class_ns <- function (class_ns_chr, dev_pckg_ns_chr) 
{
    ifelse(class_ns_chr %in% c("base", dev_pckg_ns_chr), "", 
        class_ns_chr)
}
#' Transform function into
#' @description transform_fn_into_chr() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to a transform function into. Function argument fn specifies the object to be updated. Argument NA provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param fn Function (a function)
#' @return NULL
#' @rdname transform_fn_into_chr
#' @export 

transform_fn_into_chr <- function (fn) 
{
    deparse(fn) %>% paste0(collapse = "\n")
}
#' Transform parent namespace
#' @description transform_parent_ns_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to a transform parent a namespace. Function argument parent_ns_ls specifies the object to be updated. Argument NA provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname transform_parent_ns_ls
#' @export 

transform_parent_ns_ls <- function (parent_ns_ls) 
{
    if (is.null(parent_ns_ls$untransformed_chr)) {
        parent_ns_ls$transformed_chr
    }
    else {
        ifelse(parent_ns_ls$untransformed_chr == "base", "base", 
            parent_ns_ls$transformed_chr)
    }
}
#' Transform prototype list for new classes
#' @description transform_pt_ls_for_new_clss() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to a transform prototype list for new clss. Function argument new_classes_ls specifies the object to be updated. Argument NA provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param new_classes_ls New classes (a list)
#' @return NULL
#' @rdname transform_pt_ls_for_new_clss
#' @export 
#' @importFrom purrr map_lgl keep
transform_pt_ls_for_new_clss <- function (new_classes_ls) 
{
    s3_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s3_pars")
    s4_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s4_pars")
    list(s3_ls = new_classes_ls %>% purrr::keep(s3_idx), s4_ls = new_classes_ls %>% 
        purrr::keep(s4_idx))
}
