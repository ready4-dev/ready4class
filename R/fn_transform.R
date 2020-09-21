#' Transform algorithm to reference class name
#' @description transform_alg_to_ref_cls_nm() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform algorithm to reference class name. Function argument alg_to_ref_cls_nm specifies the object to be updated. Argument pkg_nm_1L_chr provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param alg_to_ref_cls_nm PARAM_DESCRIPTION
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @return Transformed algorithm to reference class (a name)
#' @rdname transform_alg_to_ref_cls_nm
#' @export 
#' @importFrom stringr str_replace
transform_alg_to_ref_cls_nm <- function (alg_to_ref_cls_nm, pkg_nm_1L_chr) 
{
    tfd_alg_to_ref_cls_nm <- alg_to_ref_cls_nm %>% stringr::str_replace("methods::className\\(", 
        "") %>% stringr::str_replace(paste0(",\"", pkg_nm_1L_chr, 
        "\"\\)"), "")
    return(tfd_alg_to_ref_cls_nm)
}
#' Transform class namespace
#' @description transform_class_ns() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform class namespace. Function argument class_ns_1L_chr specifies the object to be updated. Argument dev_pkg_ns_1L_chr provides the object to be updated. The function returns Transformed class namespace (a character vector of length one).
#' @param class_ns_1L_chr Class namespace (a character vector of length one)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @return Transformed class namespace (a character vector of length one)
#' @rdname transform_class_ns
#' @export 

transform_class_ns <- function (class_ns_1L_chr, dev_pkg_ns_1L_chr) 
{
    tfd_class_ns_1L_chr <- ifelse(class_ns_1L_chr %in% c("base", 
        dev_pkg_ns_1L_chr), "", class_ns_1L_chr)
    return(tfd_class_ns_1L_chr)
}
#' Transform function into
#' @description transform_fn_into_chr() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform function into character vector. Function argument fn specifies the object to be updated. The function returns Function (a character vector of length one).
#' @param fn Function (a function)
#' @return Function (a character vector of length one)
#' @rdname transform_fn_into_chr
#' @export 

transform_fn_into_chr <- function (fn) 
{
    fn_1L_chr <- deparse(fn) %>% paste0(collapse = "\n")
    return(fn_1L_chr)
}
#' Transform parent namespace
#' @description transform_parent_ns_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform parent namespace list. Function argument parent_ns_ls specifies the object to be updated. The function returns Transformed parent namespace (a list).
#' @param parent_ns_ls Parent namespace (a list)
#' @return Transformed parent namespace (a list)
#' @rdname transform_parent_ns_ls
#' @export 

transform_parent_ns_ls <- function (parent_ns_ls) 
{
    if (is.null(parent_ns_ls$untransformed_1L_chr)) {
        tfd_parent_ns_ls <- parent_ns_ls$transformed_1L_chr
    }
    else {
        tfd_parent_ns_ls <- ifelse(parent_ns_ls$untransformed_1L_chr == 
            "base", "base", parent_ns_ls$transformed_1L_chr)
    }
    return(tfd_parent_ns_ls)
}
#' Transform prototype list for new classes
#' @description transform_pt_ls_for_new_clss() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform prototype list for new classes. Function argument pts_for_new_clss_ls specifies the object to be updated. The function returns Transformed prototypes for new classes (a list).
#' @param pts_for_new_clss_ls Prototypes for new classes (a list)
#' @return Transformed prototypes for new classes (a list)
#' @rdname transform_pt_ls_for_new_clss
#' @export 
#' @importFrom purrr map_lgl keep
transform_pt_ls_for_new_clss <- function (pts_for_new_clss_ls) 
{
    s3_idx <- pts_for_new_clss_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s3_pars")
    s4_idx <- pts_for_new_clss_ls %>% purrr::map_lgl(~class(.x) == 
        "test_new_s4_pars")
    tfd_pts_for_new_clss_ls <- list(s3_ls = pts_for_new_clss_ls %>% 
        purrr::keep(s3_idx), s4_ls = pts_for_new_clss_ls %>% 
        purrr::keep(s4_idx))
    return(tfd_pts_for_new_clss_ls)
}
