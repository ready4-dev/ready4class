#' Get class file names
#' @description get_class_fl_nms() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get class file names. The function returns Class file names (a character vector).
#' @param class_names_chr Class names (a character vector)
#' @param s3_1L_lgl S3 (a logical vector of length one), Default: T
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: NA
#' @return Class file names (a character vector)
#' @rdname get_class_fl_nms
#' @export 
#' @keywords internal
get_class_fl_nms <- function (class_names_chr, s3_1L_lgl = T, output_dir_1L_chr = NA) 
{
    class_fl_nms_chr <- paste0(ifelse(is.na(output_dir_1L_chr), 
        "", paste0(output_dir_1L_chr, "/")), ifelse(s3_1L_lgl, 
        "C3_", "C4_"), class_names_chr, ".R")
    return(class_fl_nms_chr)
}
#' Get class namespace
#' @description get_class_ns() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get class namespace. The function returns Class namespace (a character vector of length one).
#' @param prototype_lup Prototype (a lookup table)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @return Class namespace (a character vector of length one)
#' @rdname get_class_ns
#' @export 
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
get_class_ns <- function (prototype_lup, class_nm_1L_chr) 
{
    class_ns_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_1L_chr = "type_chr", match_value_xx = class_nm_1L_chr, 
        target_var_nm_1L_chr = "pt_ns_chr", evaluate_1L_lgl = F)
    return(class_ns_1L_chr)
}
#' Get names of classes to include
#' @description get_nms_of_clss_to_inc() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get names of classes to include. The function returns Names of classes to include (a character vector).
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param parent_ns_ls Parent namespace (a list)
#' @param base_set_of_clss_to_inc_chr Base set of classes to include (a character vector), Default: NULL
#' @return Names of classes to include (a character vector)
#' @rdname get_nms_of_clss_to_inc
#' @export 
#' @keywords internal
get_nms_of_clss_to_inc <- function (parent_cls_nm_1L_chr, parent_ns_ls, base_set_of_clss_to_inc_chr = NULL) 
{
    nms_of_clss_to_inc_chr <- base_set_of_clss_to_inc_chr
    if (!is.null(parent_cls_nm_1L_chr) & parent_ns_ls$transformed_1L_chr == 
        "") {
        nms_of_clss_to_inc_chr <- c(parent_cls_nm_1L_chr, nms_of_clss_to_inc_chr)
        nms_of_clss_to_inc_chr <- nms_of_clss_to_inc_chr[!duplicated(nms_of_clss_to_inc_chr)]
    }
    return(nms_of_clss_to_inc_chr)
}
#' Get names of current generics
#' @description get_nms_of_curr_gnrcs() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get names of current generics. The function returns Names of current generics (a list).
#' @param req_pkgs_chr Require packages (a character vector)
#' @param generic_1L_chr Generic (a character vector of length one)
#' @return Names of current generics (a list)
#' @rdname get_nms_of_curr_gnrcs
#' @export 
#' @importFrom purrr discard walk
#' @importFrom ready4fun force_instl_of_reqd_pkg
#' @importFrom methods getGenerics
#' @importFrom stringr str_replace_all
#' @keywords internal
get_nms_of_curr_gnrcs <- function (req_pkgs_chr, generic_1L_chr) 
{
    if (!req_pkgs_chr %>% purrr::discard(is.na) %>% identical(character(0))) 
        purrr::walk(req_pkgs_chr %>% purrr::discard(is.na), ~ready4fun::force_instl_of_reqd_pkg(.x))
    current_gens_s4 <- methods::getGenerics()
    packages_chr <- current_gens_s4@package
    curr_gnrcs_chr <- names(packages_chr) %>% stringr::str_replace_all("..GlobalEnv", 
        "")
    global_env_chr <- packages_chr %in% c(".GlobalEnv")
    in_global_1L_lgl <- generic_1L_chr %in% curr_gnrcs_chr[global_env_chr]
    nms_of_curr_gnrcs_ls <- list(curr_gnrcs_chr = curr_gnrcs_chr, 
        packages_chr = packages_chr, in_global_1L_lgl = in_global_1L_lgl)
    return(nms_of_curr_gnrcs_ls)
}
#' Get parent class namespace
#' @description get_parent_cls_ns() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get parent class namespace. The function is called for its side effects and does not return a value.
#' @param prototype_lup Prototype (a lookup table)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @return Parent class (a namespace)
#' @rdname get_parent_cls_ns
#' @export 
#' @keywords internal
get_parent_cls_ns <- function (prototype_lup, parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr) 
{
    if (!is.null(parent_cls_nm_1L_chr)) {
        untransformed_1L_chr <- get_class_ns(prototype_lup = prototype_lup, 
            class_nm_1L_chr = parent_cls_nm_1L_chr)
        transformed_1L_chr <- transform_class_ns(class_ns_1L_chr = untransformed_1L_chr, 
            dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr)
        parent_cls_ns <- list(untransformed_1L_chr = untransformed_1L_chr, 
            transformed_1L_chr = transformed_1L_chr)
    }
    else {
        parent_cls_ns <- list(untransformed_1L_chr = NULL, transformed_1L_chr = "")
    }
    return(parent_cls_ns)
}
#' Get parent class prototype function
#' @description get_parent_cls_pt_fn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get parent class prototype function. The function returns Parent class prototype function (a character vector of length one).
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param prototype_lup Prototype (a lookup table)
#' @return Parent class prototype function (a character vector of length one)
#' @rdname get_parent_cls_pt_fn
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
get_parent_cls_pt_fn <- function (parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    prototype_lup) 
{
    parent_cls_pt_fn_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_1L_chr = "type_chr", match_value_xx = parent_cls_nm_1L_chr, 
        target_var_nm_1L_chr = "val_chr", evaluate_1L_lgl = F)
    parent_ns_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_1L_chr = "type_chr", match_value_xx = parent_cls_nm_1L_chr, 
        target_var_nm_1L_chr = "pt_ns_chr", evaluate_1L_lgl = F)
    if (!parent_ns_1L_chr %in% c("base", dev_pkg_ns_1L_chr) & 
        !startsWith(parent_cls_pt_fn_1L_chr, paste0(parent_ns_1L_chr, 
            "::"))) 
        parent_cls_pt_fn_1L_chr <- paste0(parent_ns_1L_chr, "::", 
            parent_cls_pt_fn_1L_chr)
    return(parent_cls_pt_fn_1L_chr)
}
#' Get parent class prototypes
#' @description get_parent_cls_pts() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get parent class prototypes. The function returns Parent class prototypes (a character vector).
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param parent_ns_ls Parent namespace (a list)
#' @param slot_names_chr Slot names (a character vector)
#' @return Parent class prototypes (a character vector)
#' @rdname get_parent_cls_pts
#' @export 
#' @importFrom ready4fun force_instl_of_reqd_pkg
#' @importFrom purrr map_chr
#' @importFrom ready4 get_r4_obj_slots
#' @keywords internal
get_parent_cls_pts <- function (parent_cls_nm_1L_chr, parent_ns_ls, slot_names_chr) 
{
    if (ifelse(is.null(parent_ns_ls$transformed_1L_chr), F, ifelse(is.na(parent_ns_ls$transformed_1L_chr), 
        F, parent_ns_ls$transformed_1L_chr != ""))) 
        ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_1L_chr)
    parent_cls_pts_chr <- purrr::map_chr(slot_names_chr, ~ready4::get_r4_obj_slots(parent_cls_nm_1L_chr, 
        package_1L_chr = transform_parent_ns_ls(parent_ns_ls))[[.x]])
    return(parent_cls_pts_chr)
}
#' Get parent class slot names
#' @description get_parent_cls_slot_nms() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get parent class slot names. The function returns Parent class slot names (a character vector).
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param parent_ns_ls Parent namespace (a list)
#' @return Parent class slot names (a character vector)
#' @rdname get_parent_cls_slot_nms
#' @export 
#' @importFrom ready4fun force_instl_of_reqd_pkg
#' @importFrom ready4 get_r4_obj_slots
#' @keywords internal
get_parent_cls_slot_nms <- function (parent_cls_nm_1L_chr, parent_ns_ls) 
{
    if (ifelse(is.null(parent_ns_ls$transformed_1L_chr), F, ifelse(is.na(parent_ns_ls$transformed_1L_chr), 
        F, parent_ns_ls$transformed_1L_chr != ""))) 
        ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_1L_chr)
    parent_cls_slot_nms_chr <- ready4::get_r4_obj_slots(parent_cls_nm_1L_chr, 
        package = transform_parent_ns_ls(parent_ns_ls)) %>% names()
    return(parent_cls_slot_nms_chr)
}
