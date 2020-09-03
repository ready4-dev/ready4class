#' Get class file names
#' @description get_class_fl_nms() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a class file names. Function argument class_names_chr_vec specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param class_names_chr_vec PARAM_DESCRIPTION
#' @param s3_lgl S3 (a logical vector), Default: T
#' @param output_dir_chr Output directory (a character vector), Default: NA
#' @return NULL
#' @rdname get_class_fl_nms
#' @export 

#' @keywords internal
get_class_fl_nms <- function (class_names_chr_vec, s3_lgl = T, output_dir_chr = NA) 
{
    paste0(ifelse(is.na(output_dir_chr), "", paste0(output_dir_chr, 
        "/")), ifelse(s3_lgl, "C3_", "C4_"), class_names_chr_vec, 
        ".R")
}
#' Get class namespace
#' @description get_class_ns() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a class namespace. Function argument prototype_lup specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param prototype_lup Prototype (a lookup table)
#' @param class_chr Class (a character vector)
#' @return NULL
#' @rdname get_class_ns
#' @export 
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
get_class_ns <- function (prototype_lup, class_chr) 
{
    ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_1L_chr = "type", match_value_xx = class_chr, 
        target_var_nm_1L_chr = "type_namespace", evaluate_lgl = F)
}
#' Get names of classes to inc
#' @description get_nms_of_clss_to_inc() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get names of clss to inc. Function argument parent_chr specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector)
#' @param parent_ns_ls Parent namespace (a list)
#' @param prespecified_includes_chr Prespecified includes (a character vector), Default: NULL
#' @return NULL
#' @rdname get_nms_of_clss_to_inc
#' @export 

#' @keywords internal
get_nms_of_clss_to_inc <- function (parent_chr, parent_ns_ls, prespecified_includes_chr = NULL) 
{
    if (!is.null(parent_chr) & parent_ns_ls$transformed_chr == 
        "") {
        if (!is.null(prespecified_includes_chr)) {
            includes_chr_vec <- c(parent_chr, prespecified_includes_chr)
            includes_chr_vec[!duplicated(includes_chr_vec)]
        }
        else {
            parent_chr
        }
    }
}
#' Get names of current generics
#' @description get_nms_of_curr_gnrcs() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get names of curr gnrcs. Function argument required_pckg_chr_vec specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param required_pckg_chr_vec PARAM_DESCRIPTION
#' @param generic_chr Generic (a character vector)
#' @return NULL
#' @rdname get_nms_of_curr_gnrcs
#' @export 
#' @importFrom purrr discard walk
#' @importFrom ready4fun force_instl_of_reqd_pkg
#' @importFrom methods getGenerics
#' @importFrom stringr str_replace_all
#' @keywords internal
get_nms_of_curr_gnrcs <- function (required_pckg_chr_vec, generic_chr) 
{
    if (!required_pckg_chr_vec %>% purrr::discard(is.na) %>% 
        identical(character(0))) 
        purrr::walk(required_pckg_chr_vec %>% purrr::discard(is.na), 
            ~ready4fun::force_instl_of_reqd_pkg(.x))
    current_gens_s4 <- methods::getGenerics()
    package_chr_vec <- current_gens_s4@package
    current_gens_chr_vec <- names(package_chr_vec) %>% stringr::str_replace_all("..GlobalEnv", 
        "")
    global_env_chr_vec <- package_chr_vec %in% c(".GlobalEnv")
    in_global_lgl <- generic_chr %in% current_gens_chr_vec[global_env_chr_vec]
    list(current_gens_chr_vec = current_gens_chr_vec, package_chr_vec = package_chr_vec, 
        in_global_lgl = in_global_lgl)
}
#' Get parent class namespace
#' @description get_parent_cls_ns() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent cls a namespace. Function argument prototype_lup specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param prototype_lup Prototype (a lookup table)
#' @param parent_chr Parent (a character vector)
#' @param dev_pckg_ns_chr Development pckg namespace (a character vector)
#' @return NULL
#' @rdname get_parent_cls_ns
#' @export 

#' @keywords internal
get_parent_cls_ns <- function (prototype_lup, parent_chr, dev_pckg_ns_chr) 
{
    if (!is.null(parent_chr)) {
        untransformed_chr <- get_class_ns(prototype_lup = prototype_lup, 
            class_chr = parent_chr)
        transformed_chr <- transform_class_ns(class_ns_chr = untransformed_chr, 
            dev_pckg_ns_chr = dev_pckg_ns_chr)
        list(untransformed_chr = untransformed_chr, transformed_chr = transformed_chr)
    }
    else {
        list(untransformed_chr = NULL, transformed_chr = "")
    }
}
#' Get parent class prototype
#' @description get_parent_cls_pt_fn() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent cls a prototype. Function argument parent_chr specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector)
#' @param prototype_lup Prototype (a lookup table)
#' @return NULL
#' @rdname get_parent_cls_pt_fn
#' @export 
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
get_parent_cls_pt_fn <- function (parent_chr, prototype_lup) 
{
    parent_proto_fn_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_1L_chr = "type", match_value_xx = parent_chr, 
        target_var_nm_1L_chr = "value", evaluate_lgl = F)
}
#' Get parent class prototypes
#' @description get_parent_cls_pts() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent cls prototypes. Function argument parent_chr specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector)
#' @param parent_ns_ls Parent namespace (a list)
#' @param slot_names_chr_vec PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_parent_cls_pts
#' @export 
#' @importFrom ready4fun force_instl_of_reqd_pkg get_r4_obj_slots
#' @importFrom purrr map_chr
#' @keywords internal
get_parent_cls_pts <- function (parent_chr, parent_ns_ls, slot_names_chr_vec) 
{
    if (ifelse(is.null(parent_ns_ls$transformed_chr), F, ifelse(is.na(parent_ns_ls$transformed_chr), 
        F, parent_ns_ls$transformed_chr != ""))) 
        ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_chr)
    purrr::map_chr(slot_names_chr_vec, ~ready4fun::get_r4_obj_slots(parent_chr, 
        package_1L_chr = transform_parent_ns_ls(parent_ns_ls))[[.x]])
}
#' Get parent class slot names
#' @description get_parent_cls_slot_nms() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent cls slot names. Function argument parent_chr specifies the where to look for the required object.The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector)
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname get_parent_cls_slot_nms
#' @export 
#' @importFrom ready4fun force_instl_of_reqd_pkg get_r4_obj_slots
#' @keywords internal
get_parent_cls_slot_nms <- function (parent_chr, parent_ns_ls) 
{
    if (ifelse(is.null(parent_ns_ls$transformed_chr), F, ifelse(is.na(parent_ns_ls$transformed_chr), 
        F, parent_ns_ls$transformed_chr != ""))) 
        ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_chr)
    ready4fun::get_r4_obj_slots(parent_chr, package = transform_parent_ns_ls(parent_ns_ls)) %>% 
        names()
}
