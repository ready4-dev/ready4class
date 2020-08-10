#' Get class files
#' @description get_class_files_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get class files. Function argument class_names_chr_vec specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param class_names_chr_vec Class names (a character vector)
#' @param s3_lgl S3 (a logical vector of length 1), Default: T
#' @param output_dir_chr Output directory (a character vector of length 1), Default: NA
#' @return NULL
#' @rdname get_class_files_chr
#' @export 

#' @keywords internal
get_class_files_chr <- function (class_names_chr_vec, s3_lgl = T, output_dir_chr = NA) 
{
    paste0(ifelse(is.na(output_dir_chr), "", paste0(output_dir_chr, 
        "/")), ifelse(s3_lgl, "C3_", "C4_"), class_names_chr_vec, 
        ".R")
}
#' Get class namespace
#' @description get_class_ns_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get class a namespace. Function argument prototype_lup specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param prototype_lup Prototype (a lookup table)
#' @param class_chr Class (a character vector of length 1)
#' @return NULL
#' @rdname get_class_ns_chr
#' @export 
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
get_class_ns_chr <- function (prototype_lup, class_chr) 
{
    ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_chr = "type", match_value_xx = class_chr, 
        target_var_nm_chr = "type_namespace", evaluate_lgl = F)
}
#' Get included classes
#' @description get_included_classes_chr_vec() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get included classes. Function argument parent_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector of length 1)
#' @param parent_ns_ls Parent namespace (a list)
#' @param prespecified_includes_chr Prespecified includes (a character vector of length 1), Default: NULL
#' @return NULL
#' @rdname get_included_classes_chr_vec
#' @export 

#' @keywords internal
get_included_classes_chr_vec <- function (parent_chr, parent_ns_ls, prespecified_includes_chr = NULL) 
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
#' Get parent namespace
#' @description get_parent_ns_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent a namespace. Function argument prototype_lup specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param prototype_lup Prototype (a lookup table)
#' @param parent_chr Parent (a character vector of length 1)
#' @param dev_pckg_ns_chr Dev pckg namespace (a character vector of length 1)
#' @return NULL
#' @rdname get_parent_ns_ls
#' @export 

#' @keywords internal
get_parent_ns_ls <- function (prototype_lup, parent_chr, dev_pckg_ns_chr) 
{
    if (!is.null(parent_chr)) {
        untransformed_chr <- get_class_ns_chr(prototype_lup = prototype_lup, 
            class_chr = parent_chr)
        transformed_chr <- transform_class_ns_chr(class_ns_chr = untransformed_chr, 
            dev_pckg_ns_chr = dev_pckg_ns_chr)
        list(untransformed_chr = untransformed_chr, transformed_chr = transformed_chr)
    }
    else {
        list(untransformed_chr = NULL, transformed_chr = "")
    }
}
#' Get parent proto function
#' @description get_parent_proto_fn_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent proto a function. Function argument parent_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector of length 1)
#' @param prototype_lup Prototype (a lookup table)
#' @return NULL
#' @rdname get_parent_proto_fn_chr
#' @export 
#' @importFrom ready4fun get_from_lup_obj
#' @keywords internal
get_parent_proto_fn_chr <- function (parent_chr, prototype_lup) 
{
    parent_proto_fn_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
        match_var_nm_chr = "type", match_value_xx = parent_chr, 
        target_var_nm_chr = "value", evaluate_lgl = F)
}
#' Get parent prototypes
#' @description get_parent_prototypes() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent prototypes. Function argument parent_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector of length 1)
#' @param parent_ns_ls Parent namespace (a list)
#' @param slot_names_chr_vec Slot names (a character vector)
#' @return NULL
#' @rdname get_parent_prototypes
#' @export 
#' @importFrom ready4fun force_req_pkg_install
#' @importFrom purrr map_chr
#' @keywords internal
get_parent_prototypes <- function (parent_chr, parent_ns_ls, slot_names_chr_vec) 
{
    if (parent_ns_ls$transformed_chr != "") 
        ready4fun::force_req_pkg_install(parent_ns_ls$transformed_chr)
    purrr::map_chr(slot_names_chr_vec, ~get_r4_obj_slots_chr_vec(parent_chr, 
        package_chr = resolve_parent_ns_chr(parent_ns_ls))[[.x]])
}
#' Get parent slot names
#' @description get_parent_slot_names() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent slot names. Function argument parent_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param parent_chr Parent (a character vector of length 1)
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname get_parent_slot_names
#' @export 
#' @importFrom ready4fun force_req_pkg_install
#' @keywords internal
get_parent_slot_names <- function (parent_chr, parent_ns_ls) 
{
    if (parent_ns_ls$transformed_chr != "") 
        ready4fun::force_req_pkg_install(parent_ns_ls$transformed_chr)
    get_r4_obj_slots_chr_vec(parent_chr, package = resolve_parent_ns_chr(parent_ns_ls)) %>% 
        names()
}
#' Get proto list
#' @description get_proto_list() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get proto a list. Function argument class_slots specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param class_slots PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION, Default: NULL
#' @param values PARAM_DESCRIPTION, Default: NULL
#' @param make_val_string PARAM_DESCRIPTION, Default: TRUE
#' @param prototype_lup Prototype (a lookup table)
#' @return NULL
#' @rdname get_proto_list
#' @export 
#' @importFrom purrr map2_chr pmap_chr
#' @importFrom ready4fun get_from_lup_obj
#' @importFrom stringr str_c
#' @keywords internal
get_proto_list <- function (class_slots, type = NULL, values = NULL, make_val_string = TRUE, 
    prototype_lup) 
{
    proto_ls <- purrr::map2_chr(class_slots, type, ~paste0(.x, 
        " = ", ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
            match_var_nm_chr = "type", match_value_xx = .y, target_var_nm_chr = "value", 
            evaluate_lgl = FALSE)))
    if (!is.null(values)) {
        proto_ls <- purrr::pmap_chr(list(class_slots, proto_ls, 
            1:length(proto_ls)), ~{
            if (..3 %in% 1:length(values)) {
                paste0(..1, " = ", ifelse(make_val_string, "\"", 
                  ""), values[[..3]], ifelse(make_val_string, 
                  "\"", ""))
            }
            else {
                ..2
            }
        })
    }
    proto_ls %>% stringr::str_c(sep = "", collapse = ",") %>% 
        paste0("list(", ., ")")
}
