#' Write classes and make lookup table method applied to ready4 S3 class Constructor Table of metadata required to make new classes..
#' @description write_classes_and_make_lup.ready4class_constructor_tbl() is a Write Classes and Make Lookup Table method that makes new classes and creates or updates a class prototype lookup table. This method is implemented for the ready4 S3 class Constructor Table of metadata required to make new classes.. The function returns Instance (ready4 S3 class Prototype Lookup Table of class metadata.).
#' @param x An instance of ready4 S3 class Constructor Table of metadata required to make new classes.
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param name_pfx_1L_chr Name prefix (a character vector of length one), Default: paste0(ready4fun::get_dev_pkg_nm(), "_")
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'R'
#' @param delete_cdn_ptrn_chr Delete condition pattern (a character vector), Default: 'NA'
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: 'overwrite'
#' @param init_class_pt_lup Initial class prototype (a lookup table), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @return Instance (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname write_classes_and_make_lup-methods
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom purrr walk map_chr reduce
#' @importFrom stringi stri_replace_last
write_classes_and_make_lup.ready4class_constructor_tbl <- function (x, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"), 
    output_dir_1L_chr = "R", delete_cdn_ptrn_chr = NA_character_, 
    file_exists_cdn_1L_chr = "overwrite", init_class_pt_lup = NULL, 
    nss_to_ignore_chr = NA_character_, req_pkgs_chr = NA_character_, 
    class_in_cache_cdn_1L_chr = "stop", abbreviations_lup, object_type_lup) 
{
    if (is.null(init_class_pt_lup)) 
        init_class_pt_lup <- prototype_lup
    x <- order_tb(x, name_pfx_1L_chr)
    if (file_exists_cdn_1L_chr == "overwrite") {
        write_to_delete_gnrc_fn_fls(x, output_dir_1L_chr = output_dir_1L_chr)
        purrr::walk(delete_cdn_ptrn_chr, ~write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr, 
            pattern_1L_chr = .x))
    }
    new_files_chr <- paste0(purrr::map_chr(x$make_s3_lgl, ~ifelse(.x, 
        "C3_", "C4_")), name_pfx_1L_chr, x$name_stub_chr, ".R")
    consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
        ifelse(length(new_files_chr) > 1, "s ", " "), new_files_chr %>% 
            paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
            " and"), " to the directory ", output_dir_1L_chr, 
        " ?"), options_chr = c("Y", "N"), force_from_opts_1L_chr = T)
    if (consent_1L_chr == "Y") {
        inst_ready4class_pt_lup <- purrr::reduce(1:nrow(x), .init = init_class_pt_lup %>% 
            update_lup_for_ns(dev_pkg_ns_1L_chr), ~add_class(.x, 
            row_idx_1L_int = .y, make_tb = x, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
            file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
            nss_to_ignore_chr = nss_to_ignore_chr, req_pkgs_chr = req_pkgs_chr, 
            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
            abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup, 
            consent_1L_chr = consent_1L_chr))
    }
    else {
        inst_ready4class_pt_lup <- NULL
    }
    return(inst_ready4class_pt_lup)
}
#' @rdname write_classes_and_make_lup-methods
#' @aliases write_classes_and_make_lup,ready4class_constructor_tbl-method
methods::setMethod("write_classes_and_make_lup", "ready4class_constructor_tbl", write_classes_and_make_lup.ready4class_constructor_tbl)
