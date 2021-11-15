#' authorClasses - a method that authors and saves files necessary for creating and documenting classes
#' @description authorClasses.ready4class_constructor() is an authorClasses method that authors and saves files necessary for creating and documenting classes. This method is implemented for the ready4 S3 class Constructor Table. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Constructor Table.
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: NULL
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param fn_types_lup Function types (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname authorClasses-methods
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom purrr map_chr pwalk flatten_chr flatten
#' @importFrom stringr str_sub
#' @importFrom Hmisc capitalize
#' @importFrom ready4 make_prompt authorClasses
#' @importFrom stringi stri_replace_last
#' @importFrom dplyr filter
authorClasses.ready4class_constructor <- function (x, name_pfx_1L_chr, output_dir_1L_chr, file_exists_cdn_1L_chr = NULL, 
    prototype_lup = NULL, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    nss_to_ignore_chr, req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop", 
    abbreviations_lup, fn_types_lup, object_type_lup, consent_1L_chr = NULL) 
{
    new_files_chr <- paste0(purrr::map_chr(x$make_s3_lgl, ~ifelse(.x, 
        "C3_", "C4_")), purrr::map_chr(x$make_s3_lgl, ~ifelse(.x, 
        name_pfx_1L_chr, stringr::str_sub(name_pfx_1L_chr, end = -2) %>% 
            Hmisc::capitalize())), x$name_stub_chr, ".R")
    if (is.null(consent_1L_chr)) {
        consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
            ifelse(length(new_files_chr) > 1, "s ", " "), new_files_chr %>% 
                paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                " and"), " to the directory ", output_dir_1L_chr, 
            " ?"), options_chr = c("Y", "N"), force_from_opts_1L_chr = T)
    }
    if (consent_1L_chr == "Y") {
        purrr::pwalk(x %>% dplyr::filter(make_s3_lgl == T), ~write_scripts_to_mk_r3_cls(name_stub_1L_chr = ..2, 
            name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
            class_desc_1L_chr = ..10, parent_cls_nm_1L_chr = if (is.na(..11)) {
                NULL
            }
            else {
                ..11
            }, type_1L_chr = ..3[[1]], pt_chkr_pfx_1L_chr = ..4[[1]], 
            pt_ns_1L_chr = ifelse(..5[[1]] %in% c("base"), "", 
                ..5[[1]]), vals_ls = ..6, allowed_vals_ls = ..7, 
            min_max_vals_dbl = ..8[[1]], start_end_vals_dbl = ..9[[1]], 
            file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
            prototype_lup = prototype_lup, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            nss_to_ignore_chr = nss_to_ignore_chr, abbreviations_lup = abbreviations_lup, 
            asserts_ls = ..15, object_type_lup = object_type_lup, 
            fn_types_lup = fn_types_lup, consent_1L_chr = consent_1L_chr))
        purrr::pwalk(x %>% dplyr::filter(make_s3_lgl != T), ~write_scripts_to_mk_r4_cls(name_stub_1L_chr = ..2, 
            name_pfx_1L_chr = stringr::str_sub(name_pfx_1L_chr, 
                end = -2) %>% Hmisc::capitalize(), output_dir_1L_chr = output_dir_1L_chr, 
            class_desc_1L_chr = ..10, parent_cls_nm_1L_chr = if (is.na(..11)) {
                NULL
            }
            else {
                ..11
            }, slots_chr = if (is.list(..12[[1]])) {
                ..12[[1]] %>% purrr::flatten_chr()
            }
            else {
                ..12 %>% purrr::flatten() %>% purrr::flatten_chr()
            }, type_chr = if (is.list(..3[[1]])) {
                ..3[[1]] %>% purrr::flatten_chr()
            }
            else {
                ..3 %>% purrr::flatten() %>% purrr::flatten_chr()
            }, meaningful_nms_ls = ..13, vals_ls = ..6[[1]], 
            allowed_vals_ls = ..7[[1]], clss_to_inc_chr = ..14[[1]], 
            prototype_lup = prototype_lup, nss_to_ignore_chr = nss_to_ignore_chr, 
            req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
            asserts_ls = ..15[[1]], abbreviations_lup = abbreviations_lup, 
            fn_types_lup = fn_types_lup, object_type_lup = object_type_lup, 
            consent_1L_chr = consent_1L_chr))
    }
}
#' @rdname authorClasses-methods
#' @aliases authorClasses,ready4class_constructor-method
#' @importFrom ready4 authorClasses
methods::setMethod("authorClasses", methods::className("ready4class_constructor", package = "ready4class"), authorClasses.ready4class_constructor)
