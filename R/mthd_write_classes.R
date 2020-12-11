#' Write classes method applied to readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE.
#' @description write_classes.ready4_constructor_tbl() is a Write Classes method that writes new classes. This method is implemented for the readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param x An instance of readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: NULL
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_classes-methods
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom utils data
#' @importFrom purrr pwalk flatten_chr
#' @importFrom dplyr filter
write_classes.ready4_constructor_tbl <- function (x, name_pfx_1L_chr, output_dir_1L_chr, file_exists_cdn_1L_chr = NULL, 
    prototype_lup = NULL, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    nss_to_ignore_chr, req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop", 
    abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4class", 
            envir = environment())
    purrr::pwalk(x %>% dplyr::filter(make_s3_lgl == T), ~write_scripts_to_mk_r3_cls(name_stub_1L_chr = ..2, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        class_desc_1L_chr = ..10, parent_cls_nm_1L_chr = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, type_1L_chr = ..3[[1]], pt_chkr_pfx_1L_chr = ..4[[1]], 
        pt_ns_1L_chr = ifelse(..5[[1]] %in% c("base"), "", ..5[[1]]), 
        vals_ls = ..6, allowed_vals_ls = ..7, min_max_vals_dbl = ..8[[1]], 
        start_end_vals_dbl = ..9[[1]], file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
        prototype_lup = prototype_lup, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
        nss_to_ignore_chr = nss_to_ignore_chr, abbreviations_lup = abbreviations_lup))
    purrr::pwalk(x %>% dplyr::filter(make_s3_lgl != T), ~write_scripts_to_mk_r4_cls(name_stub_1L_chr = ..2, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        class_desc_1L_chr = ..10, parent_cls_nm_1L_chr = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, slots_chr = ..12[[1]] %>% purrr::flatten_chr(), type_chr = ..3[[1]] %>% 
            purrr::flatten_chr(), meaningful_nms_ls = ..13, vals_ls = ..6[[1]], 
        allowed_vals_ls = ..7[[1]], clss_to_inc_chr = ..14[[1]], 
        prototype_lup = prototype_lup, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr))
}
#' @rdname write_classes-methods
#' @aliases write_classes,ready4_constructor_tbl-method
methods::setMethod("write_classes", "ready4_constructor_tbl", write_classes.ready4_constructor_tbl)
