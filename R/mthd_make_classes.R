#' Make classes method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_classes.ready4_class_make_tb() is a Make Classes method that writes new classes. This method is implemented for the Class Make Table readyforwhatsnext S3 class.NA
#' @param x PARAM_DESCRIPTION
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: NULL
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @return NULL
#' @rdname make_classes.ready4_class_make_tb
#' @export 
#' @importFrom purrr pwalk
#' @importFrom dplyr filter
make_classes.ready4_class_make_tb <- function (x, name_pfx_1L_chr, output_dir_1L_chr, file_exists_cdn_1L_chr = NULL, 
    prototype_lup = NULL, nss_to_ignore_chr, req_pkgs_chr = NA_character_, 
    class_in_cache_cdn_1L_chr = "stop") 
{
    purrr::pwalk(x %>% dplyr::filter(make_s3_lgl == T), ~write_scripts_to_mk_r3_cls(name_stub_1L_chr = ..2, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        class_desc_1L_chr = ..10, parent_cls_nm_1L_chr = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, type_1L_chr = ..3[[1]], pt_chkr_pfx_1L_chr = ..4[[1]], 
        pt_ns_1L_chr = ifelse(..5[[1]] %in% c("base"), "", ..5[[1]]), 
        vals_chr = ..6, allowed_vals_ls = ..7, min_max_vals_dbl = ..8, 
        start_end_vals_dbl = ..9, file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
        prototype_lup = prototype_lup, nss_to_ignore_chr = nss_to_ignore_chr))
    purrr::pwalk(x %>% dplyr::filter(make_s3_lgl != T), ~write_scripts_to_mk_r4_cls(name_stub_1L_chr = ..2, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        class_desc_1L_chr = ..10, parent = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, slots_chr = ..12[[1]], type_chr = ..3[[1]], meaningful_nms_ls = ..13, 
        vals_ls = ..6[[1]], allowed_vals_ls = ..7[[1]], clss_to_inc_chr = ..14[[1]], 
        prototype_lup = prototype_lup, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr))
}
