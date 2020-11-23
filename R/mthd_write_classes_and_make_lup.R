#' Write classes and make lookup table method applied to readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE.
#' @description write_classes_and_make_lup.ready4_constructor_tbl() is a Write Classes and Make Lookup Table method that makes new classes and creates or updates a class prototype lookup table. This method is implemented for the readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE. The function returns Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE).
#' @param x An instance of readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param name_pfx_1L_chr Name prefix (a character vector of length one), Default: paste0(ready4fun::get_dev_pkg_nm(), "_")
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'R'
#' @param delete_cdn_ptrn_chr Delete condition pattern (a character vector), Default: 'NA'
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: 'overwrite'
#' @param init_class_pt_lup Init class prototype (a lookup table), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE)
#' @rdname write_classes_and_make_lup.ready4_constructor_tbl
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom utils data
#' @importFrom purrr walk reduce
write_classes_and_make_lup.ready4_constructor_tbl <- function (x, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"), 
    output_dir_1L_chr = "R", delete_cdn_ptrn_chr = NA_character_, 
    file_exists_cdn_1L_chr = "overwrite", init_class_pt_lup = NULL, 
    nss_to_ignore_chr = NA_character_, req_pkgs_chr = NA_character_, 
    class_in_cache_cdn_1L_chr = "stop", abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4class", 
            envir = environment())
    if (is.null(init_class_pt_lup)) 
        init_class_pt_lup <- prototype_lup
    x <- order_tb(x, name_pfx_1L_chr)
    if (file_exists_cdn_1L_chr == "overwrite") {
        write_to_delete_gnrc_fn_fls(x, output_dir_1L_chr = output_dir_1L_chr)
        purrr::walk(delete_cdn_ptrn_chr, ~write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr, 
            pattern_1L_chr = .x))
    }
    inst_ready4_class_pt_lup <- purrr::reduce(1:nrow(x), .init = init_class_pt_lup %>% 
        update_lup_for_ns(dev_pkg_ns_1L_chr), ~add_class(.x, 
        row_idx_1L_int = .y, make_tb = x, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
        abbreviations_lup = abbreviations_lup))
    return(inst_ready4_class_pt_lup)
}
methods::setMethod("write_classes_and_make_lup", "ready4_constructor_tbl", write_classes_and_make_lup.ready4_constructor_tbl)
