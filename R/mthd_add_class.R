#' Add class method applied to readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE.
#' @description add_class.ready4_class_pt_lup() is an Add Class method that adds information about a class. This method is implemented for the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE. The function returns Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE).
#' @param x An instance of readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param row_idx_1L_int Row index (an integer vector of length one)
#' @param make_tb Make (a tibble)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Instance (a readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE)
#' @rdname add_class.ready4_class_pt_lup
#' @export 
#' @importFrom dplyr slice pull filter bind_rows
add_class.ready4_class_pt_lup <- function (x, row_idx_1L_int, make_tb, dev_pkg_ns_1L_chr, name_pfx_1L_chr, 
    output_dir_1L_chr, file_exists_cdn_1L_chr, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop", 
    abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4class", envir = environment())
    make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
    write_classes(make_tb, name_pfx_1L_chr = name_pfx_1L_chr, 
        output_dir_1L_chr = output_dir_1L_chr, file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
        prototype_lup = x, nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, 
            nss_to_ignore_chr), req_pkgs_chr = req_pkgs_chr, 
        class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
        abbreviations_lup = abbreviations_lup)
    new_pt_lup <- make_lup(make_tb, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
        prefix = name_pfx_1L_chr)
    classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
    inst_ready4_class_pt_lup <- x %>% dplyr::filter(!type_chr %in% 
        classes_to_add_chr) %>% dplyr::bind_rows(new_pt_lup)
    return(inst_ready4_class_pt_lup)
}
methods::setMethod("add_class", "ready4_class_pt_lup", add_class.ready4_class_pt_lup)
