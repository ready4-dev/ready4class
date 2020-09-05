#' Add class method applied to Class Prototype Lookup Table readyforwhatsnext S3 class.
#' @description add_class.ready4_class_pt_lup() is an Add Class method that adds information about a class. This method is implemented for the Class Prototype Lookup Table readyforwhatsnext S3 class.The function returns inst of ready4 class prototype (a lookup table).
#' @param x PARAM_DESCRIPTION
#' @param row_idx_1L_int Row idx (an integer vector of length one)
#' @param make_tb Make (a tibble)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @return Inst of ready4 class prototype (a lookup table)
#' @rdname add_class.ready4_class_pt_lup
#' @export 
#' @importFrom dplyr slice pull filter bind_rows
add_class.ready4_class_pt_lup <- function (x, row_idx_1L_int, make_tb, dev_pkg_ns_1L_chr, name_pfx_1L_chr, 
    output_dir_1L_chr, file_exists_cdn_1L_chr, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop") 
{
    make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
    write_classes(make_tb, name_pfx_1L_chr = name_pfx_1L_chr, 
        output_dir_1L_chr = output_dir_1L_chr, file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
        prototype_lup = x, nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, 
            nss_to_ignore_chr), req_pkgs_chr = req_pkgs_chr, 
        class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr)
    new_pt_lup <- make_lup(make_tb, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
        prefix = name_pfx_1L_chr)
    classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
    inst_of_ready4_class_pt_lup <- x %>% dplyr::filter(!type_chr %in% 
        classes_to_add_chr) %>% dplyr::bind_rows(new_pt_lup)
    return(inst_of_ready4_class_pt_lup)
}
