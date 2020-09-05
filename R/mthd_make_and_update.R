#' Make and update method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_and_update.ready4_class_make_tb() is a Make and Update method that applies a Make method and then updates the output of that method. This method is implemented for the Class Make Table readyforwhatsnext S3 class.The function returns inst of ready4 class prototype (a lookup table).
#' @param x PARAM_DESCRIPTION
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param name_pfx_1L_chr Name prefix (a character vector of length one), Default: paste0(ready4fun::get_dev_pkg_nm(), "_")
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'R'
#' @param delete_cdn_ptrn_chr Delete condition pattern (a character vector), Default: 'NA'
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: 'overwrite'
#' @param init_class_pt_lup Init class prototype (a lookup table), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Req packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @return Inst of ready4 class prototype (a lookup table)
#' @rdname make_and_update.ready4_class_make_tb
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom purrr walk reduce
make_and_update.ready4_class_make_tb <- function (x, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"), 
    output_dir_1L_chr = "R", delete_cdn_ptrn_chr = NA_character_, 
    file_exists_cdn_1L_chr = "overwrite", init_class_pt_lup = NULL, 
    nss_to_ignore_chr = NA_character_, req_pkgs_chr = NA_character_, 
    class_in_cache_cdn_1L_chr = "stop") 
{
    if (is.null(init_class_pt_lup)) 
        init_class_pt_lup <- prototype_lup
    x <- order_tb(x, name_pfx_1L_chr)
    if (file_exists_cdn_1L_chr == "overwrite") {
        write_to_delete_gnrc_fn_fls(x, output_dir_1L_chr = output_dir_1L_chr)
        purrr::walk(delete_cdn_ptrn_chr, ~write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr, 
            pattern_1L_chr = .x))
    }
    inst_of_ready4_class_pt_lup <- purrr::reduce(1:nrow(x), .init = init_class_pt_lup %>% 
        update_lup_for_ns(dev_pkg_ns_1L_chr), ~add_class(.x, 
        row_idx_1L_int = .y, make_tb = x, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
        name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr))
    return(inst_of_ready4_class_pt_lup)
}
