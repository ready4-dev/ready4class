#' Make and update method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_and_update.ready4_class_make_tb() is a Make and Update method that applies a Make method and then updates the output of that method. This method is implemented for the Class Make Table readyforwhatsnext S3 class.NA
#' @param x PARAM_DESCRIPTION
#' @param dev_pckg_namespace PARAM_DESCRIPTION, Default: ready4fun::get_dev_pkg_nm_1L_chr()
#' @param name_prefix PARAM_DESCRIPTION, Default: paste0(ready4fun::get_dev_pkg_nm_1L_chr(), "_")
#' @param output_dir PARAM_DESCRIPTION, Default: 'R'
#' @param delete_files_pattern_chr_vec Delete files pattern (a character vector), Default: 'NA'
#' @param file_exists_logic PARAM_DESCRIPTION, Default: 'overwrite'
#' @param init_class_pt_lup Init class prototype (a lookup table), Default: NULL
#' @param ignore_ns_chr Ignore namespace (a character vector of length 1), Default: 'NA'
#' @param required_pckg_chr_vec Required pckg (a character vector), Default: 'NA'
#' @param class_in_cache_logic_chr Class in cache logic (a character vector of length 1), Default: 'stop'
#' @return NULL
#' @rdname make_and_update.ready4_class_make_tb
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm_1L_chr
#' @importFrom purrr walk reduce
#' @keywords internal
make_and_update.ready4_class_make_tb <- function (x, dev_pckg_namespace = ready4fun::get_dev_pkg_nm_1L_chr(), 
    name_prefix = paste0(ready4fun::get_dev_pkg_nm_1L_chr(), 
        "_"), output_dir = "R", delete_files_pattern_chr_vec = NA_character_, 
    file_exists_logic = "overwrite", init_class_pt_lup = NULL, 
    ignore_ns_chr = NA_character_, required_pckg_chr_vec = NA_character_, 
    class_in_cache_logic_chr = "stop") 
{
    if (is.null(init_class_pt_lup)) 
        init_class_pt_lup <- prototype_lup
    x <- order_tb(x, name_prefix)
    if (file_exists_logic == "overwrite") {
        delete_getters_setters(x, output_dir = output_dir)
        purrr::walk(delete_files_pattern_chr_vec, ~delete_files(dir_chr = output_dir, 
            pattern_chr = .x))
    }
    purrr::reduce(1:nrow(x), .init = init_class_pt_lup %>% update_lup_for_ns(dev_pckg_namespace), 
        ~add_class(.x, tb_row_idx = .y, make_tb = x, dev_pckg_namespace = dev_pckg_namespace, 
            name_prefix = name_prefix, output_dir = output_dir, 
            file_exists_logic = file_exists_logic, ignore_ns_chr = ignore_ns_chr, 
            required_pckg_chr_vec = required_pckg_chr_vec, class_in_cache_logic_chr = class_in_cache_logic_chr))
}
