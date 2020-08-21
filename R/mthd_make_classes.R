#' Make classes method applied to Class Make Table readyforwhatsnext S3 class.
#' @description make_classes.ready4_class_make_tb() is a Make Classes method that writes new classes. This method is implemented for the Class Make Table readyforwhatsnext S3 class. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param name_prefix PARAM_DESCRIPTION
#' @param output_dir PARAM_DESCRIPTION
#' @param file_exists_logic PARAM_DESCRIPTION, Default: NULL
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @param ignore_ns_chr Ignore namespace (a character vector of length 1)
#' @param required_pckg_chr_vec Required pckg (a character vector), Default: 'NA'
#' @param class_in_cache_logic_chr Class in cache logic (a character vector of length 1), Default: 'stop'
#' @return NULL
#' @rdname make_classes.ready4_class_make_tb
#' @export 
#' @importFrom purrr pwalk
#' @importFrom dplyr filter
#' @keywords internal
make_classes.ready4_class_make_tb <- function (x, name_prefix, output_dir, file_exists_logic = NULL, 
    prototype_lup = NULL, ignore_ns_chr, required_pckg_chr_vec = NA_character_, 
    class_in_cache_logic_chr = "stop") 
{
    purrr::pwalk(x %>% dplyr::filter(make_s3 == T), ~make_ready_s3(name_stub = ..2, 
        name_prefix = name_prefix, output_folder = output_dir, 
        class_desc = ..10, parent = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, type = ..3[[1]], type_checker_prefix = ..4[[1]], type_namespace = ifelse(..5[[1]] %in% 
            c("base"), "", ..5[[1]]), values = ..6, allowed_values = ..7, 
        min_max_values = ..8, start_end_values = ..9, file_exists_logic = file_exists_logic, 
        prototype_lup = prototype_lup, ignore_ns_chr = ignore_ns_chr))
    purrr::pwalk(x %>% dplyr::filter(make_s3 != T), ~make_ready_s4(name_stub = ..2, 
        name_prefix = name_prefix, output_folder = output_dir, 
        class_desc = ..10, parent = if (is.na(..11)) {
            NULL
        }
        else {
            ..11
        }, class_slots = ..12[[1]], type = ..3[[1]], meaningful_names = ..13, 
        values = ..6[[1]], allowed_values = ..7[[1]], include_classes = ..14[[1]], 
        prototype_lup = prototype_lup, ignore_ns_chr = ignore_ns_chr, 
        required_pckg_chr_vec = required_pckg_chr_vec, class_in_cache_logic_chr = class_in_cache_logic_chr))
}
