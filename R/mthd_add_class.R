#' Add class method applied to Class Prototype Lookup Table readyforwhatsnext S3 class.
#' @description add_class.ready4_class_pt_lup() is an Add Class method that adds information about a class. This method is implemented for the Class Prototype Lookup Table readyforwhatsnext S3 class.The function returns inst of ready4 class prototype (a lookup table).
#' @param x PARAM_DESCRIPTION
#' @param tb_row_idx PARAM_DESCRIPTION
#' @param make_tb Make (a tibble)
#' @param dev_pckg_namespace PARAM_DESCRIPTION
#' @param name_prefix PARAM_DESCRIPTION
#' @param output_dir PARAM_DESCRIPTION
#' @param file_exists_logic PARAM_DESCRIPTION
#' @param ignore_ns_chr Ignore namespace (a character vector), Default: 'NA'
#' @param required_pckg_chr_vec PARAM_DESCRIPTION, Default: 'NA'
#' @param class_in_cache_logic_chr Class in cache logic (a character vector), Default: 'stop'
#' @return Inst of ready4 class prototype (a lookup table)
#' @rdname add_class.ready4_class_pt_lup
#' @export 
#' @importFrom dplyr slice pull filter bind_rows
add_class.ready4_class_pt_lup <- function (x, tb_row_idx, make_tb, dev_pckg_namespace, name_prefix, 
    output_dir, file_exists_logic, ignore_ns_chr = NA_character_, 
    required_pckg_chr_vec = NA_character_, class_in_cache_logic_chr = "stop") 
{
    make_tb <- make_tb %>% dplyr::slice(tb_row_idx)
    make_classes(make_tb, name_prefix = name_prefix, output_dir = output_dir, 
        file_exists_logic = file_exists_logic, prototype_lup = x, 
        ignore_ns_chr = c(dev_pckg_namespace, ignore_ns_chr), 
        required_pckg_chr_vec = required_pckg_chr_vec, class_in_cache_logic_chr = class_in_cache_logic_chr)
    new_pt_lup <- make_lup(make_tb, dev_pckg_namespace = dev_pckg_namespace, 
        prefix = name_prefix)
    classes_to_add_vec <- new_pt_lup %>% dplyr::pull(type)
    inst_of_ready4_class_pt_lup <- x %>% dplyr::filter(!type %in% 
        classes_to_add_vec) %>% dplyr::bind_rows(new_pt_lup)
    return(inst_of_ready4_class_pt_lup)
}
