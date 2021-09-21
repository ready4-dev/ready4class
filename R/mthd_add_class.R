#' Add class method applied to ready4 S3 class Prototype Lookup Table of class metadata..
#' @description add_class.ready4class_pt_lup() is an Add Class method that adds information about a class. This method is implemented for the ready4 S3 class Prototype Lookup Table of class metadata.. The function returns Instance (ready4 S3 class Prototype Lookup Table of class metadata.).
#' @param x An instance of ready4 S3 class Prototype Lookup Table of class metadata.
#' @param row_idx_1L_int Row index (an integer vector of length one)
#' @param make_tb Make (a tibble)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one)
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return Instance (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname add_class-methods
#' @export 
#' @importFrom dplyr slice pull filter bind_rows
#' @importFrom purrr map_chr
#' @importFrom ready4fun make_prompt
#' @importFrom stringi stri_replace_last
add_class.ready4class_pt_lup <- function (x, row_idx_1L_int, make_tb, dev_pkg_ns_1L_chr, name_pfx_1L_chr, 
    output_dir_1L_chr, file_exists_cdn_1L_chr, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop", 
    abbreviations_lup, object_type_lup, consent_1L_chr = NULL) 
{
    make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
    if (is.null(consent_1L_chr)) {
        new_files_chr <- paste0(purrr::map_chr(make_tb$make_s3_lgl, 
            ~ifelse(.x, "C3_", "C4_")), name_pfx_1L_chr, make_tb$name_stub_chr, 
            ".R")
        consent_1L_chr <- ready4fun::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
            ifelse(length(new_files_chr) > 1, "s ", " "), new_files_chr %>% 
                paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                " and"), " to the directory ", output_dir_1L_chr, 
            " ?"), options_chr = c("Y", "N"), force_from_opts_1L_chr = T)
    }
    if (consent_1L_chr == "Y") {
        write_classes(make_tb, name_pfx_1L_chr = name_pfx_1L_chr, 
            output_dir_1L_chr = output_dir_1L_chr, file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
            prototype_lup = x, nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, 
                nss_to_ignore_chr), req_pkgs_chr = req_pkgs_chr, 
            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
            abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup, 
            consent_1L_chr = consent_1L_chr)
        new_pt_lup <- make_lup(make_tb, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            prefix = name_pfx_1L_chr)
        classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
        inst_ready4class_pt_lup <- x %>% dplyr::filter(!type_chr %in% 
            classes_to_add_chr) %>% dplyr::bind_rows(new_pt_lup)
    }
    else {
        inst_ready4class_pt_lup <- NULL
    }
    return(inst_ready4class_pt_lup)
}
#' @rdname add_class-methods
#' @aliases add_class,ready4class_pt_lup-method
methods::setMethod("add_class", "ready4class_pt_lup", add_class.ready4class_pt_lup)
