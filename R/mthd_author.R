#' Author - a method that writes files to local or remote locations.
#' @description author.ready4class_constructor() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class Constructor Table. The function returns X (ready4 S3 class Prototype Lookup Table of class metadata.).
#' @param x An instance of ready4 S3 class Constructor Table.
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param name_pfx_1L_chr Name prefix (a character vector of length one), Default: paste0(ready4fun::get_dev_pkg_nm(), "_")
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'R'
#' @param delete_cdn_ptrn_chr Delete condition pattern (a character vector), Default: 'NA'
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: 'overwrite'
#' @param init_class_pt_lup Initial class prototype (a lookup table), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param fn_types_lup Function types (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @return X (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname author-methods
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom ready4 renew make_prompt author
#' @importFrom purrr walk map_chr reduce
#' @importFrom stringr str_sub
#' @importFrom Hmisc capitalize
#' @importFrom stringi stri_replace_last
author.ready4class_constructor <- function (x, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"), 
    output_dir_1L_chr = "R", delete_cdn_ptrn_chr = NA_character_, 
    file_exists_cdn_1L_chr = "overwrite", init_class_pt_lup = NULL, 
    nss_to_ignore_chr = NA_character_, req_pkgs_chr = NA_character_, 
    class_in_cache_cdn_1L_chr = "stop", abbreviations_lup, fn_types_lup, 
    object_type_lup) 
{
    if (is.null(init_class_pt_lup)) 
        init_class_pt_lup <- prototype_lup
    x <- ready4::renew(x, name_pfx_1L_chr = name_pfx_1L_chr, 
        type_1L_chr = "order")
    if (file_exists_cdn_1L_chr == "overwrite") {
        write_to_delete_gnrc_fn_fls(x, output_dir_1L_chr = output_dir_1L_chr)
        purrr::walk(delete_cdn_ptrn_chr, ~write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr, 
            pattern_1L_chr = .x))
    }
    new_files_chr <- paste0(purrr::map_chr(x$make_s3_lgl, ~ifelse(.x, 
        "C3_", "C4_")), purrr::map_chr(x$make_s3_lgl, ~ifelse(.x, 
        name_pfx_1L_chr, stringr::str_sub(name_pfx_1L_chr, end = -2) %>% 
            Hmisc::capitalize())), x$name_stub_chr, ".R")
    consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
        ifelse(length(new_files_chr) > 1, "s ", " "), new_files_chr %>% 
            paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
            " and"), " to the directory ", output_dir_1L_chr, 
        " ?"), options_chr = c("Y", "N"), force_from_opts_1L_chr = T)
    if (consent_1L_chr == "Y") {
        x_ready4class_pt_lup <- purrr::reduce(1:nrow(x), .init = init_class_pt_lup %>% 
            ready4::renew(dev_pkg_ns_1L_chr), ~author.ready4class_pt_lup(.x, 
            row_idx_1L_int = .y, make_tb = x, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            name_pfx_1L_chr = name_pfx_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
            file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
            nss_to_ignore_chr = nss_to_ignore_chr, req_pkgs_chr = req_pkgs_chr, 
            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
            abbreviations_lup = abbreviations_lup, fn_types_lup = fn_types_lup, 
            object_type_lup = object_type_lup, consent_1L_chr = consent_1L_chr))
    }
    else {
        x_ready4class_pt_lup <- NULL
    }
    return(x_ready4class_pt_lup)
}
#' @rdname author-methods
#' @aliases author,ready4class_constructor-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("ready4class_constructor", package = "ready4class"), author.ready4class_constructor)
#' Author - a method that writes files to local or remote locations.
#' @description author.ready4class_manifest() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class Manifest. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Manifest.
#' @param init_class_pt_lup Initial class prototype (a lookup table), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: T
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @return X (ready4 S3 class for encapsulating the metadata required for package set-up.)
#' @rdname author-methods
#' @export 
#' @importFrom ready4fun add_new_cls_pts make_pt_ready4fun_executor ready4fun_executor
#' @importFrom ready4 author
author.ready4class_manifest <- function (x, init_class_pt_lup = NULL, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    list_generics_1L_lgl = T, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_, self_serve_1L_lgl = F, self_serve_fn_ls = NULL) 
{
    if (is.null(init_class_pt_lup)) {
        if (is.null(x$x_ready4fun_manifest$subsequent_ls$prototype_lup)) {
            x$x_ready4fun_manifest <- ready4fun::add_new_cls_pts(x$x_ready4fun_manifest)
        }
        init_class_pt_lup <- x$x_ready4fun_manifest$subsequent_ls$prototype_lup
    }
    else {
        x$x_ready4fun_manifest$subsequent_ls$prototype_lup <- init_class_pt_lup
    }
    x$x_ready4fun_manifest$subsequent_ls$cls_fn_ls <- ready4fun::make_pt_ready4fun_executor(args_ls = list(x = x$constructor_r3, 
        dev_pkg_ns_1L_chr = x$x_ready4fun_manifest$initial_ls$pkg_desc_ls$Package, 
        name_pfx_1L_chr = paste0(x$x_ready4fun_manifest$initial_ls$pkg_desc_ls$Package, 
            "_"), output_dir_1L_chr = paste0(x$x_ready4fun_manifest$initial_ls$path_to_pkg_rt_1L_chr, 
            "/R"), delete_cdn_ptrn_chr = NA_character_, file_exists_cdn_1L_chr = "overwrite", 
        init_class_pt_lup = init_class_pt_lup, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, class_in_cache_cdn_1L_chr = "stop", 
        abbreviations_lup = x$x_ready4fun_manifest$subsequent_ls$abbreviations_lup, 
        fn_types_lup = x$x_ready4fun_manifest$subsequent_ls$fn_types_lup, 
        object_type_lup = x$x_ready4fun_manifest$subsequent_ls$object_type_lup), 
        fn = author.ready4class_constructor) %>% ready4fun::ready4fun_executor()
    x$x_ready4fun_manifest$subsequent_ls$s4_fns_ls$fn <- write_r4_mthds
    x$x_ready4fun_manifest$subsequent_ls$s4_fns_ls$args_ls <- list(fns_dir_1L_chr = paste0(x$x_ready4fun_manifest$initial_ls$path_to_pkg_rt_1L_chr, 
        "/data-raw/s4_fns"), fn_types_lup = x$x_ready4fun_manifest$subsequent_ls$fn_types_lup, 
        import_from_chr = x$x_ready4fun_manifest$subsequent_ls$import_from_chr, 
        output_dir_1L_chr = paste0(x$x_ready4fun_manifest$initial_ls$path_to_pkg_rt_1L_chr, 
            "/R"), pkg_nm_1L_chr = x$x_ready4fun_manifest$initial_ls$pkg_desc_ls$Package)
    x_ready4fun_manifest <- ready4::author(x$x_ready4fun_manifest, 
        key_1L_chr = key_1L_chr, list_generics_1L_lgl = list_generics_1L_lgl, 
        self_serve_1L_lgl = self_serve_1L_lgl, self_serve_fn_ls = self_serve_fn_ls)
    return(x_ready4fun_manifest)
}
#' @rdname author-methods
#' @aliases author,ready4class_manifest-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("ready4class_manifest", package = "ready4class"), author.ready4class_manifest)
#' Author - a method that writes files to local or remote locations.
#' @description author.ready4class_pt_lup() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class Prototype Lookup Table of class metadata. The function returns X (ready4 S3 class Prototype Lookup Table of class metadata.).
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
#' @param fn_types_lup Function types (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return X (ready4 S3 class Prototype Lookup Table of class metadata.)
#' @rdname author-methods
#' @export 
#' @importFrom dplyr slice pull filter bind_rows
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @importFrom Hmisc capitalize
#' @importFrom ready4 make_prompt author
#' @importFrom stringi stri_replace_last
author.ready4class_pt_lup <- function (x, row_idx_1L_int, make_tb, dev_pkg_ns_1L_chr, name_pfx_1L_chr, 
    output_dir_1L_chr, file_exists_cdn_1L_chr, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_, class_in_cache_cdn_1L_chr = "stop", 
    abbreviations_lup, fn_types_lup, object_type_lup, consent_1L_chr = NULL) 
{
    make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
    if (is.null(consent_1L_chr)) {
        new_files_chr <- paste0(purrr::map_chr(make_tb$make_s3_lgl, 
            ~ifelse(.x, "C3_", "C4_")), purrr::map_chr(make_tb$make_s3_lgl, 
            ~ifelse(.x, name_pfx_1L_chr, stringr::str_sub(name_pfx_1L_chr, 
                end = -2) %>% Hmisc::capitalize())), make_tb$name_stub_chr, 
            ".R")
        consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file", 
            ifelse(length(new_files_chr) > 1, "s ", " "), new_files_chr %>% 
                paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                " and"), " to the directory ", output_dir_1L_chr, 
            " ?"), options_chr = c("Y", "N"), force_from_opts_1L_chr = T)
    }
    if (consent_1L_chr == "Y") {
        authorClasses.ready4class_constructor(make_tb, name_pfx_1L_chr = name_pfx_1L_chr, 
            output_dir_1L_chr = output_dir_1L_chr, file_exists_cdn_1L_chr = file_exists_cdn_1L_chr, 
            prototype_lup = x, nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, 
                nss_to_ignore_chr), req_pkgs_chr = req_pkgs_chr, 
            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr, 
            abbreviations_lup = abbreviations_lup, fn_types_lup = fn_types_lup, 
            object_type_lup = object_type_lup, consent_1L_chr = consent_1L_chr)
        new_pt_lup <- manufacture(make_tb, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            prefix = name_pfx_1L_chr)
        classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
        x_ready4class_pt_lup <- x %>% dplyr::filter(!type_chr %in% 
            classes_to_add_chr) %>% dplyr::bind_rows(new_pt_lup)
    }
    else {
        x_ready4class_pt_lup <- NULL
    }
    return(x_ready4class_pt_lup)
}
#' @rdname author-methods
#' @aliases author,ready4class_pt_lup-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("ready4class_pt_lup", package = "ready4class"), author.ready4class_pt_lup)
