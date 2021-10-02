#' Renew method applied to ready4 S3 class Constructor Table..
#' @description renew.ready4class_constructor() is a Renew method that updates an instance of a class with new values. This method is implemented for the ready4 S3 class Constructor Table. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Constructor Table.
#' @param type_1L_chr Type (a character vector of length one), Default: 'listify'
#' @param name_pfx_1L_chr Name prefix (a character vector of length one), Default: NULL
#' @return NA ()
#' @rdname renew-methods
#' @export 
#' @importFrom dplyr mutate_at vars select mutate pull
#' @importFrom purrr map map2 reduce
#' @importFrom stringr str_remove
renew.ready4class_constructor <- function (x, type_1L_chr = "listify", name_pfx_1L_chr = NULL) 
{
    if (type_1L_chr == "listify") {
        x <- x %>% dplyr::mutate_at(dplyr::vars(pt_ls, pt_chkr_pfx_ls, 
            pt_ns_ls, slots_ls, inc_clss_ls), ~purrr::map(., 
            ~list(.x)))
    }
    if (type_1L_chr == "order") {
        ordering_tb <- x %>% dplyr::select(name_stub_chr, pt_ls, 
            parent_class_chr) %>% dplyr::mutate(class_name_chr = paste0(name_pfx_1L_chr, 
            name_stub_chr)) %>% dplyr::mutate(preceeded_by = purrr::map2(pt_ls, 
            parent_class_chr, ~if (is.na(.y)) {
                unlist(.x)[unlist(.x) %in% class_name_chr]
            }
            else {
                c(.y[.y %in% class_name_chr], unlist(.x)[unlist(.x) %in% 
                  class_name_chr])
            })) %>% dplyr::mutate(sequence = purrr::map2(preceeded_by, 
            class_name_chr, ~c(.x, .y)))
        ordering_chr <- purrr::reduce(ordering_tb %>% dplyr::pull(sequence), 
            ~append(.x, .y[!.y %in% .x])) %>% stringr::str_remove(name_pfx_1L_chr)
        x <- x[match(ordering_chr, x$name_stub_chr), ]
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4class_constructor-method
methods::setMethod("renew", methods::className("ready4class_constructor", package = "ready4class"), renew.ready4class_constructor)
#' Renew method applied to ready4 S3 class Prototype Lookup Table of class metadata..
#' @description renew.ready4class_pt_lup() is a Renew method that updates an instance of a class with new values. This method is implemented for the ready4 S3 class Prototype Lookup Table of class metadata. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Prototype Lookup Table of class metadata.
#' @param attached_nss_chr Attached namespaces (a character vector)
#' @param type_1L_chr Type (a character vector of length one), Default: 'namespace'
#' @return NA ()
#' @rdname renew-methods
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr pmap_chr
renew.ready4class_pt_lup <- function (x, attached_nss_chr, type_1L_chr = "namespace") 
{
    if (type_1L_chr == "namespace") {
        attached_nss_chr <- c("base", attached_nss_chr) %>% unique()
        x <- x %>% dplyr::mutate(val_chr = purrr::pmap_chr(dplyr::select(x, 
            pt_ns_chr, fn_to_call_chr, default_val_chr), ~make_alg_to_get_pt_val(pt_ns_1L_chr = ..1, 
            fn_to_call_1L_chr = ..2, default_val_1L_chr = ..3, 
            attached_nss_chr = attached_nss_chr)))
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4class_pt_lup-method
methods::setMethod("renew", methods::className("ready4class_pt_lup", package = "ready4class"), renew.ready4class_pt_lup)
