#' Order tibble method applied to readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE.
#' @description order_tb.ready4_constructor_tbl() is an Order Tibble method that orders a tibble. This method is implemented for the readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE. The function is called for its side effects and does not return a value.
#' @param x An instance of readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @return Instance (a readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE)
#' @rdname order_tb-methods
#' @export 
#' @importFrom dplyr select mutate pull
#' @importFrom purrr map2 reduce
#' @importFrom stringr str_remove
order_tb.ready4_constructor_tbl <- function (x, name_pfx_1L_chr) 
{
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
    inst_ready4_constructor_tbl <- x[match(ordering_chr, x$name_stub_chr), 
        ]
    return(inst_ready4_constructor_tbl)
}
#' @rdname order_tb-methods
#' @aliases order_tb,ready4_constructor_tbl-method
methods::setMethod("order_tb", "ready4_constructor_tbl", order_tb.ready4_constructor_tbl)
