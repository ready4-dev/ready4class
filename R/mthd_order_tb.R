#' Order tibble method applied to Class Make Table readyforwhatsnext S3 class.
#' @description order_tb.ready4_class_make_tb() is an Order Tibble method that orders a tibble. This method is implemented for the Class Make Table readyforwhatsnext S3 class. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param name_prefix PARAM_DESCRIPTION
#' @return NULL
#' @rdname order_tb.ready4_class_make_tb
#' @export 
#' @importFrom dplyr select mutate pull
#' @importFrom purrr map2 reduce
#' @importFrom stringr str_remove
#' @keywords internal
order_tb.ready4_class_make_tb <- function (x, name_prefix) 
{
    ordering_tb <- x %>% dplyr::select(name_stub, prototype, 
        parent_class) %>% dplyr::mutate(class_name = paste0(name_prefix, 
        name_stub)) %>% dplyr::mutate(preceeded_by = purrr::map2(prototype, 
        parent_class, ~if (is.na(.y)) {
            unlist(.x)[unlist(.x) %in% class_name]
        }
        else {
            c(.y[.y %in% class_name], unlist(.x)[unlist(.x) %in% 
                class_name])
        })) %>% dplyr::mutate(sequence = purrr::map2(preceeded_by, 
        class_name, ~c(.x, .y)))
    ordering_vec <- purrr::reduce(ordering_tb %>% dplyr::pull(sequence), 
        ~append(.x, .y[!.y %in% .x])) %>% stringr::str_remove(name_prefix)
    x[match(ordering_vec, x$name_stub), ]
}
