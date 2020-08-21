#' Remake list columns method applied to Class Make Table readyforwhatsnext S3 class.
#' @description remake_ls_cols.ready4_class_make_tb() is a Remake List Columns method that remakes list columns. This method is implemented for the Class Make Table readyforwhatsnext S3 class. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname remake_ls_cols.ready4_class_make_tb
#' @export 
#' @importFrom dplyr mutate_at vars
#' @importFrom purrr map
#' @keywords internal
remake_ls_cols.ready4_class_make_tb <- function (x) 
{
    x %>% dplyr::mutate_at(dplyr::vars(prototype, prototype_checker_prefix, 
        prototype_namespace, class_slots, include_classes), ~purrr::map(., 
        ~list(.x)))
}