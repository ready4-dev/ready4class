#' Remake list columns method applied to Class Make Table readyforwhatsnext S3 class.
#' @description remake_ls_cols.ready4_class_make_tb() is a Remake List Columns method that remakes list columns. This method is implemented for the Class Make Table readyforwhatsnext S3 class.The function returns inst of ready4 class make (a tibble).
#' @param x PARAM_DESCRIPTION
#' @return Inst of ready4 class make (a tibble)
#' @rdname remake_ls_cols.ready4_class_make_tb
#' @export 
#' @importFrom dplyr mutate_at vars
#' @importFrom purrr map
#' @keywords internal
remake_ls_cols.ready4_class_make_tb <- function (x) 
{
    inst_of_ready4_class_make_tb <- x %>% dplyr::mutate_at(dplyr::vars(pt_ls, 
        pt_chkr_pfx_ls, pt_ns_ls, slots_ls, inc_clss_ls), ~purrr::map(., 
        ~list(.x)))
    return(inst_of_ready4_class_make_tb)
}
