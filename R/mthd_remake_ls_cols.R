#' Remake list columns method applied to CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class.
#' @description remake_ls_cols.ready4_constructor_tbl() is a Remake List Columns method that remakes list columns. This method is implemented for the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class.NA
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname remake_ls_cols.ready4_constructor_tbl
#' @export 
#' @importFrom dplyr mutate_at vars
#' @importFrom purrr map
#' @keywords internal
remake_ls_cols.ready4_constructor_tbl <- function (x) 
{
    inst_of_ready4_constructor_tbl <- x %>% dplyr::mutate_at(dplyr::vars(pt_ls, 
        pt_chkr_pfx_ls, pt_ns_ls, slots_ls, inc_clss_ls), ~purrr::map(., 
        ~list(.x)))
    return(inst_of_ready4_constructor_tbl)
}
