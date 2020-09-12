#' Remake list columns method applied to readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE.
#' @description remake_ls_cols.ready4_constructor_tbl() is a Remake List Columns method that remakes list columns. This method is implemented for the readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE. The function is called for its side effects and does not return a value.
#' @param x An instance of readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE
#' @return Instance (a readyforwhatsnext S3 class CLASS CONSTRUCTOR TABLE)
#' @rdname remake_ls_cols.ready4_constructor_tbl
#' @export 
#' @importFrom dplyr mutate_at vars
#' @importFrom purrr map
remake_ls_cols.ready4_constructor_tbl <- function (x) 
{
    inst_ready4_constructor_tbl <- x %>% dplyr::mutate_at(dplyr::vars(pt_ls, 
        pt_chkr_pfx_ls, pt_ns_ls, slots_ls, inc_clss_ls), ~purrr::map(., 
        ~list(.x)))
    return(inst_ready4_constructor_tbl)
}
