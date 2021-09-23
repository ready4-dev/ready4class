#' Remake list columns method applied to ready4 S3 class Constructor Table of metadata required to make new classes..
#' @description remake_ls_cols.ready4class_constructor_tbl() is a Remake List Columns method that remakes list columns. This method is implemented for the ready4 S3 class Constructor Table of metadata required to make new classes. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Constructor Table of metadata required to make new classes.
#' @return Instance (ready4 S3 class Constructor Table of metadata required to make new classes.)
#' @rdname remake_ls_cols-methods
#' @export 
#' @importFrom dplyr mutate_at vars
#' @importFrom purrr map
remake_ls_cols.ready4class_constructor_tbl <- function (x) 
{
    inst_ready4class_constructor_tbl <- x %>% dplyr::mutate_at(dplyr::vars(pt_ls, 
        pt_chkr_pfx_ls, pt_ns_ls, slots_ls, inc_clss_ls), ~purrr::map(., 
        ~list(.x)))
    return(inst_ready4class_constructor_tbl)
}
#' @rdname remake_ls_cols-methods
#' @aliases remake_ls_cols,ready4class_constructor_tbl-method
methods::setMethod("remake_ls_cols", "ready4class_constructor_tbl", remake_ls_cols.ready4class_constructor_tbl)
