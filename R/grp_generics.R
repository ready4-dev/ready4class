#' Add class
#' @description add_class() is an Add Class generic that adds information about a class.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname add_class
#' @export 

#' @keywords internal
add_class <- function (x, ...) 
{
    UseMethod("add_class", x)
}
#' Make lookup table
#' @description make_lup() is a Make Lookup Table generic that makes a lookup table.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_lup
#' @export 

#' @keywords internal
make_lup <- function (x, ...) 
{
    UseMethod("make_lup", x)
}
#' Order tibble
#' @description order_tb() is an Order Tibble generic that orders a tibble.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname order_tb
#' @export 

#' @keywords internal
order_tb <- function (x, ...) 
{
    UseMethod("order_tb", x)
}
#' Remake list columns
#' @description remake_ls_cols() is a Remake List Columns generic that remakes list columns.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname remake_ls_cols
#' @export 

#' @keywords internal
remake_ls_cols <- function (x, ...) 
{
    UseMethod("remake_ls_cols", x)
}
#' Update lookup table for namespace
#' @description update_lup_for_ns() is an Update Lookup Table for Namespace generic that updates a lookup table with namespace data.NA
#' @param x PARAM_DESCRIPTION
#' @param attached_nss_chr Attached namespaces (a character vector)
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_lup_for_ns
#' @export 

#' @keywords internal
update_lup_for_ns <- function (x, attached_nss_chr, ...) 
{
    UseMethod("update_lup_for_ns", x)
}
