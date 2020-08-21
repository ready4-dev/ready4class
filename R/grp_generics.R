#' Add class
#' @description add_class() is an Add Class generic that adds information about a class. The function is called for its side effects and does not return a value.
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
#' Make and update
#' @description make_and_update() is a Make and Update generic that applies a Make method and then updates the output of that method. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_and_update
#' @export 

#' @keywords internal
make_and_update <- function (x, ...) 
{
    UseMethod("make_and_update", x)
}
#' Make classes
#' @description make_classes() is a Make Classes generic that writes new classes. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_classes
#' @export 

#' @keywords internal
make_classes <- function (x, ...) 
{
    UseMethod("make_classes", x)
}
#' Make lookup table
#' @description make_lup() is a Make Lookup Table generic that makes a lookup table. The function is called for its side effects and does not return a value.
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
#' @description order_tb() is an Order Tibble generic that orders a tibble. The function is called for its side effects and does not return a value.
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
#' @description remake_ls_cols() is a Remake List Columns generic that remakes list columns. The function is called for its side effects and does not return a value.
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
#' @description update_lup_for_ns() is an Update Lookup Table for Namespace generic that updates a lookup table with namespace data. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param namespace_contexts PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_lup_for_ns
#' @export 

#' @keywords internal
update_lup_for_ns <- function (x, namespace_contexts, ...) 
{
    UseMethod("update_lup_for_ns", x)
}
