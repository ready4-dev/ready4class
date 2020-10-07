#' Add class
#' @name add_class
#' @description add_class() is an Add Class generic that adds information about a class.
#' @param x An object
#' @param ... Additional arguments
#' @export

add_class <- function (x, ...)
{
    UseMethod("add_class", x)
}
methods::setGeneric("add_class")
#' Make lookup table
#' @name make_lup
#' @description make_lup() is a Make Lookup Table generic that applies a Make method and then updates the output of that method.
#' @param x An object
#' @param ... Additional arguments
#' @export

make_lup <- function (x, ...)
{
    UseMethod("make_lup", x)
}
methods::setGeneric("make_lup")
#' Order tibble
#' @name order_tb
#' @description order_tb() is an Order Tibble generic that orders a tibble.
#' @param x An object
#' @param ... Additional arguments
#' @export

order_tb <- function (x, ...)
{
    UseMethod("order_tb", x)
}
methods::setGeneric("order_tb")
#' Remake list columns
#' @name remake_ls_cols
#' @description remake_ls_cols() is a Remake List Columns generic that remakes list columns.
#' @param x An object
#' @param ... Additional arguments
#' @export

remake_ls_cols <- function (x, ...)
{
    UseMethod("remake_ls_cols", x)
}
methods::setGeneric("remake_ls_cols")
#' Update lookup table for namespace
#' @name update_lup_for_ns
#' @description update_lup_for_ns() is an Update Lookup Table for Namespace generic that updates a lookup table with namespace data.
#' @param x An object
#' @param attached_nss_chr Attached namespaces (a character vector)
#' @param ... Additional arguments
#' @export

update_lup_for_ns <- function (x, attached_nss_chr, ...)
{
    UseMethod("update_lup_for_ns", x)
}
methods::setGeneric("update_lup_for_ns")
#' Write classes
#' @name write_classes
#' @description write_classes() is a Write Classes generic that writes new classes.
#' @param x An object
#' @param ... Additional arguments
#' @export

write_classes <- function (x, ...)
{
    UseMethod("write_classes", x)
}
methods::setGeneric("write_classes")
#' Write classes and make lookup table
#' @name write_classes_and_make_lup
#' @description write_classes_and_make_lup() is a Write Classes and Make Lookup Table generic that makes new classes and creates or updates a class prototype lookup table.
#' @param x An object
#' @param ... Additional arguments
#' @export

write_classes_and_make_lup <- function (x, ...)
{
    UseMethod("write_classes_and_make_lup", x)
}
methods::setGeneric("write_classes_and_make_lup")
