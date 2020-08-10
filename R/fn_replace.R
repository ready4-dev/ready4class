#' Replace NA in function
#' @description replace_NA_in_fn_chr() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to replace NA in a function. Function argument fn_chr specifies the object to be updated. Argument NA provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param fn_chr Function (a character vector of length 1)
#' @return NULL
#' @rdname replace_NA_in_fn_chr
#' @export 
#' @importFrom stringr str_replace_all
#' @keywords internal
replace_NA_in_fn_chr <- function (fn_chr) 
{
    stringr::str_replace_all(fn_chr, "\"NA\"", "NA_character_")
}
