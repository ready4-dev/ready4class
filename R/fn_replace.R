#' Replace NA in
#' @description replace_NA_in_fn() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to a replace NA in. Function argument fn_body_1L_chr specifies the object to be updated.The function returns a function body (a character vector of length one).
#' @param fn_body_1L_chr Function body (a character vector of length one)
#' @return Function body (a character vector of length one)
#' @rdname replace_NA_in_fn
#' @export 
#' @importFrom stringr str_replace_all
replace_NA_in_fn <- function (fn_body_1L_chr) 
{
    fn_body_1L_chr <- stringr::str_replace_all(fn_body_1L_chr, 
        "\"NA\"", "NA_character_")
    return(fn_body_1L_chr)
}
