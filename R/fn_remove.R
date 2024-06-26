#' Remove class from cache
#' @description remove_class_from_cache() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove class from cache. The function is called for its side effects and does not return a value.
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @return No return value, called for side effects.
#' @rdname remove_class_from_cache
#' @export 
#' @importFrom methods isClass removeClass
#' @keywords internal
remove_class_from_cache <- function (class_nm_1L_chr, class_in_cache_cdn_1L_chr = "stop") 
{
    keep_going_1L_lgl <- methods::isClass(class_nm_1L_chr)
    if (class_in_cache_cdn_1L_chr == "stop" & keep_going_1L_lgl) {
        stop(paste0("A class of name \"", class_nm_1L_chr, "\" is currently in memory. You may wish to confirm that you want to create a class of this name. To do so, rerun with the class_in_cache_locig_chr argument set to 'overwrite'"))
    }
    if (class_in_cache_cdn_1L_chr == "overwrite") {
        a <- 1
        while (keep_going_1L_lgl) {
            a <- a + 1
            print(1)
            keep_going_1L_lgl <- methods::removeClass(class_nm_1L_chr)
        }
    }
}
