#' Remove class from cache
#' @description remove_class_from_cache() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove a class from cache. Function argument class_name_chr specifies the object to be updated. Argument class_in_cache_logic_chr provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param class_name_chr Class name (a character vector)
#' @param class_in_cache_logic_chr Class in cache logic (a character vector), Default: 'stop'
#' @return NULL
#' @rdname remove_class_from_cache
#' @export 
#' @importFrom methods isClass removeClass
remove_class_from_cache <- function (class_name_chr, class_in_cache_logic_chr = "stop") 
{
    keep_going_lgl <- methods::isClass(class_name_chr)
    if (class_in_cache_logic_chr == "stop" & keep_going_lgl) {
        stop(paste0("A class of name \"", class_name_chr, "\" is currently in memory. You may wish to confirm that you want to create a class of this name. To do so, rerun with the class_in_cache_locig_chr argument set to 'overwrite'"))
    }
    if (class_in_cache_logic_chr == "overwrite") {
        a <- 1
        while (keep_going_lgl) {
            a <- a + 1
            print(1)
            keep_going_lgl <- methods::removeClass(class_name_chr)
        }
    }
}