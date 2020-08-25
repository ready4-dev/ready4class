#' Resolve parent namespace
#' @description resolve_parent_ns_chr() is a Resolve function that resolves inconsistencies between two or more objects. Specifically, this function implements an algorithm to resolve parent a namespace.NA
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname resolve_parent_ns_chr
#' @export 

#' @keywords internal
resolve_parent_ns_chr <- function (parent_ns_ls) 
{
    if (is.null(parent_ns_ls$untransformed_chr)) {
        parent_ns_ls$transformed_chr
    }
    else {
        ifelse(parent_ns_ls$untransformed_chr == "base", "base", 
            parent_ns_ls$transformed_chr)
    }
}
