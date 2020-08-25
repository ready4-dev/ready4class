#' Delete files
#' @description delete_files() is a Delete function that deletes a file from a specified location. Specifically, this function implements an algorithm to delete files.NA
#' @param dir_chr Directory (a character vector of length 1)
#' @param pattern_chr Pattern (a character vector of length 1)
#' @return NULL
#' @rdname delete_files
#' @export 

#' @keywords internal
delete_files <- function (dir_chr, pattern_chr) 
{
    if (!is.na(pattern_chr)) {
        files_chr_vec <- list.files(dir_chr, pattern = pattern_chr)
        if (!identical(files_chr_vec, character(0))) 
            paste0(dir_chr, "/", files_chr_vec) %>% file.remove()
    }
}
#' Delete getters setters
#' @description delete_getters_setters() is a Delete function that deletes a file from a specified location. Specifically, this function implements an algorithm to delete getters setters.NA
#' @param x PARAM_DESCRIPTION
#' @param output_dir PARAM_DESCRIPTION
#' @return NULL
#' @rdname delete_getters_setters
#' @export 
#' @importFrom dplyr pull
#' @importFrom purrr compact flatten flatten_chr reduce walk
#' @keywords internal
delete_getters_setters <- function (x, output_dir) 
{
    delete_vec <- x %>% dplyr::pull(class_slots) %>% purrr::compact() %>% 
        purrr::flatten() %>% purrr::flatten_chr()
    if (!identical(delete_vec, character(0))) 
        paste0(output_dir, "/gnrc_", purrr::reduce(delete_vec, 
            ~append(.x, .y[!.y %in% .x])), ".R") %>% purrr::walk(~if (file.exists(.x)) 
            file.remove(.x))
}
