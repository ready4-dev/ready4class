replace_NA_in_fn_chr <- function(fn_chr){
  stringr::str_replace_all(fn_chr,"\"NA\"","NA_character_")
}
