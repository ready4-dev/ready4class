replace_NA_in_fn <- function(fn_body_1L_chr){
  fn_body_1L_chr <- stringr::str_replace_all(fn_body_1L_chr,"\"NA\"","NA_character_")
  return(fn_body_1L_chr)
}
