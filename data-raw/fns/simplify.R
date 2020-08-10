simplify_class_name <- function(st_class_fn_chr,
                                package_chr){
  st_class_fn_chr %>%
    stringr::str_replace("methods::className\\(",
                         "")%>%
    stringr::str_replace(paste0(",\"",package_chr,"\"\\)"),
                         "")
}
