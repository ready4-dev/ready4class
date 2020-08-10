order_tb <- function(x,
                     ...){
  UseMethod("order_tb",x)
}

order_tb.ready4_class_make_tb <- function(x,
                                          name_prefix){
  ordering_tb <- x %>%
    dplyr::select(name_stub,prototype,parent_class) %>%
    dplyr::mutate(class_name = paste0(name_prefix,name_stub)) %>%
    dplyr::mutate(preceeded_by = purrr::map2(prototype,parent_class,
                                             ~ if(is.na(.y)){
                                               unlist(.x)[unlist(.x) %in% class_name]
                                             }else{
                                               c(.y[.y %in% class_name],unlist(.x)[unlist(.x) %in% class_name])
                                             })) %>%
    dplyr::mutate(sequence = purrr::map2(preceeded_by,
                                         class_name,
                                         ~ c(.x,.y)))
  ordering_vec <- purrr::reduce(ordering_tb %>%
                                  dplyr::pull(sequence),
                                ~ append(.x,.y[!.y %in% .x])) %>%
    stringr::str_remove(name_prefix)

  x[match(ordering_vec, x$name_stub),]
}
