remake_ls_cols.ready4_class_make_tb <- function(x){
  x %>%
    dplyr::mutate_at(dplyr::vars(prototype,
                                 prototype_checker_prefix,
                                 prototype_namespace,
                                 class_slots,
                                 include_classes),
                     ~ purrr::map(., ~ list(.x)))
}
