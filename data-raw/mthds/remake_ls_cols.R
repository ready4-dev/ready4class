remake_ls_cols.ready4_class_make_tb <- function(x){
  x %>%
    dplyr::mutate_at(dplyr::vars(prototype,
                                 pt_chk_pfx_ls,
                                 pt_ns_ls,
                                 class_slots,
                                 include_classes),
                     ~ purrr::map(., ~ list(.x)))
}
