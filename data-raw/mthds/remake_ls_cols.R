remake_ls_cols.ready4_class_make_tb <- function(x){
  x %>%
    dplyr::mutate_at(dplyr::vars(pt_ls,
                                 pt_chkr_pfx_ls,
                                 pt_ns_ls,
                                 slots_ls,
                                 inc_clss_ls),
                     ~ purrr::map(., ~ list(.x)))
}
