remake_ls_cols.ready4class_constructor_tbl <- function(x){
  inst_ready4class_constructor_tbl <- x %>%
    dplyr::mutate_at(dplyr::vars(pt_ls,
                                 pt_chkr_pfx_ls,
                                 pt_ns_ls,
                                 slots_ls,
                                 inc_clss_ls),
                     ~ purrr::map(., ~ list(.x)))
  return(inst_ready4class_constructor_tbl)
}
