update_pt_fn_args_ls <- function(args_ls){
  arg_lgths_dbl <- args_ls %>% purrr::map_dbl(~length(.x))
  arg_max_lgth_1L_dbl <- max(arg_lgths_dbl)
  updated_args_ls <- purrr::map2(args_ls %>% unname(),
                         unname(arg_lgths_dbl==0 & arg_lgths_dbl != arg_max_lgth_1L_dbl),
                         ~{
                           val_xx <- .x
                           if(.y){
                             val_xx <- parse(text=paste0(
                               ifelse(is.character(val_xx),
                                      "NA_character_",
                                      ifelse(is.integer(val_xx),
                                             "NA_integer_",
                                             ifelse(is.complex(val_xx),
                                                    "NA_complex_",
                                                    ifelse(is.numeric(val_xx),
                                                           "NA_real_",
                                                           ifelse(is.logical(val_xx),
                                                                  "NA",
                                                                  "list(NULL)"))))))) %>% eval()
                           }
                           val_xx
                         }) %>%
    stats::setNames(names(args_ls))
  return(updated_args_ls)
}
