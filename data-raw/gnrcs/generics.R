add_class <- function(x,
                      ...){
  UseMethod("add_class",x)
}
make_and_update <- function(x,
                            ...){
  UseMethod("make_and_update",x)
}
make_classes <- function(x,
                         ...){
  UseMethod("make_classes",x)
}
make_lup <- function(x,
                     ...){
  UseMethod("make_lup",x)
}
order_tb <- function(x,
                     ...){
  UseMethod("order_tb",x)
}
remake_ls_cols <- function(x,
                           ...){
  UseMethod("remake_ls_cols",x)
}
update_lup_for_ns <- function(x,
                              attached_nss_chr,
                              ...){
  UseMethod("update_lup_for_ns",x)
}
