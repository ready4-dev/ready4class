add_class <- function(x,
                      ...){
  UseMethod("add_class",x)
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
write_classes <- function(x,
                          ...){
  UseMethod("write_classes",x)
}
write_classes_and_make_lup <- function(x,
                                       ...){
  UseMethod("write_classes_and_make_lup",x)
}

