transform_new_classes_ls <- function(new_classes_ls){
  s3_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s3_pars")
  s4_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s4_pars")
  list(s3_ls = new_classes_ls %>% purrr::keep(s3_idx),
       s4_ls = new_classes_ls %>% purrr::keep(s4_idx))
}
transform_class_ns_chr <- function(class_ns_chr,
                                   dev_pckg_ns_chr){
  ifelse(class_ns_chr %in% c("base",dev_pckg_ns_chr),
         "",
         class_ns_chr)
}
