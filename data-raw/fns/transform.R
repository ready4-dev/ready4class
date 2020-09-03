transform_alg_to_ref_cls_nm <- function(st_class_fn_chr,
                                        package_chr){
  st_class_fn_chr %>%
    stringr::str_replace("methods::className\\(",
                         "")%>%
    stringr::str_replace(paste0(",\"",package_chr,"\"\\)"),
                         "")
}
transform_class_ns <- function(class_ns_chr,
                                   dev_pckg_ns_chr){
  ifelse(class_ns_chr %in% c("base",dev_pckg_ns_chr),
         "",
         class_ns_chr)
}
transform_fn_into_chr <- function(fn){
  deparse(fn) %>% paste0(collapse="\n")
}
transform_parent_ns_ls <- function(parent_ns_ls){
  if(is.null(parent_ns_ls$untransformed_chr)){
    parent_ns_ls$transformed_chr
  }else{
    ifelse(parent_ns_ls$untransformed_chr=="base",
           "base",
           parent_ns_ls$transformed_chr)
  }

}
transform_pt_ls_for_new_clss <- function(new_classes_ls){
  s3_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s3_pars")
  s4_idx <- new_classes_ls %>% purrr::map_lgl(~class(.x) == "test_new_s4_pars")
  list(s3_ls = new_classes_ls %>% purrr::keep(s3_idx),
       s4_ls = new_classes_ls %>% purrr::keep(s4_idx))
}
