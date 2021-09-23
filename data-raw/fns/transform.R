transform_alg_to_ref_cls_nm <- function(alg_to_ref_cls_nm_1L_chr,
                                        pkg_nm_1L_chr){
  tfd_alg_to_ref_cls_nm <- alg_to_ref_cls_nm_1L_chr %>%
    stringr::str_replace("methods::className\\(",
                         "")%>%
    stringr::str_replace(paste0(",\"",pkg_nm_1L_chr,"\"\\)"),
                         "")
  return(tfd_alg_to_ref_cls_nm)
}
transform_class_ns <- function(class_ns_1L_chr,
                                   dev_pkg_ns_1L_chr){
  tfd_class_ns_1L_chr <- ifelse(class_ns_1L_chr %in% c("base",dev_pkg_ns_1L_chr),
         "",
         class_ns_1L_chr)
  return(tfd_class_ns_1L_chr)
}
# transform_cls_type_ls <- function(cls_type_ls){
#   max_lngth_1L_int <- purrr::map_int(cls_type_ls,
#                                      ~ length(.x)) %>%
#     max()
#   tfmd_cls_type_ls <- cls_type_ls %>% purrr::map(~{
#     c(.x,rep(NA_character_,max_lngth_1L_int - length(.x)))
#   })
#   return(tfmd_cls_type_ls)
# }
transform_fn_into_chr <- function(fn){
  fn_1L_chr <- deparse(fn) %>% paste0(collapse="\n")
  return(fn_1L_chr)
}
transform_parent_ns_ls <- function(parent_ns_ls){
  if(is.null(parent_ns_ls$untransformed_1L_chr)){
    tfd_parent_ns_ls <- parent_ns_ls$transformed_1L_chr
  }else{
    tfd_parent_ns_ls <- ifelse(parent_ns_ls$untransformed_1L_chr=="base",
           "base",
           parent_ns_ls$transformed_1L_chr)
  }
  return(tfd_parent_ns_ls)
}
transform_pt_ls_for_new_clss <- function(pts_for_new_clss_ls){
  s3_idx <- pts_for_new_clss_ls %>% purrr::map_lgl(~class(.x) == "test_new_s3_pars")
  s4_idx <- pts_for_new_clss_ls %>% purrr::map_lgl(~class(.x) == "test_new_s4_pars")
  tfd_pts_for_new_clss_ls <- list(s3_ls = pts_for_new_clss_ls %>% purrr::keep(s3_idx),
       s4_ls = pts_for_new_clss_ls %>% purrr::keep(s4_idx))
  return(tfd_pts_for_new_clss_ls)
}
