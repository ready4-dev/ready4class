get_class_fl_nms <- function(class_names_chr,
                             s3_1L_lgl = T,
                             output_dir_1L_chr = NA){
  class_fl_nms_chr <- paste0(ifelse(is.na(output_dir_1L_chr),
                "",
                paste0(output_dir_1L_chr,"/")),
         ifelse(s3_1L_lgl,"C3_","C4_"),
         class_names_chr,
         ".R")
  return(class_fl_nms_chr)
}
get_class_ns <- function(prototype_lup,
                         class_nm_1L_chr){
  class_ns_1L_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                              match_var_nm_1L_chr = "type_chr",
                              match_value_xx = class_nm_1L_chr,
                              target_var_nm_1L_chr = "pt_ns_chr",
                              evaluate_lgl = F)
  return(class_ns_1L_chr)
}
get_nms_of_clss_to_inc <- function(parent_cls_nm_1L_chr,
                                         parent_ns_ls,
                                         base_set_of_clss_to_inc_chr = NULL){
  if(!is.null(parent_cls_nm_1L_chr) & parent_ns_ls$transformed_1L_chr == ""){
    if(!is.null(base_set_of_clss_to_inc_chr)){
      nms_of_clss_to_inc_chr <- c(parent_cls_nm_1L_chr,base_set_of_clss_to_inc_chr)
      nms_of_clss_to_inc_chr <- nms_of_clss_to_inc_chr[!duplicated(nms_of_clss_to_inc_chr)]
    }else{
      nms_of_clss_to_inc_chr <- parent_cls_nm_1L_chr
    }
  }
  return(nms_of_clss_to_inc_chr)
}
get_nms_of_curr_gnrcs <- function(req_pkgs_chr,
                                  generic_1L_chr){
  if(!req_pkgs_chr %>% purrr::discard(is.na) %>% identical(character(0)))
    purrr::walk(req_pkgs_chr %>% purrr::discard(is.na),
                ~ ready4fun::force_instl_of_reqd_pkg(.x))
  current_gens_s4 <- methods::getGenerics()
  packages_chr <- current_gens_s4@package
  curr_gnrcs_chr <- names(packages_chr) %>% stringr::str_replace_all("..GlobalEnv","")
  global_env_chr_vec <- packages_chr %in% c(".GlobalEnv")
  in_global_1L_lgl <- generic_1L_chr %in% curr_gnrcs_chr[global_env_chr_vec]
  nms_of_curr_gnrcs_ls <- list(curr_gnrcs_chr = curr_gnrcs_chr,
                               packages_chr = packages_chr,
                               in_global_1L_lgl = in_global_1L_lgl)
  return(nms_of_curr_gnrcs_ls)
}
get_parent_cls_ns <- function(prototype_lup,
                              parent_cls_nm_1L_chr,
                              dev_pkg_ns_1L_chr){ ## Can become a method of prototype table.
  if(!is.null(parent_cls_nm_1L_chr)){
    untransformed_1L_chr <- get_class_ns(prototype_lup = prototype_lup,
                                      class_nm_1L_chr = parent_cls_nm_1L_chr)
    transformed_1L_chr <- transform_class_ns(class_ns_chr = untransformed_1L_chr,
                                          dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr)
    parent_cls_ns <- list(untransformed_1L_chr = untransformed_1L_chr,
         transformed_1L_chr = transformed_1L_chr)
  }else{
    parent_cls_ns <- list(untransformed_1L_chr = NULL,
         transformed_1L_chr = "")
  }
  return(parent_cls_ns)
}
get_parent_cls_pt_fn <- function(parent_cls_nm_1L_chr,
                                    prototype_lup){
  parent_cls_pt_fn_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                     match_var_nm_1L_chr = "type_chr",
                                                     match_value_xx = parent_cls_nm_1L_chr,
                                                     target_var_nm_1L_chr = "vals_ls",
                                                     evaluate_lgl = F)
  return(parent_cls_pt_fn)
}
get_parent_cls_pts <- function(parent_cls_nm_1L_chr,
                               parent_ns_ls,
                               slot_names_chr){
  if(ifelse(is.null(parent_ns_ls$transformed_1L_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_1L_chr),
                   F,
                   parent_ns_ls$transformed_1L_chr!="")))
    ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_1L_chr)
  parent_cls_pts_chr <- purrr::map_chr(slot_names_chr,
                                       ~ ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr,
                                                                     package_1L_chr = transform_parent_ns_ls(parent_ns_ls))[[.x]])
  return(parent_cls_pts_chr)
}
get_parent_cls_slot_nms <- function(parent_cls_nm_1L_chr,
                                    parent_ns_ls){
  if(ifelse(is.null(parent_ns_ls$transformed_1L_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_1L_chr),
                   F,
                   parent_ns_ls$transformed_1L_chr!="")))
    ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_1L_chr)
  parent_cls_slot_nms_chr <- ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr,
                                  package = transform_parent_ns_ls(parent_ns_ls)) %>% names()
  return(parent_cls_slot_nms_chr)
}
