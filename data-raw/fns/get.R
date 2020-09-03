get_class_fl_nms <- function(class_names_chr_vec,
                             s3_lgl = T,
                             output_dir_chr = NA){
  paste0(ifelse(is.na(output_dir_chr),
                "",
                paste0(output_dir_chr,"/")),
         ifelse(s3_lgl,"C3_","C4_"),
         class_names_chr_vec,
         ".R")
}
get_class_ns <- function(prototype_lup,
                         class_chr){
  ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                              match_var_nm_1L_chr = "type",
                              match_value_xx = class_chr,
                              target_var_nm_1L_chr = "type_namespace",
                              evaluate_lgl = F)
}
get_nms_of_clss_to_inc <- function(parent_chr,
                                         parent_ns_ls,
                                         prespecified_includes_chr = NULL){
  if(!is.null(parent_chr) & parent_ns_ls$transformed_chr == ""){
    if(!is.null(prespecified_includes_chr)){
      includes_chr_vec <- c(parent_chr,prespecified_includes_chr)
      includes_chr_vec[!duplicated(includes_chr_vec)]
    }else{
      parent_chr
    }
  }
}
get_nms_of_curr_gnrcs <- function(required_pckg_chr_vec,
                                  generic_chr){
  if(!required_pckg_chr_vec %>% purrr::discard(is.na) %>% identical(character(0)))
    purrr::walk(required_pckg_chr_vec %>% purrr::discard(is.na),
                ~ ready4fun::force_instl_of_reqd_pkg(.x))
  current_gens_s4 <- methods::getGenerics()
  package_chr_vec <- current_gens_s4@package
  current_gens_chr_vec <- names(package_chr_vec) %>% stringr::str_replace_all("..GlobalEnv","")
  global_env_chr_vec <- package_chr_vec %in% c(".GlobalEnv")
  in_global_lgl <- generic_chr %in% current_gens_chr_vec[global_env_chr_vec]
  list(current_gens_chr_vec = current_gens_chr_vec,
       package_chr_vec = package_chr_vec,
       in_global_lgl = in_global_lgl)
}
get_parent_cls_ns <- function(prototype_lup,
                              parent_chr,
                              dev_pckg_ns_chr){ ## Can become a method of prototype table.
  if(!is.null(parent_chr)){
    untransformed_chr <- get_class_ns(prototype_lup = prototype_lup,
                                      class_chr = parent_chr)
    transformed_chr <- transform_class_ns(class_ns_chr = untransformed_chr,
                                          dev_pckg_ns_chr = dev_pckg_ns_chr)
    list(untransformed_chr = untransformed_chr,
         transformed_chr = transformed_chr)
  }else{
    list(untransformed_chr = NULL,
         transformed_chr = "")

  }
}
get_parent_cls_pt_fn <- function(parent_chr,
                                    prototype_lup){
  parent_proto_fn_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                     match_var_nm_1L_chr = "type",
                                                     match_value_xx = parent_chr,
                                                     target_var_nm_1L_chr = "value",
                                                     evaluate_lgl = F)
}
get_parent_cls_pts <- function(parent_chr,
                               parent_ns_ls,
                               slot_names_chr_vec){
  if(ifelse(is.null(parent_ns_ls$transformed_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_chr),
                   F,
                   parent_ns_ls$transformed_chr!="")))
    ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_chr)
  purrr::map_chr(slot_names_chr_vec,
                 ~ ready4fun::get_r4_obj_slots(parent_chr,
                                                   package_1L_chr = transform_parent_ns_ls(parent_ns_ls))[[.x]])
}
get_parent_cls_slot_nms <- function(parent_chr,
                                    parent_ns_ls){
  if(ifelse(is.null(parent_ns_ls$transformed_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_chr),
                   F,
                   parent_ns_ls$transformed_chr!="")))
    ready4fun::force_instl_of_reqd_pkg(parent_ns_ls$transformed_chr)
  ready4fun::get_r4_obj_slots(parent_chr,
                                  package = transform_parent_ns_ls(parent_ns_ls)) %>% names()
}
