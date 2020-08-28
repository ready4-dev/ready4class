get_parent_ns_ls <- function(prototype_lup,
                             parent_chr,
                             dev_pckg_ns_chr){ ## Can become a method of prototype table.
  if(!is.null(parent_chr)){
    untransformed_chr <- get_class_ns_chr(prototype_lup = prototype_lup,
                                          class_chr = parent_chr)
    transformed_chr <- transform_class_ns_chr(class_ns_chr = untransformed_chr,
                                              dev_pckg_ns_chr = dev_pckg_ns_chr)
    list(untransformed_chr = untransformed_chr,
         transformed_chr = transformed_chr)
  }else{
    list(untransformed_chr = NULL,
         transformed_chr = "")

  }
}
get_class_ns_chr <- function(prototype_lup,
                             class_chr){
  ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                        match_var_nm_chr = "type",
                        match_value_xx = class_chr,
                        target_var_nm_chr = "type_namespace",
                        evaluate_lgl = F)
}
get_included_classes_chr_vec <- function(parent_chr,
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
get_class_files_chr <- function(class_names_chr_vec,
                               s3_lgl = T,
                               output_dir_chr = NA){
  paste0(ifelse(is.na(output_dir_chr),
                "",
                paste0(output_dir_chr,"/")),
         ifelse(s3_lgl,"C3_","C4_"),
         class_names_chr_vec,
         ".R")
}
get_parent_prototypes <- function(parent_chr,
                                  parent_ns_ls,
                                  slot_names_chr_vec){
  if(ifelse(is.null(parent_ns_ls$transformed_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_chr),
                   F,
                   parent_ns_ls$transformed_chr!="")))
    ready4fun::force_req_pkg_install(parent_ns_ls$transformed_chr)
  purrr::map_chr(slot_names_chr_vec,
                 ~ ready4fun::get_r4_obj_slots_chr_vec(parent_chr,
                                    package_chr = resolve_parent_ns_chr(parent_ns_ls))[[.x]])
}
get_parent_slot_names <- function(parent_chr,
                                  parent_ns_ls){
  if(ifelse(is.null(parent_ns_ls$transformed_chr),
            F,
            ifelse(is.na(parent_ns_ls$transformed_chr),
                   F,
                   parent_ns_ls$transformed_chr!="")))
    ready4fun::force_req_pkg_install(parent_ns_ls$transformed_chr)
  ready4fun::get_r4_obj_slots_chr_vec(parent_chr,
                   package = resolve_parent_ns_chr(parent_ns_ls)) %>% names()
}
get_proto_list <- function(class_slots,
                           type = NULL,
                           values = NULL,
                           make_val_string = TRUE,
                           prototype_lup){
    proto_ls <- purrr::map2_chr(class_slots,
                                type,
                                ~ paste0(.x,
                                         ' = ',
                                         ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                              match_var_nm_chr = "type",
                                                              match_value_xx = .y,
                                                              target_var_nm_chr = "value",
                                                              evaluate_lgl = FALSE)
                                         ))
    if(!is.null(values)){
    proto_ls <- purrr::pmap_chr(list(class_slots,
                                proto_ls,
                                1:length(proto_ls)),
                                ~ {
                                  if(..3 %in% 1:length(values)){
                                    paste0(..1,
                                           ' = ',
                                           ifelse(make_val_string,"\"",""),
                                           values[[..3]],
                                           ifelse(make_val_string,"\"",""))
                                  }else{
                                    ..2
                                  }
                                })

 }
  proto_ls %>%
    stringr::str_c(sep="",collapse=",") %>%
    paste0("list(",.,")")
}
get_parent_proto_fn_chr <- function(parent_chr,
                                    prototype_lup){
  parent_proto_fn_chr <- ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                     match_var_nm_chr = "type",
                                                     match_value_xx = parent_chr,
                                                     target_var_nm_chr = "value",
                                                     evaluate_lgl = F)
}
