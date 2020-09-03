make_helper_fn <- function(class_name,
                                parent,
                                class_slots,
                                proto_ls,
                                prototype_lup,
                                parent_ns_ls){
  if(!is.null(parent)){
    child_slots_chr <- class_slots
    class_slots <- get_parent_cls_slot_nms(parent_chr = parent, parent_ns_ls = parent_ns_ls)
    parent_proto <- get_parent_cls_pts(parent_chr = parent, parent_ns_ls = parent_ns_ls, slot_names_chr_vec = class_slots)
    child_ls_chr <- proto_ls %>% stringr::str_sub(start = 6, end = -2)
    proto_ls <- make_pt_ls(class_slots = class_slots,
                               type = parent_proto,
                               prototype_lup = prototype_lup)
    proto_ls <- paste0(proto_ls %>% stringr::str_sub(end = -2),
                       ",",
                       child_ls_chr,
                       ")")
    class_slots <- c(class_slots, child_slots_chr)
  }
  func_args <- proto_ls %>% stringr::str_replace("list","function") %>% stringr::str_replace_all(",",",\n")
  helper_function <- paste0(class_name,
                            " <- ",
                            func_args,
                            "{ \n",
                            "methods::new(\"",
                            class_name,
                            "\",\n",
                            paste0(class_slots," = ",class_slots) %>% stringr::str_c(sep="",collapse=",\n"),
                            ")\n}")
  return(helper_function)
}
write_slot_gtr_str_mthds <- function(slot_name_chr,
                             set_only,
                             parent,
                             class_name,
                             print_accessors,
                             output_folder,
                             ignore_ns_chr,
                             required_pckg_chr_vec){
  current_generics_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(required_pckg_chr_vec = required_pckg_chr_vec,
                                                 generic_chr = slot_name_chr,
                                                 ignore_ns_chr = ignore_ns_chr)
  import_packages_ls <- make_ls_of_pkgs_to_imp(current_generics_ls = current_generics_ls,
                                                fn_name_chr = slot_name_chr,
                                                ignore_ns_chr = ignore_ns_chr)
  write_gtr_str_mthds_for_r4(slot_name_chr = slot_name_chr,
                  set_only = set_only,
                  import_packages_ls = import_packages_ls,
                  class_name = class_name,
                  print_accessors = print_accessors,
                  output_folder = output_folder)
}
write_slot_gtr_str_mthds_rec <- function(slot_names_chr_vec,
                                 set_only,
                                 parent,
                                 class_name,
                                 print_accessors,
                                 output_folder,
                                 ignore_ns_chr,
                                 required_pckg_chr_vec){
  required_pckg_chr_vec <- purrr::map_chr(required_pckg_chr_vec, ~ stringr::str_replace(.x,"NA",NA_character_))
  ignore_ns_chr <- purrr::map_chr(ignore_ns_chr, ~ stringr::str_replace(.x,"NA",NA_character_))
  purrr::walk(slot_names_chr_vec,
              ~ write_slot_gtr_str_mthds(.x,
                                 set_only = .x %in% set_only,
                                 parent = parent,
                                 class_name = class_name,
                                 print_accessors = print_accessors,
                                 output_folder = output_folder,
                                 ignore_ns_chr = ignore_ns_chr,
                                 required_pckg_chr_vec = required_pckg_chr_vec))
}
make_alg_to_write_gtr_str_mthds <- function(class_name,
                                   parent,
                                   print_accessors,
                                   output_folder,
                                   ignore_ns_chr,
                                   required_pckg_chr_vec,
                                   parent_ns_ls){
  slot_names_chr_vec <- get_r4_obj_slots_chr_vec(class_name) %>% names()
  if(is.null(parent)){
    set_only <- ""
  }else{
    set_only  <- get_r4_obj_slots_chr_vec(parent,
                                          package_chr = transform_parent_ns_ls(parent_ns_ls)) %>% names()
  }
  accessors <- paste0("write_slot_gtr_str_mthds_rec(",
                      "slot_names_chr_vec = c(\"",
                      slot_names_chr_vec %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",",
                      "set_only = c(\"",
                      set_only %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",parent = \"",
                      parent,"\",",
                      "class_name = \"", class_name,
                      "\", print_accessors = ",
                      print_accessors,
                      ",output_folder = \"",output_folder,"\"",
                      ",ignore_ns_chr = c(\"",
                      ignore_ns_chr  %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",required_pckg_chr_vec = \"",required_pckg_chr_vec,"\"",
                      ")")
  return(accessors)
}
make_show_mthd_fn <- function(class_name,
                                   meaningful_names){
  descriptive_str <- purrr::map2_chr(names(meaningful_names),
                                     meaningful_names,
                                     ~ paste0("\"  ",
                                              .x,
                                              ":   \", format(object@",
                                              .y,
                                              "),  \"\\n\"")) %>%
    stringr::str_c(sep="",collapse=",")
  function_str <- paste0("function(object){\n",
                         "cat(is(object)[[1]], ",
                         "\"\\n\",",
                         descriptive_str,
                         ",\nsep = \"\")}")
  paste0("methods::setMethod(\"show\",\n",
         make_alg_to_gen_ref_to_cls(class_name),
         ",\n",
         function_str,
         ',\nwhere =  ',
         'globalenv()',
         "\n)")
}
