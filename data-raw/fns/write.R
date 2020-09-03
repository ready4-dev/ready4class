write_gtr_str_mthds_for_r4 <- function(slot_name_chr,
                                       set_only,
                                       import_packages_ls,
                                       class_name,
                                       print_accessors,
                                       output_folder){
  assign_to_slot_chr <- paste0(slot_name_chr,"<-")
  if(!set_only){
    purrr::reduce(list(getter_ls = list(fn_name_chr = slot_name_chr,
                                        args_chr_vec = c("x"),
                                        fn = eval(parse(text=paste0("function(x){","x@",slot_name_chr,"}"))),
                                        fn_type_chr_vec = c("gen_get_slot","meth_get_slot"),
                                        import_chr_vec = import_packages_ls$getter_import_pckg),
                       setter_ls = list(fn_name_chr = assign_to_slot_chr,
                                        args_chr_vec = c("x","value"),
                                        fn = eval(parse(text=paste0("function(x, value) {",
                                                                    "\nx@",slot_name_chr,' <- value',
                                                                    "\nmethods::validObject(x)",
                                                                    "\nx",
                                                                    "\n}"))),
                                        fn_type_chr_vec = c("gen_set_slot","meth_set_slot"),
                                        import_chr_vec = import_packages_ls$setter_import_pckg)),
                  .init = list(new_file_lgl = F,
                               gnr_file = paste0(output_folder,
                                                 "/gnrc_",
                                                 slot_name_chr,
                                                 ".R"),
                               meth_file = ifelse(import_packages_ls$gen_get_exists_lgl,
                                                  paste0(output_folder,
                                                         "/gs_",
                                                         slot_name_chr,
                                                         ".R"),
                                                  paste0(output_folder,
                                                         "/gnrc_",
                                                         slot_name_chr,
                                                         ".R"))),
                  ~ write_scripts_to_make_gnrc_and_mthd(fn_name_chr = .y[[1]],
                                                        args_chr_vec = .y[[2]],
                                                        pkg_nm_1L_chr = ".GlobalEnv",
                                                        where_chr = 'globalenv()',
                                                        class_nm_1L_chr = class_name,
                                                        fn = .y[[3]],
                                                        fn_type_chr_vec = .y[[4]],
                                                        import_chr_vec = .y[[5]],
                                                        write_file_ls = .x,
                                                        output_dir_chr = output_folder,
                                                        append_lgl = T,
                                                        doc_in_class_lgl = F,
                                                        generic_exists_lgl = import_packages_ls$gen_get_exists_lgl,
                                                        s3_lgl = F,
                                                        write_lgl = print_accessors))
  }
}
write_gtr_str_mthds_for_slots <- function(slot_names_chr,
                                          set_only,
                                          parent_cls_nm_1L_chr,
                                          class_name,
                                          print_accessors,
                                          output_folder,
                                          ignore_ns_chr,
                                          req_pkgs_chr){
  req_pkgs_chr <- purrr::map_chr(req_pkgs_chr, ~ stringr::str_replace(.x,"NA",NA_character_))
  ignore_ns_chr <- purrr::map_chr(ignore_ns_chr, ~ stringr::str_replace(.x,"NA",NA_character_))
  purrr::walk(slot_names_chr,
              ~ write_slot_gtr_str_mthds(.x,
                                         set_only = .x %in% set_only,
                                         parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                         class_name = class_name,
                                         print_accessors = print_accessors,
                                         output_folder = output_folder,
                                         ignore_ns_chr = ignore_ns_chr,
                                         req_pkgs_chr = req_pkgs_chr))
}
write_mthds_for_r3_or_r4_clss <- function(methods_tb,
                                          fn_ls,
                                          pkg_nm_1L_chr,
                                          output_dir_chr){
  purrr::pwalk(methods_tb %>%
                 dplyr::mutate(first_lgl = c(T,rep(F,length(fn_ls)-1))) %>%
                 dplyr::mutate(append_lgl = c(F,rep(T,length(fn_ls)-1))),
               ~ write_std_mthd(fn = fn_ls[[..1]],
                                fn_name_chr = ..2,
                                class_nm_1L_chr = ..3,
                                fn_desc_chr_vec = c(..4,..5),
                                fn_title_chr = ..6,
                                fn_out_type_chr = ..7,
                                pkg_nm_1L_chr = pkg_nm_1L_chr,
                                output_dir_chr = output_dir_chr,
                                signature_chr = ..8,
                                append_lgl = ..10,
                                first_lgl = ..9))
}
write_scripts_to_mk_r3_clss <- function(name_stub,
                                        name_prefix = "ready4_",
                                        output_folder = "data-raw",
                                        class_desc = "",
                                        parent_cls_nm_1L_chr = NULL,
                                        type,
                                        type_namespace = "",
                                        type_checker_prefix = "is.",
                                        values = NULL,
                                        ordered = FALSE,
                                        allowed_values = NULL,
                                        min_max_values = NULL,
                                        start_end_values = NULL,
                                        prototype_lup,
                                        ignore_ns_chr  = NA_character_,
                                        file_exists_logic = "skip"){
  if(!dir.exists(output_folder))
    dir.create(output_folder)
  class_name <- paste0(name_prefix,name_stub)
  class_file_chr <- get_class_fl_nms(class_names_chr = class_name,
                                     s3_lgl = T,
                                     output_dir_chr = output_folder)
  if(file_exists_logic == "overwrite"){
    if (file.exists(class_file_chr))
      file.remove(class_file_chr)
  }
  if(file_exists_logic %in% c("append","overwrite")){
    s3_components_ls <- make_pt_ls_for_new_r3_cls(class_name_chr = class_name,
                                                  type_chr = type,
                                                  type_ns_chr = type_namespace,
                                                  type_checker_pfx_chr = type_checker_prefix,
                                                  values_chr = values,
                                                  ordered_lgl = ordered,
                                                  parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                  prototype_lup = prototype_lup,
                                                  min_max_values_dbl_vec = min_max_values,
                                                  start_end_values_dbl_vec = start_end_values,
                                                  ignore_ns_chr = ignore_ns_chr)
    sink(class_file_chr, append = ifelse(file_exists_logic =="append",TRUE,FALSE))
    writeLines(s3_components_ls$include_tags_chr)
    purrr::pwalk(list(s3_components_ls$fn_name_ls,
                      s3_components_ls$fn_text_ls,
                      c("s3_valid_instance", "s3_unvalidated_instance", "s3_prototype", "s3_validator", "s3_checker")),
                 ~ make_lines_for_writing_dmtd_fn(fn_name = ..1,
                                                  fn_text = ..2,
                                                  fn_type = ..3,
                                                  class_name = class_name,
                                                  class_desc = class_desc))
    ready4fun::close_open_sinks()
  }
  devtools::document()
  devtools::load_all()
}
write_scripts_to_mk_r3_clss_checker <- function(class_name,
                                                s3_validator){
  name_of_fn_to_check_if_is_valid_instance <- paste0("is_",class_name)
  fn_to_check_if_is_valid_instance <- paste0 (name_of_fn_to_check_if_is_valid_instance,
                                              " <- function(x) inherits(",
                                              s3_validator$fn_name,
                                              "(x), \"",
                                              class_name,
                                              "\")")
  list(fn_name = name_of_fn_to_check_if_is_valid_instance,
       fn_text = fn_to_check_if_is_valid_instance)
}
write_scripts_to_mk_r3_clss_constructor <- function(type,
                                                    type_checker_prefix,
                                                    type_namespace,
                                                    class_name,
                                                    s3_prototype){
  name_of_fn_to_construct_instance <- paste0("new_",class_name)
  stop_cndn_in_constructor <- ifelse(type=="factor",
                                     "TRUE",
                                     paste0(type_namespace,
                                            ifelse(type_namespace=="","","::"),
                                            type_checker_prefix,
                                            type,
                                            "(x)"))
  fn_to_construct_instance <- paste0(name_of_fn_to_construct_instance,
                                     " <- function(x){ \n",
                                     "stopifnot(",
                                     stop_cndn_in_constructor,
                                     ")\n",
                                     "class(x) <- append(",
                                     "c(\"",
                                     class_name,
                                     "\",setdiff(",
                                     paste0(s3_prototype$fn_name,"()"),
                                     " %>% class(),class(x)))",#class_name,
                                     ",\nclass(x))\nx\n}")
  list(fn_name = name_of_fn_to_construct_instance,
       fn_text = fn_to_construct_instance)

}
write_scripts_to_mk_r3_clss_pts <- function(type,
                                            type_namespace,
                                            values,
                                            ordered,
                                            class_name,
                                            parent_cls_nm_1L_chr,
                                            prototype_lup){
  ## Part 2 - Make Prototype Function
  if(type %in% c("tibble","list")){
    fn_call_to_create_prototype <- paste0(ifelse(type=="tibble","tibble::tibble(","list("),
                                          purrr::map2_chr(names(values),
                                                          values,
                                                          ~ paste0(.x,
                                                                   " = ",
                                                                   .y)) %>%
                                            stringr::str_c(sep="",collapse=",\n") ,
                                          ")")
    fn_call_to_create_prototype <- make_child_cls_fn_body(child_ext_fn_chr = fn_call_to_create_prototype,
                                                          parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                          prototype_lup = prototype_lup,
                                                          prepend_lgl = T)

  }else{
    if(type == "factor"){
      fn_call_to_create_prototype <- paste0("factor(x = character(),\nlevels=c(\"",
                                            values %>%
                                              stringr::str_c(sep="",collapse="\",\"\n") ,
                                            "\"),\nordered=",
                                            ordered,
                                            ")")

    }else{
      fn_call_to_create_prototype <- paste0(type_namespace,
                                            ifelse(type_namespace=="","","::"),
                                            type,
                                            "(0)"
      )
    }

  }
  name_of_fn_to_make_prototype <- paste0("make_prototype_",class_name)
  fn_to_make_prototype <- paste0(name_of_fn_to_make_prototype,
                                 " <- function(){ \n",
                                 fn_call_to_create_prototype,
                                 "\n}")
  list(fn_name = name_of_fn_to_make_prototype,
       fn_text = fn_to_make_prototype)

}
write_scripts_to_mk_r3_clss_validator <- function(type,
                                                  class_name,
                                                  s3_prototype,
                                                  min_max_values,
                                                  start_end_values,
                                                  values){
  name_of_fn_to_validate_instance <- paste0("validate_",class_name)
  validator_stop_cond_ls <- validator_stop_msg_call_ls <- NULL
  if(type %in% c("tibble","list")){
    stop_cndn_in_validator_1 <- paste0("sum(stringr::str_detect(names(x)[names(x) %in% names(",
                                       s3_prototype$fn_name,
                                       "())],\n",
                                       "names(",
                                       s3_prototype$fn_name,
                                       "())))!=length(names(",
                                       s3_prototype$fn_name,
                                       "()))")
    tb_or_ls_class_summary <- ifelse(type == "list",
                                     "lapply(class) %>% tibble::as_tibble() ",
                                     "dplyr::summarise_all(class) ")
    var_class_lup <- paste0(s3_prototype$fn_name,
                            "() %>% \n",
                            tb_or_ls_class_summary ,
                            "%>% \n tidyr::gather(variable,class)")
    stop_cndn_in_validator_2 <- paste0("!identical(",
                                       var_class_lup,
                                       " %>% \n",
                                       "dplyr::arrange(variable),\n",
                                       "x %>% \n",
                                       tb_or_ls_class_summary ,
                                       "%>% \n tidyr::gather(variable,class) %>% \n",
                                       "dplyr::filter(variable %in% names(",
                                       s3_prototype$fn_name,
                                       "())) %>% ",
                                       "dplyr::arrange(variable)",
                                       ")")
    obj_components_vec <- c(toupper(type),ifelse(type=="list","elements","columns"))
    stop_msg_call_in_validator_1 <- paste0("paste0(\"",
                                           obj_components_vec[1],
                                           " must include ",
                                           obj_components_vec[2],
                                           " named: \",\n",
                                           "names(",
                                           s3_prototype$fn_name,
                                           "()) %>% stringr::str_c(sep=\"\", collapse = \", \"))")
    stop_msg_call_in_validator_2 <- paste0("paste0(\"",
                                           obj_components_vec[1],
                                           " ",
                                           obj_components_vec[2],
                                           " should be of the following classes: \",\n",
                                           "purrr::map2_chr(",
                                           var_class_lup,
                                           " %>% \ndplyr::pull(1),\n ",
                                           var_class_lup,
                                           " %>% \ndplyr::pull(2),\n ",
                                           "~ paste0(.x,\": \",.y)) %>% \n",
                                           "stringr::str_c(sep=\"\", collapse = \", \"))")
    validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1,
                                   b = stop_cndn_in_validator_2)
    validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1,
                                       b = stop_msg_call_in_validator_2)
  }else{
    if(!is.null(min_max_values)){
      stop_cndn_in_validator_1 <- stop_msg_call_in_validator_1 <- stop_cndn_in_validator_2 <- stop_msg_call_in_validator_2 <- NULL
      if(!is.na(min_max_values[1])){
        stop_cndn_in_validator_1 <- paste0("any(",
                                           ifelse(type == "character","stringr::str_length(x)","x"),
                                           " < ",
                                           min_max_values[1],
                                           ")")
        stop_msg_call_in_validator_1 <- paste0("\"All values in valid ",
                                               class_name,
                                               " object must be ",
                                               ifelse(type == "character","of length ",""),
                                               "greater than or equal to ",
                                               min_max_values[1],
                                               ".\"")
      }
      if(!is.na(min_max_values[2])){
        stop_cndn_in_validator_2 <- paste0("any(",
                                           ifelse(type == "character","stringr::str_length(x)","x"),
                                           " > ",
                                           min_max_values[2],")")
        stop_msg_call_in_validator_2 <- paste0("\"All values in valid ",
                                               class_name,
                                               " object must be ",
                                               ifelse(type == "character","of length ",""),
                                               "less than or equal to ",
                                               min_max_values[2],
                                               ".\"")
      }
      validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1,
                                     b = stop_cndn_in_validator_2) %>% purrr::compact()
      validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1,
                                         b = stop_msg_call_in_validator_2) %>% purrr::compact()


    }
    ###
    if(!is.null(start_end_values)){
      stop_cndn_in_validator_1 <- stop_msg_call_in_validator_1 <- stop_cndn_in_validator_2 <- stop_msg_call_in_validator_2 <- NULL
      if(!is.na(start_end_values[1])){
        stop_cndn_in_validator_1 <- paste0("any(purrr::map_lgl(x, ~ !startsWith(.x,\"",
                                           start_end_values[1],
                                           "\")))")
        stop_msg_call_in_validator_1 <- paste0("\"All values in valid ",
                                               class_name,
                                               " object must start with \'",
                                               start_end_values[1],
                                               "\'.\"")
      }
      if(!is.na(start_end_values[2])){
        stop_cndn_in_validator_2 <- paste0("any(purrr::map_lgl(x, ~ !endsWith(.x,\"",
                                           start_end_values[2],
                                           "\")))")
        stop_msg_call_in_validator_2 <- paste0("\"All values in valid ",
                                               class_name,
                                               " object must end with \'",
                                               start_end_values[2],
                                               "\'.\"")
      }
      validator_stop_cond_ls <- append(validator_stop_cond_ls,
                                       list(a = stop_cndn_in_validator_1,
                                            b = stop_cndn_in_validator_2) %>% purrr::compact())
      validator_stop_msg_call_ls <- append(validator_stop_msg_call_ls,
                                           list(a = stop_msg_call_in_validator_1,
                                                b = stop_msg_call_in_validator_2) %>% purrr::compact())

    }
    if(type == "factor"){
      stop_cndn_in_validator_1 <- paste0("!identical(setdiff(x,c(\"",
                                         values %>% stringr::str_c(collapse = "\",\""),
                                         "\")),character(0))")
      stop_msg_call_in_validator_1 <- paste0("\"Levels in valid ",
                                             class_name,
                                             " object are: ",
                                             values %>% stringr::str_c(collapse = ","),
                                             ".\"")
      validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1)
      validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1)

    }
  }
  fn_to_validate_instance <- paste0(name_of_fn_to_validate_instance,
                                    " <- function(x){\n",
                                    purrr::map2_chr(validator_stop_cond_ls,
                                                    validator_stop_msg_call_ls,
                                                    ~ paste0("if(",
                                                             .x,
                                                             "){\n",
                                                             "stop(",
                                                             .y,
                                                             ",\ncall. = FALSE)\n}"
                                                    )) %>%
                                      stringr::str_c(sep="",
                                                     collapse = "\n "),
                                    "\nx}")
  list(fn_name = name_of_fn_to_validate_instance,
       fn_text = fn_to_validate_instance)

}
write_scripts_to_mk_r3_clss_valid_instance <- function(class_name,
                                                       s3_prototype,
                                                       s3_constructor,
                                                       s3_validator){
  fn_call_to_make_valid_instance <- paste0(s3_validator$fn_name,
                                           "(",
                                           s3_constructor$fn_name,
                                           "(x))")
  name_of_fn_to_make_valid_instance <- class_name
  fn_to_make_valid_instance <- paste0(name_of_fn_to_make_valid_instance,
                                      " <- function(x = ",
                                      s3_prototype$fn_name,
                                      "()){ \n",
                                      fn_call_to_make_valid_instance,
                                      "\n}")
  list(fn_name = name_of_fn_to_make_valid_instance,
       fn_text = fn_to_make_valid_instance)
}
write_scripts_to_mk_r4_clss <- function(name_stub,
                                        name_prefix = "ready4_",
                                        output_folder = "data-raw",
                                        output_sub_folder = NULL,
                                        class_desc = "",
                                        parent_cls_nm_1L_chr = NULL,
                                        class_slots,
                                        type,
                                        meaningful_names = NULL,
                                        values = NULL,
                                        allowed_values = NULL,
                                        include_classes = NULL,
                                        prototype_lup,
                                        ignore_ns_chr = NA_character_,
                                        req_pkgs_chr = NA_character_,
                                        names_include = NULL,
                                        not_same_length = NULL,
                                        print_set_class = TRUE,
                                        print_helper = TRUE,
                                        print_accessors = TRUE,
                                        print_validator = TRUE,
                                        print_meaningful_names = TRUE,
                                        class_in_cache_logic_chr = "stop"){
  if(!is.null(output_sub_folder)){
    output_folder <- paste0(output_folder,
                            "/",
                            output_sub_folder)
    if(!dir.exists(output_folder))
      dir.create(output_folder)
  }
  proto_ls <- make_pt_ls(class_slots = class_slots,
                         type = type,
                         values = values,
                         prototype_lup = prototype_lup)
  class_name <- paste0(name_prefix,name_stub)
  output_file_class <- get_class_fl_nms(class_names_chr = class_name,
                                        s3_lgl = F,
                                        output_dir_chr = output_folder)
  parent_ns_ls <- get_parent_cls_ns(prototype_lup = prototype_lup,
                                    parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                    dev_pkg_ns_1L_chr = ignore_ns_chr[1])
  write_to_mk_r4_cls(class_name = class_name,
                     class_slots = class_slots,
                     type = type,
                     proto_ls = proto_ls,
                     parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                     print_set_class = print_set_class,
                     class_desc = class_desc,
                     output_file_class = output_file_class,
                     include_classes = include_classes,
                     prototype_lup = prototype_lup,
                     helper_lgl = print_helper,
                     parent_ns_ls = parent_ns_ls)
  helper_function <- make_helper_fn(class_name = class_name,
                                    parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                    class_slots = class_slots,
                                    proto_ls = proto_ls,
                                    prototype_lup = prototype_lup,
                                    parent_ns_ls = parent_ns_ls)
  eval(parse(text=helper_function))
  if(print_helper){
    sink(output_file_class, append = TRUE)
    write_fn_dmt(fn_name_chr = class_name,
                 fn_type_chr = "set_class",
                 fn = eval(parse(text = class_name)),
                 class_name_chr = class_name)
    writeLines(helper_function)
    ready4fun::close_open_sinks()
  }
  accessors <- make_alg_to_write_gtr_str_mthds(class_name = class_name,
                                               parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                               print_accessors = print_accessors,
                                               output_folder = output_folder,
                                               ignore_ns_chr = ignore_ns_chr,
                                               req_pkgs_chr = req_pkgs_chr,
                                               parent_ns_ls = parent_ns_ls)
  eval(parse(text=accessors %>% replace_NA_in_fn()))
  valid_txt <- make_alg_to_set_validity_of_r4_cls(class_name = class_name,
                                                  parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                  not_same_length = not_same_length,
                                                  allowed_values = allowed_values,
                                                  names_include = names_include)
  if(print_validator){
    sink(output_file_class, append = TRUE)
    writeLines(paste0("\n",
                      valid_txt %>% stringr::str_replace(paste0(",\nwhere =  ",
                                                                "globalenv\\(\\)"),"")))
    ready4fun::close_open_sinks()
  }
  eval(parse(text=valid_txt))
  if(!is.null(meaningful_names)){
    meaningful_txt <- make_show_mthd_fn(class_name = class_name,
                                        meaningful_names = meaningful_names)
    eval(parse(text = meaningful_txt))
    if(print_meaningful_names){
      sink(output_file_class, append = TRUE)
      writeLines(paste0("\n",
                        meaningful_txt %>%
                          stringr::str_replace(paste0(",\nwhere =  ",
                                                      "globalenv\\(\\)"),"") %>%
                          stringr::str_replace_all("\\\\n\\\",","\\\\n\\\",\n") %>%
                          stringr::str_replace("\\nsep","sep")))
      ready4fun::close_open_sinks()
    }
  }
  devtools::document()
  devtools::load_all()
}
write_scripts_to_mk_clss <- function(new_classes_ls,
                                     pckg_name_chr,
                                     class_pfx_chr,
                                     R_dir_chr = "R",
                                     pt_lup,
                                     description_ls = NULL,
                                     ignore_ns_chr = NA_character_,
                                     req_pkgs_chr = NA_character_){
  reset_pkg_files_R(pckg_name_chr,
                    description_ls = description_ls)
  pt_lup <- make_class_pts_tb(new_classes_ls) %>%
    make_and_update(dev_pckg_namespace = pckg_name_chr,
                    name_prefix = class_pfx_chr,
                    output_dir = R_dir_chr,
                    file_exists_logic = "overwrite",
                    init_class_pt_lup =  pt_lup,
                    ignore_ns_chr = ignore_ns_chr,
                    req_pkgs_chr = req_pkgs_chr, ## Need to implement new delete package logic now documenting and loading package with each new class.
                    class_in_cache_logic_chr = "overwrite")
  usethis::use_data(pt_lup,overwrite = T)
  ready4fun::write_pt_lup_db()
  devtools::document()
  devtools::load_all()
  pt_lup
}
write_script_to_make_gnrc <- function(write_file_ls,
                             generic_exists_lgl,
                             gen_mthd_pair_ls,
                             fn_name_chr,
                             fn_type_chr,
                             fn_desc_chr = NA_character_,
                             fn_out_type_chr = NA_character_,
                             fn_title_chr = NA_character_,
                             class_name_chr = NA_character_,
                             output_dir_chr = NA_character_,
                             overwrite_lgl = F,
                             s3_lgl = F,
                             write_lgl = T,
                             doc_in_class_lgl = F){
  else_lgl <- write_file_ls$new_file_lgl
  if(!generic_exists_lgl){
    eval(parse(text = gen_mthd_pair_ls$generic_1L_chr))
    if(write_lgl & (!file.exists(write_file_ls$gnr_file) | write_file_ls$new_file_lgl | overwrite_lgl)){
      sink(write_file_ls$gnr_file,
           append = ifelse(fn_type_chr %in% c("gen_std_s3_mthd",
                                              "gen_std_s4_mthd"),F,write_file_ls$new_file_lgl))
      ready4fun::write_fn_dmt(fn_name_1L_chr = fn_name_chr,
                              fn_type_1L_chr = fn_type_chr,
                              fn = eval(parse(text=gen_mthd_pair_ls$gen_fn_chr)),
                              fn_desc_1L_chr = fn_desc_chr,
                              fn_out_type_1L_chr = fn_out_type_chr,
                              fn_title_1L_chr = fn_title_chr,
                              doc_in_class_1L_lgl = doc_in_class_lgl)
      writeLines(gen_mthd_pair_ls$generic_1L_chr %>% stringr::str_replace(paste0(",\nwhere =  ",
                                                                              "globalenv\\(\\)"),""))
      ready4fun::close_open_sinks()
      write_file_ls$new_file_lgl <- T
    }
    write_file_ls$meth_file <- write_file_ls$gnr_file
  }else{
    if(#else_lgl &
      !file.exists(write_file_ls$gnr_file)){
      write_file_ls$meth_file <- paste0(output_dir_chr,
                                        ifelse(fn_type_chr %in% c("gen_std_s3_mthd",
                                                                  "gen_std_s4_mthd"),
                                               "/mthd_",
                                               "/gs_"),
                                               fn_name_chr,
                                               ".R")
      if(!file.exists(write_file_ls$meth_file))
        file.create(write_file_ls$meth_file)
    }else{
      write_file_ls$meth_file <- write_file_ls$gnr_file
    }
  }
  write_file_ls
}
write_scripts_to_make_gnrc_and_mthd <- function(fn_name_chr,
                                                args_chr_vec = c("x"),
                                                signature_chr = NA_character_,
                                                pkg_nm_1L_chr = NA_character_ ,
                                                where_chr = NA_character_,
                                                class_nm_1L_chr,
                                                fn,
                                                fn_type_chr_vec,
                                                fn_desc_chr_vec = rep(NA_character_,2),
                                                fn_title_chr = NA_character_,
                                                fn_out_type_chr = NA_character_,
                                                import_chr_vec,
                                                write_file_ls,
                                                output_dir_chr,
                                                append_lgl = T,
                                                doc_in_class_lgl = F,
                                                generic_exists_lgl,
                                                overwrite_lgl = F,
                                                s3_lgl,
                                                write_lgl){
  gen_mthd_pair_ls <- make_gnrc_mthd_pair_ls(name_chr = fn_name_chr,
                                             args_chr_vec = args_chr_vec,
                                             signature_chr = signature_chr,
                                             pkg_nm_1L_chr = pkg_nm_1L_chr,
                                             where_chr = where_chr,
                                             class_nm_1L_chr = class_nm_1L_chr,
                                             fn = fn)
  write_file_ls <- write_script_to_make_gnrc(write_file_ls = write_file_ls,
                                             generic_exists_lgl = generic_exists_lgl,
                                             gen_mthd_pair_ls = gen_mthd_pair_ls,
                                             fn_name_chr = fn_name_chr,
                                             fn_type_chr = fn_type_chr_vec[1],
                                             fn_desc_chr = fn_desc_chr_vec[1],
                                             fn_out_type_chr = NA_character_,
                                             fn_title_chr = fn_title_chr,
                                             class_name_chr = class_nm_1L_chr,
                                             output_dir_chr = output_dir_chr,
                                             overwrite_lgl = overwrite_lgl,
                                             s3_lgl = s3_lgl,
                                             write_lgl = write_lgl,
                                             doc_in_class_lgl = doc_in_class_lgl)
  write_file_ls$new_file_lgl <- ifelse(!overwrite_lgl,T,write_file_ls$new_file_lgl)
  write_script_to_make_mthd(write_file_ls = write_file_ls,
                            gen_mthd_pair_ls = gen_mthd_pair_ls,
                            class_name_chr = class_nm_1L_chr,
                            fn_name_chr = fn_name_chr,
                            fn_type_chr = fn_type_chr_vec[2],
                            fn_desc_chr = fn_desc_chr_vec[2],
                            fn_out_type_chr = fn_out_type_chr,
                            import_chr_vec = import_chr_vec,
                            write_lgl = write_lgl,
                            append_lgl = append_lgl,
                            doc_in_class_lgl = doc_in_class_lgl)
  write_file_ls
}
write_script_to_make_mthd <- function(write_file_ls,
                         gen_mthd_pair_ls,
                         class_name_chr,
                         fn_name_chr,
                         fn_type_chr,
                         fn_desc_chr = NA_character_,
                         fn_out_type_chr = NA_character_,
                         import_chr_vec,
                         write_lgl = T,
                         append_lgl = T,
                         doc_in_class_lgl = F){
  eval(parse(text = gen_mthd_pair_ls$method_chr))
  if(write_lgl){
    sink(write_file_ls$meth_file, append =  ifelse(identical(write_file_ls$gen_file,write_file_ls$meth_file),
                                                   T,
                                                   ifelse(fn_type_chr %in% c("gen_std_s3_mthd",
                                                                             "gen_std_s4_mthd"),T,write_file_ls$new_file_lgl)))
    ready4fun::write_fn_dmt(fn_name_1L_chr = fn_name_chr,
                            fn_type_1L_chr = fn_type_chr,
                            fn = eval(parse(text=gen_mthd_pair_ls$meth_fn_chr)),
                            fn_desc_1L_chr = fn_desc_chr,
                            fn_out_type_1L_chr = fn_out_type_chr,
                            class_name_1L_chr = class_name_chr,
                            import_chr = import_chr_vec,
                            doc_in_class_1L_lgl = doc_in_class_lgl)
    writeLines(gen_mthd_pair_ls$method_chr %>% stringr::str_replace(paste0(",\nwhere =  ",
                                                                           "globalenv\\(\\)"),""))
    ready4fun::close_open_sinks()
  }
}
write_slot_gtr_str_mthds <- function(slot_name_chr,
                                     set_only,
                                     parent_cls_nm_1L_chr,
                                     class_name,
                                     print_accessors,
                                     output_folder,
                                     ignore_ns_chr,
                                     req_pkgs_chr){
  current_generics_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr,
                                                          generic_1L_chr = slot_name_chr,
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
write_std_mthd <- function(fn,
                           fn_name_chr,
                           class_nm_1L_chr,
                           fn_desc_chr_vec,
                           fn_title_chr,
                           fn_out_type_chr,
                           pkg_nm_1L_chr,
                           output_dir_chr,
                           signature_chr = NA_character_, ## Add required package here.
                           append_lgl = T,
                           first_lgl = T){
  s3_lgl = !isS4(eval(parse(text=paste0(class_nm_1L_chr,"()"))))
  testit::assert("x" %in% formalArgs(fn))
  fn_type_chr_vec <- paste0(c("gen_","meth_"),
                            "std_",
                            ifelse(s3_lgl,"s3","s4"),
                            "_mthd")
  write_file_ls <- list(new_file_lgl = F,
                        gnr_file = paste0(output_dir_chr,
                                          "/gnrc_",
                                          fn_name_chr,
                                          ".R"),
                        meth_file = paste0(output_dir_chr,
                                           "/meth_",
                                           fn_name_chr,
                                           ".R"))
  current_generics_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(req_pkgs_chr = NA_character_, # Add ready4 here
                                                          generic_1L_chr = fn_name_chr,
                                                          ignore_ns_chr = ifelse(pkg_nm_1L_chr %in% rownames(installed.packages()),
                                                                                 pkg_nm_1L_chr,
                                                                                 NA_character_))
  ## NB: Ensure latest ready4 bundle (ready4dev and ready4mod) is installed.
  import_packages_ls <- make_ls_of_pkgs_to_imp(current_generics_ls = current_generics_ls,
                                               fn_name_chr = fn_name_chr,
                                               ignore_ns_chr = ifelse(pkg_nm_1L_chr %in% rownames(installed.packages()),
                                                                      pkg_nm_1L_chr,
                                                                      NA_character_))
  generic_exists_lgl <- import_packages_ls$gen_get_exists_lgl
  import_chr_vec <- import_packages_ls$getter_import_pckg[import_packages_ls$getter_import_pckg!=pkg_nm_1L_chr]
  if(identical(import_chr_vec,character(0)))
    import_chr_vec <- NA_character_
  write_file_ls <- write_scripts_to_make_gnrc_and_mthd(fn_name_chr = fn_name_chr,
                                                       args_chr_vec = c("x",
                                                                        ifelse(length(formalArgs(fn))>1,
                                                                               "...",
                                                                               NA_character_)) %>%
                                                         purrr::discard(is.na),
                                                       signature_chr = signature_chr,
                                                       pkg_nm_1L_chr = NA_character_,
                                                       where_chr = NA_character_,
                                                       class_nm_1L_chr = class_nm_1L_chr,
                                                       fn = fn,
                                                       fn_type_chr_vec = fn_type_chr_vec,
                                                       fn_desc_chr_vec = fn_desc_chr_vec,
                                                       fn_title_chr = fn_title_chr,
                                                       fn_out_type_chr = fn_out_type_chr,
                                                       import_chr_vec = import_chr_vec,
                                                       write_file_ls = write_file_ls,
                                                       output_dir_chr = output_dir_chr,
                                                       append_lgl = append_lgl,
                                                       doc_in_class_lgl = F,
                                                       generic_exists_lgl = generic_exists_lgl,
                                                       overwrite_lgl = !append_lgl,
                                                       s3_lgl = s3_lgl,
                                                       write_lgl = T)
  write_file_ls
}
write_to_delete_fls_with_ptrn <- function(dir_chr,
                                          pattern_chr){
  if(!is.na(pattern_chr)){
    files_chr_vec <- list.files(dir_chr, pattern = pattern_chr)
    if(!identical(files_chr_vec, character(0)))
      paste0(dir_chr,"/",files_chr_vec) %>% file.remove()
  }
}
write_to_delete_gnrc_fn_fls <- function(x,
                                        output_dir){ ## NEEDS TO BE TESTED AND COMPARED TO DELETE_FILES FUNCITON
  delete_vec <- x %>%
    dplyr::pull(class_slots) %>%
    purrr::compact() %>%
    purrr::flatten() %>%
    purrr::flatten_chr()
  if(!identical(delete_vec,character(0)))
    paste0(output_dir,"/gnrc_",
           purrr::reduce(delete_vec ,
                         ~ append(.x,.y[!.y %in% .x])),
           ".R") %>%
    purrr::walk(~ if(file.exists(.x))
      file.remove(.x))
}
write_to_mk_r4_cls <- function(class_name,
                               class_slots,
                               type,
                               proto_ls,
                               parent_cls_nm_1L_chr,
                               print_set_class,
                               class_desc,
                               output_file_class,
                               include_classes,
                               prototype_lup,
                               helper_lgl = F,
                               parent_ns_ls){
  slot_str <- purrr::map2_chr(class_slots,
                              type,
                              ~ paste0(.x,
                                       ' = "',
                                       .y,
                                       '"')) %>%
    stringr::str_c(sep="",collapse=",") %>%
    paste0("c(",.,")")
  slots <- eval(parse(text = slot_str))
  old_class_tb_extension <- make_alg_to_set_old_clss(type = type,
                                                     prototype_lup = prototype_lup)
  if(!identical(old_class_tb_extension,character(0))){
    eval(parse(text = old_class_tb_extension)) ## CHECK
  }else{
    old_class_tb_extension <- ""
  }
  prototype <- eval(parse(text = proto_ls))
  if(is.null(parent_cls_nm_1L_chr)){
    st_class_fn <- paste0("methods::setClass(",
                          make_alg_to_gen_ref_to_cls(class_name),
                          ",\nslots = ",
                          slot_str,
                          ",\nprototype =  ",
                          proto_ls,
                          ",\nwhere =  ",
                          "globalenv()",
                          ")")
  }else{
    st_class_fn <- paste0("methods::setClass(",
                          make_alg_to_gen_ref_to_cls(class_name,
                                                     pkg_nm_1L_chr = transform_parent_ns_ls(parent_ns_ls) %>%
                                                       ready4fun::update_ns()),
                          ",\ncontains = \"",
                          parent_cls_nm_1L_chr,
                          "\",\nslots = ",
                          slot_str,
                          ",\nprototype =  ",
                          proto_ls,
                          ",\nwhere =  ",
                          "globalenv()",
                          ")")
    parent_slots_chr_vec <- get_parent_cls_slot_nms(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                    parent_ns_ls = parent_ns_ls)
    parent_prototype_chr_vec <- get_parent_cls_pts(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                   parent_ns_ls = parent_ns_ls,
                                                   slot_names_chr = parent_slots_chr_vec)
    parent_prototype_chr_vec <- `names<-`(parent_prototype_chr_vec,parent_slots_chr_vec)
    slots <- c(slots, parent_prototype_chr_vec)
    slots <- slots[!duplicated(names(slots))]
  }
  slots_tags <- paste0("#' @slot ",
                       names(slots),
                       " ",
                       slots,
                       "\n",
                       collapse="")
  included_classes_chr_vec <- get_nms_of_clss_to_inc(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                     parent_ns_ls = parent_ns_ls,
                                                     base_set_of_clss_to_inc_chr = include_classes)
  include_tags_chr <- make_dmt_inc_tag(included_classes_chr_vec, s3_lgl = F)
  if(print_set_class){
    sink(output_file_class)
    writeLines(paste0(paste0("#' ",class_name,"\n"),
                      paste0("#' @name ",class_name,"\n"),
                      "#' @description An S4 class to represent ",
                      class_desc,
                      "\n",
                      include_tags_chr,
                      old_class_tb_extension %>%
                        stringr::str_replace_all(paste0(",where =  ",
                                                        "globalenv\\(\\)"),""),
                      ifelse(old_class_tb_extension=="","","\n"),
                      slots_tags,
                      ifelse(!ifelse(is.null(parent_ns_ls$transformed_1L_chr),
                                     F,
                                     ifelse(is.na(parent_ns_ls$transformed_1L_chr),
                                            F,
                                            parent_ns_ls$transformed_1L_chr!="")),
                             "",
                             paste0("#' @import ",parent_ns_ls$transformed_1L_chr,"\n")),
                      ifelse(helper_lgl,"",paste0("#' @exportClass ",class_name,"\n")),
                      ifelse(helper_lgl,"",paste0(class_name," <- ")),
                      st_class_fn %>%
                        stringr::str_replace(paste0(",\nwhere =  ",
                                                    "globalenv\\(\\)"),"") %>%
                        transform_alg_to_ref_cls_nm(pkg_nm_1L_chr = ifelse(is.null(parent_cls_nm_1L_chr),
                                                                         ".GlobalEnv",
                                                                         transform_parent_ns_ls(parent_ns_ls) %>%
                                                                           ready4fun::update_ns())),
                      "\n"))
    ready4fun::close_open_sinks()
  }
  eval(parse(text = st_class_fn))
}
