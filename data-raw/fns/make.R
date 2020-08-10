make_new_classes <- function(new_classes_ls,
                             pckg_name_chr,
                             class_pfx_chr,
                             R_dir_chr = "R",
                             pt_lup,
                             description_ls = NULL,
                             ignore_ns_chr = NA_character_,
                             required_pckg_chr_vec = NA_character_){
  reset_pkg_files_R(pckg_name_chr,
                    description_ls = description_ls)
  pt_lup <- make_class_mk_tb(new_classes_ls) %>%
    make_and_update(dev_pckg_namespace = pckg_name_chr,
                    name_prefix = class_pfx_chr,
                    output_dir = R_dir_chr,
                    file_exists_logic = "overwrite",
                    init_class_pt_lup =  pt_lup,
                    ignore_ns_chr = ignore_ns_chr,
                    required_pckg_chr_vec = required_pckg_chr_vec, ## Need to implement new delete package logic now documenting and loading package with each new class.
                    class_in_cache_logic_chr = "overwrite")
  usethis::use_data(pt_lup,overwrite = T)
  ready4fun::write_pt_lup_db_R()
  devtools::document()
  devtools::load_all()
  pt_lup
}
make_gen_fn_chr <- function(name_chr,
                            args_chr_vec){
  if(all(!is.na(args_chr_vec))){
    paste0('function(',paste0(args_chr_vec, collapse = ", "),') standardGeneric("', name_chr,'")')
  }else{
    ""
  }
}
make_meth_fn_chr <- function(fn){
  deparse(fn) %>% paste0(collapse="\n")
}
make_generic_chr <- function(name_chr,
                             args_chr_vec = c("x"),
                             signature_chr = NA_character_,
                             where_chr = NA_character_){
  paste0('methods::setGeneric(\"', name_chr,'\"',
         ifelse(is.na(args_chr_vec[1]),
                '',
                paste0(', ',make_gen_fn_chr(name_chr,args_chr_vec = args_chr_vec))),
         ifelse(is.na(where_chr[1]),'',paste0(',\nwhere =  ', where_chr)),
         ifelse(is.na(signature_chr[1]),'',paste0(',\nsignature =  \"', signature_chr,'\"')),
         ')' )
}
make_method_chr <- function(name_chr,
                            class_chr,
                            fn,
                            package_chr = NA_character_ ,
                            where_chr = NA_character_){
  paste0('methods::setMethod(\"', name_chr, '\"',
         ', ',ifelse(is.na(package_chr[1]),paste0('\"',class_chr,'\"'),paste0(make_className_chr(class_chr,package_chr=package_chr))),
         ', ', make_meth_fn_chr(fn),
         ifelse(is.na(where_chr[1]),'',paste0(',\nwhere =  ', where_chr)),
         ')')
}
make_gen_mthd_pair_ls <- function(name_chr,
                                  args_chr_vec = c("x"),
                                  signature_chr = NA_character_,
                                  package_chr = NA_character_ ,
                                  where_chr = NA_character_,
                                  class_chr,
                                  fn){
  list(generic_chr = make_generic_chr(name_chr,
                                      args_chr_vec = args_chr_vec,
                                      signature_chr = signature_chr,
                                      where_chr = where_chr),
       method_chr = make_method_chr(name_chr,
                                    class_chr = class_chr,
                                    fn = fn,
                                    package_chr = package_chr,
                                    where_chr = where_chr),
       gen_fn_chr = make_gen_fn_chr(name_chr,
                                    args_chr_vec = args_chr_vec),
       meth_fn_chr = make_meth_fn_chr(fn))
}
make_class_type_mk_tb <- function(class_type_mk_ls,
                                  make_s3_lgl = T){
  cl_mk_tb <- class_type_mk_ls  %>%
    purrr:::reduce(.init = ready4_class_make_tb(),
                   ~ {
                     testit::assert(paste0("Allowable list element names are: ", names(.x) %>% paste0(collapse = ",")),names(.y) %in% names(.x))
                     rlang::exec(tibble::add_case,.x,!!!.y)
                   }
    ) %>%
    dplyr::mutate(make_s3 = make_s3_lgl)  %>%
    remake_ls_cols()
  if(make_s3_lgl){
    cl_mk_tb <- cl_mk_tb %>%
      dplyr::mutate_at(c("class_slots","include_classes"),
                       ~ purrr::flatten(.x))
  }
  cl_mk_tb
}
make_class_mk_tb <- function(class_mk_ls){
  purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ make_class_type_mk_tb(.x,
                                          make_s3_lgl = ifelse(.y=="s3_ls",T,F))

  )
}
make_class_mk_r3 <- function(class_mk_ls){
  purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ {
                    if(.y=="s3_ls"){
                      fn = make_s3_mk_tb_r3
                    }else{
                      fn = make_s4_mk_tb_r3
                    }
                    rlang::exec(fn,.x)
                  })
}
make_s3_mk_tb_r3 <- function(x){
  purrr::map_dfr(x,
                 ~make_s3_mk_tb_row(.x))
}
make_s4_mk_tb_r3 <- function(x){
  purrr::map_dfr(x,
                 ~make_s4_mk_tb_row(.x))
}
make_s3_mk_tb_row <- function(x){
  make_class_type_mk_tb(list(name_stub = x@name_stub_chr,
                             prototype = x@prototype_ls,
                             prototype_checker_prefix = x@prototype_chk_pfx_ls,
                             prototype_namespace = x@prototype_ns_ls,
                             values = x@values_ls,
                             allowed_values = x@allowed_values_ls,
                             min_max_values = x@min_max_values_ls,
                             start_end_values = x@start_end_values_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             include_classes = x@include_classes_ls) %>% list(),
                        make_s3_lgl = T)
}
make_s4_mk_tb_row <- function(x){
  make_class_type_mk_tb(list(name_stub = x@name_stub_chr,
                             prototype = x@prototype_ls,
                             values = x@values_ls,
                             allowed_values = x@allowed_values_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             class_slots = x@class_slots_ls,
                             meaningful_names = x@meaningful_names_ls,
                             include_classes = x@include_classes_ls) %>% list(),
                        make_s3_lgl = F)
}
make_ready_s4 <- function(name_stub,
                          name_prefix = "ready4_",
                          output_folder = "data-raw",
                          output_sub_folder = NULL,
                          class_desc = "",
                          parent = NULL,
                          class_slots,
                          type,
                          meaningful_names = NULL,
                          values = NULL,
                          allowed_values = NULL,
                          include_classes = NULL,
                          prototype_lup,
                          ignore_ns_chr = NA_character_,
                          required_pckg_chr_vec = NA_character_,
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
  proto_ls <- get_proto_list(class_slots = class_slots,
                             type = type,
                             values = values,
                             prototype_lup = prototype_lup)
  class_name <- paste0(name_prefix,name_stub)
  output_file_class <- get_class_files_chr(class_names_chr_vec = class_name,
                                           s3_lgl = F,
                                           output_dir_chr = output_folder)
  parent_ns_ls <- get_parent_ns_ls(prototype_lup = prototype_lup,
                                   parent_chr = parent,
                                   dev_pckg_ns_chr = ignore_ns_chr[1])
  set_ready_class(class_name = class_name,
                  class_slots = class_slots,
                  type = type,
                  proto_ls = proto_ls,
                  parent = parent,
                  print_set_class = print_set_class,
                  class_desc = class_desc,
                  output_file_class = output_file_class,
                  include_classes = include_classes,
                  prototype_lup = prototype_lup,
                  helper_lgl = print_helper,
                  parent_ns_ls = parent_ns_ls)
  helper_function <- create_ready_helper(class_name = class_name,
                                         parent = parent,
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
  accessors <- create_ready_accessors(class_name = class_name,
                                      parent = parent,
                                      print_accessors = print_accessors,
                                      output_folder = output_folder,
                                      ignore_ns_chr = ignore_ns_chr,
                                      required_pckg_chr_vec = required_pckg_chr_vec,
                                      parent_ns_ls = parent_ns_ls)
  eval(parse(text=accessors %>% replace_NA_in_fn_chr()))
  valid_txt <- validate_ready(class_name = class_name,
                              parent = parent,
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
    meaningful_txt <- create_ready_show_mthd(class_name = class_name,
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
make_include_tag_chr <- function(class_names_chr_vec,
                                 s3_lgl = T){
  ifelse(!is.null(class_names_chr_vec),
         paste0("#' @include ",get_class_files_chr(class_names_chr_vec = class_names_chr_vec, s3_lgl = s3_lgl) %>% stringr::str_c(collapse=" "),"\n"),
         "")
}
make_current_generics_ls <- function(required_pckg_chr_vec,
                                     generic_chr){
  if(!required_pckg_chr_vec %>% purrr::discard(is.na) %>% identical(character(0)))
    purrr::walk(required_pckg_chr_vec %>% purrr::discard(is.na),
                ~ ready4fun::force_req_pkg_install(.x))
  current_gens_s4 <- methods::getGenerics()
  package_chr_vec <- current_gens_s4@package
  current_gens_chr_vec <- names(package_chr_vec) %>% stringr::str_replace_all("..GlobalEnv","")
  global_env_chr_vec <- package_chr_vec %in% c(".GlobalEnv")
  in_global_lgl <- generic_chr %in% current_gens_chr_vec[global_env_chr_vec]
  list(current_gens_chr_vec = current_gens_chr_vec,
       package_chr_vec = package_chr_vec,
       in_global_lgl = in_global_lgl)
}
make_import_packages_ls <- function(current_generics_ls,
                                    fn_name_chr,
                                    ignore_ns_chr){
  package_chr_vec <- current_generics_ls$package_chr_vec[!current_generics_ls$package_chr_vec %in% c(".GlobalEnv")]
  gen_get_exists_lgl_vec <- purrr::map2_lgl(package_chr_vec, names(package_chr_vec), ~ ((.x == fn_name_chr | .y == paste0(fn_name_chr,".",.x)) & !.x %in% ignore_ns_chr))
  gen_get_exists_lgl <- any(gen_get_exists_lgl_vec)
  getter_import_pckg <- ifelse(gen_get_exists_lgl,package_chr_vec[gen_get_exists_lgl_vec],NA_character_)
  gen_set_exists_lgl_vec <- purrr::map2_lgl(package_chr_vec, names(package_chr_vec), ~ ((.x == paste0(fn_name_chr,"<-") | .y == paste0(fn_name_chr,"<-.",.x)) & !.x %in% ignore_ns_chr))
  gen_set_exists_lgl <- any(gen_set_exists_lgl_vec)
  setter_import_pckg <- ifelse(gen_set_exists_lgl,package_chr_vec[gen_set_exists_lgl_vec],NA_character_)
  list(getter_import_pckg = getter_import_pckg,
       setter_import_pckg = setter_import_pckg,
       gen_get_exists_lgl = gen_get_exists_lgl,
       gen_set_exists_lgl = gen_set_exists_lgl)
}
make_and_tf_curr_gen_ls <- function(required_pckg_chr_vec,
                                    generic_chr,
                                    ignore_ns_chr){
  current_generics_ls <- make_current_generics_ls(required_pckg_chr_vec = required_pckg_chr_vec,
                                                  generic_chr = generic_chr)
  if(is.na(ignore_ns_chr[1])){
    dependencies_chr_vec <- character(0)
  }else{
    dependencies_chr_vec <- gtools::getDependencies(ignore_ns_chr[1])
  }
  if(!required_pckg_chr_vec %>% purrr::discard(is.na) %>% identical(character(0)))
    required_pckg_chr_vec <- required_pckg_chr_vec[!required_pckg_chr_vec %in% dependencies_chr_vec]
  if(current_generics_ls$in_global_lgl){
    ready4fun::unload_packages(package_chr_vec = required_pckg_chr_vec)
    current_generics_ls <- make_current_generics_ls(required_pckg_chr_vec = required_pckg_chr_vec,
                                                    generic_chr = generic_chr)
  }
  current_generics_ls
}
make_className_chr <- function(class_chr,
                               package_chr = ".GlobalEnv"){
  paste0("methods::className(\"",
         class_chr,
         "\",\"",
         package_chr,
         "\")")
}
make_ready_s3 <- function(name_stub,
                          name_prefix = "ready4_",
                          output_folder = "data-raw",
                          class_desc = "",
                          parent = NULL,
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
  class_file_chr <- get_class_files_chr(class_names_chr_vec = class_name,
                                        s3_lgl = T,
                                        output_dir_chr = output_folder)
  if(file_exists_logic == "overwrite"){
    if (file.exists(class_file_chr))
      file.remove(class_file_chr)
  }
  if(file_exists_logic %in% c("append","overwrite")){
    s3_components_ls <- make_s3_components_ls(class_name_chr = class_name,
                                              type_chr = type,
                                              type_ns_chr = type_namespace,
                                              type_checker_pfx_chr = type_checker_prefix,
                                              values_chr = values,
                                              ordered_lgl = ordered,
                                              parent_chr = parent,
                                              prototype_lup = prototype_lup,
                                              min_max_values_dbl_vec = min_max_values,
                                              start_end_values_dbl_vec = start_end_values,
                                              ignore_ns_chr = ignore_ns_chr)
    sink(class_file_chr, append = ifelse(file_exists_logic =="append",TRUE,FALSE))
    writeLines(s3_components_ls$include_tags_chr)
    purrr::pwalk(list(s3_components_ls$fn_name_ls,
                      s3_components_ls$fn_text_ls,
                      c("s3_valid_instance", "s3_unvalidated_instance", "s3_prototype", "s3_validator", "s3_checker")),
                 ~ write_fn_txt_and_tags(fn_name = ..1,
                                         fn_text = ..2,
                                         fn_type = ..3,
                                         class_name = class_name,
                                         class_desc = class_desc))
    ready4fun::close_open_sinks()
  }
  devtools::document()
  devtools::load_all()
}

make_s3_components_ls <- function(class_name_chr,
                                  type_chr,
                                  type_ns_chr,
                                  type_checker_pfx_chr,
                                  values_chr,
                                  ordered_lgl,
                                  parent_chr,
                                  prototype_lup,
                                  min_max_values_dbl_vec,
                                  start_end_values_dbl_vec,
                                  ignore_ns_chr){
  s3_prototype <- make_ready_s3_prototype(type = type_chr,
                                          type_namespace = type_ns_chr,
                                          values = values_chr,
                                          ordered = ordered_lgl,
                                          class_name = class_name_chr,
                                          parent_chr = parent_chr,
                                          prototype_lup = prototype_lup)
  s3_constructor <- make_ready_s3_constructor(type = type_chr,
                                              type_checker_prefix = type_checker_pfx_chr,
                                              type_namespace = type_ns_chr,
                                              class_name = class_name_chr,
                                              s3_prototype = s3_prototype)
  s3_validator <- make_ready_s3_validator(type = type_chr,
                                          class_name = class_name_chr,
                                          s3_prototype = s3_prototype,
                                          min_max_values = min_max_values_dbl_vec,
                                          start_end_values = start_end_values_dbl_vec,
                                          values = values_chr)
  s3_valid_instance <- make_ready_s3_valid_instance(class_name = class_name_chr,
                                                    s3_prototype = s3_prototype,
                                                    s3_constructor = s3_constructor,
                                                    s3_validator = s3_validator)
  s3_checker <- make_ready_s3_checker(class_name = class_name_chr,
                                      s3_validator = s3_validator)

  fn_name_ls <- list(valid_instance = s3_valid_instance$fn_name,
                     unvalidated_instance = s3_constructor$fn_name,
                     prototype = s3_prototype$fn_name,
                     validator = s3_validator$fn_name,
                     checker = s3_checker$fn_name)
  fn_text_ls <- list(valid_instance = s3_valid_instance$fn_text,
                     unvalidated_instance = s3_constructor$fn_text,
                     prototype = s3_prototype$fn_text,
                     validator = s3_validator$fn_text,
                     checker = s3_checker$fn_text)
  include_tags_chr <- get_parent_ns_ls(prototype_lup = prototype_lup,
                                       parent_chr = parent_chr,
                                       dev_pckg_ns_chr = ignore_ns_chr[1]) %>%
    get_included_classes_chr_vec(parent_chr = parent_chr,
                                 prespecified_includes_chr = NULL) %>%
    make_include_tag_chr(s3_lgl = T)
  list(fn_name_ls = fn_name_ls,
       fn_text_ls = fn_text_ls,
       include_tags_chr = include_tags_chr)
}

make_ready_s3_prototype <- function(type,
                                    type_namespace,
                                    values,
                                    ordered,
                                    class_name,
                                    parent_chr,
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
    fn_call_to_create_prototype <- make_child_fn_chr(child_ext_fn_chr = fn_call_to_create_prototype,
                                                     parent_chr = parent_chr,
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

make_child_fn_chr <- function(child_ext_fn_chr,
                              parent_chr,
                              prototype_lup,
                              prepend_lgl = T){
  if(!is.null(parent_chr)){
    parent_proto_fn_chr <- get_parent_proto_fn_chr(parent_chr = parent_chr,
                                                   prototype_lup = prototype_lup)
    child_extension_tb <- eval(parse(text=child_ext_fn_chr))
    new_fn_chr <-paste0("purrr::reduce(names(",
                        child_ext_fn_chr,
                        "),\n.init = ",
                        parent_proto_fn_chr,
                        ",\n ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := eval(parse(text=",
                        child_ext_fn_chr,
                        "[.y]))))")
    if(prepend_lgl)
      new_fn_chr <-paste0(new_fn_chr,
                          "%>% dplyr::select(c(",
                          c(names(child_extension_tb),names(parse(text = parent_proto_fn_chr) %>% eval())) %>%
                            stringr::str_c(collapse = ","),
                          "))")
    new_fn_chr
  }else{
    child_ext_fn_chr
  }
}

make_ready_s3_constructor <- function(type,
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

make_ready_s3_validator <- function(type,
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

make_ready_s3_valid_instance <- function(class_name,
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

make_ready_s3_checker <- function(class_name,
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
