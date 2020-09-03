make_alg_to_gen_ref_to_cls <- function(class_chr,
                                       package_chr = ".GlobalEnv"){
  paste0("methods::className(\"",
         class_chr,
         "\",\"",
         package_chr,
         "\")")
}
make_alg_to_set_gnrc <- function(name_chr,
                                 args_chr_vec = c("x"),
                                 signature_chr = NA_character_,
                                 where_chr = NA_character_){
  paste0('methods::setGeneric(\"', name_chr,'\"',
         ifelse(is.na(args_chr_vec[1]),
                '',
                paste0(', ',make_gnrc_fn(name_chr,args_chr_vec = args_chr_vec))),
         ifelse(is.na(where_chr[1]),'',paste0(',\nwhere =  ', where_chr)),
         ifelse(is.na(signature_chr[1]),'',paste0(',\nsignature =  \"', signature_chr,'\"')),
         ')' )
}
make_alg_to_get_pt_val <- function(type_namespace = "",
                                   function_to_call = "",
                                   default_value = "",
                                   namespace_contexts = c("base")){
  paste0(ifelse(type_namespace %in% namespace_contexts,
                "",
                paste0(type_namespace,"::")),
         function_to_call,
         ifelse(function_to_call=="",
                "",
                "("),
         default_value,
         ifelse(function_to_call=="",
                "",
                ")")
  )
}
make_alg_to_set_mthd <- function(name_chr,
                                 class_chr,
                                 fn,
                                 package_chr = NA_character_ ,
                                 where_chr = NA_character_){
  paste0('methods::setMethod(\"', name_chr, '\"',
         ', ',ifelse(is.na(package_chr[1]),paste0('\"',class_chr,'\"'),paste0(make_alg_to_gen_ref_to_cls(class_chr,package_chr=package_chr))),
         ', ', transform_fn_into_chr(fn),
         ifelse(is.na(where_chr[1]),'',paste0(',\nwhere =  ', where_chr)),
         ')')
}
make_alg_to_set_old_clss <- function(type,
                                     prototype_lup){
  index_of_s3 <- purrr::map_lgl(type,
                                ~ ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                              match_var_nm_chr = "type",
                                                              match_value_xx = .x,
                                                              target_var_nm_chr = "old_class",
                                                              evaluate_lgl = FALSE)
  )
  if(!identical(type[index_of_s3],character(0))){
    purrr::map_chr(type[index_of_s3],
                   ~ paste0("setOldClass(c(\"",
                            .x,
                            "\",\"tbl_df\", \"tbl\", \"data.frame\")",
                            ",where =  ",
                            "globalenv()",
                            ")")) %>% stringr::str_c(sep="",collapse="\n")
  }else{
    character(0)
  }
  # purrr::walk(type[index_of_s3],
  #             ~ setOldClass(.x, where = globalenv()))

}
make_alg_to_set_validity_of_r4_cls <- function(class_name,
                                               parent,
                                               not_same_length = NULL,
                                               allowed_values = NULL,
                                               names_include = NULL,
                                               print_validator = FALSE){
  same_lngth_cond <- allowed_cond_vec <- names_include_vec <- NA_character_
  all_slots <- ready4fun::get_r4_obj_slots_vec(class_name) %>% names()
  if(!is.null(parent)){
    parental_slots <- ready4fun::get_r4_obj_slots_vec(parent) %>% names()
    all_slots <- all_slots[! all_slots %in% parental_slots]
  }
  if(!is.null(not_same_length)){
    same_length_slots <- all_slots[! all_slots %in% not_same_length]
    if(!identical(same_length_slots, character(0))){
      slot_ls <- purrr::map_chr(same_length_slots,
                                ~ paste0('object@',
                                         .x)) %>%
        stringr::str_c(sep="",collapse=",") %>%
        paste0("list(",.,")")
      same_lngth_cond <- paste0("if(length(unique(lengths(",
                                slot_ls,
                                "))) > 1)\n",
                                "msg <- c(msg, ",
                                "\"",
                                same_length_slots %>%
                                  stringr::str_c(sep="",collapse=", ") %>%
                                  stringi::stri_replace_last(fixed=",", replacement = " and"),
                                " must all be of the same length.\")"
      )
    }
  }
  if(!is.null(allowed_values)){
    allowed_cond_vec <- purrr::map2_chr(names(allowed_values),
                                        allowed_values,
                                        ~ paste0("if(!identical(",
                                                 "object@",
                                                 .x,
                                                 "[! object@",
                                                 .x,
                                                 " %in% ",
                                                 ifelse(is.character(.y),"\"",""),
                                                 .y,
                                                 ifelse(is.character(.y),"\"",""),
                                                 "],character(0))){\n",
                                                 "msg <- c(msg, ",
                                                 "\"",
                                                 .x,
                                                 " slot can only include the following values: ",
                                                 .y,
                                                 "\")\n}"))
  }
  if(!is.null(names_include)){
    names_include_conc <- purrr::map_chr(names_include,
                                         ~ paste0("c(\"",
                                                  .x %>%
                                                    stringr::str_c(sep="",collapse="\",\""),
                                                  "\")"))
    names_include_vec <- purrr::map2_chr(names(names_include),
                                         names_include_conc,
                                         ~ paste0("if(!identical(",
                                                  .y ,
                                                  "[",
                                                  .y ,
                                                  " %in% ",
                                                  "names(object@",
                                                  .x,
                                                  ")],",
                                                  .y,
                                                  ")){\n",
                                                  "msg <- c(msg, ",
                                                  "\"",
                                                  .x,
                                                  " slot object names can only include the following values: ",
                                                  .y %>% stringr::str_replace_all("\"","") %>%
                                                    stringr::str_replace("c\\(","") %>%
                                                    stringr::str_replace("\\)",""),
                                                  "\")\n}"))
  }
  ### Adapts:
  ## https://stackoverflow.com/questions/27744214/how-to-use-validity-functions-correctly-with-inherited-s4-classes-in-r
  valid_function <- paste0("function(object){\n",
                           "msg <- NULL\n",
                           ifelse(is.na(same_lngth_cond),"",paste0(same_lngth_cond,"\n")),
                           ifelse(is.na(allowed_cond_vec),"",allowed_cond_vec), ## POTENTIAL ERROR - VECTOR ARGUMENT TO IFELSE
                           ifelse(is.na(names_include_vec),"",names_include_vec), ## POTENTIAL ERROR - VECTOR ARGUMENT TO IFELSE
                           "if (is.null(msg)) TRUE else msg",
                           "\n}")
  paste0("methods::setValidity(",
         make_alg_to_gen_ref_to_cls(class_name),
         ",\n",
         valid_function,
         ",\nwhere =  ",
         "globalenv()",
         ")")
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
  accessors <- paste0("write_gtr_str_mthds_for_slots(",
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
make_child_cls_fn_body <- function(child_ext_fn_chr,
                                   parent_chr,
                                   prototype_lup,
                                   prepend_lgl = T){
  if(!is.null(parent_chr)){
    parent_proto_fn_chr <- get_parent_cls_pt_fn(parent_chr = parent_chr,
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
make_class_pt_tb_for_r3_and_r4_clss <- function(class_mk_ls){
  purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ {
                    if(.y=="s3_ls"){
                      fn = make_pt_tb_for_new_r3_cls
                    }else{
                      fn = make_pt_tb_for_new_r4_cls
                    }
                    rlang::exec(fn,.x)
                  })
}
make_class_pts_tb <- function(class_mk_ls){
  purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ make_one_row_class_pt_tb(.x,
                                             make_s3_lgl = ifelse(.y=="s3_ls",T,F))

  )
}
make_dmt_inc_tag <- function(class_names_chr_vec,
                             s3_lgl = T){
  ifelse(!is.null(class_names_chr_vec),
         paste0("#' @include ",get_class_fl_nms(class_names_chr_vec = class_names_chr_vec, s3_lgl = s3_lgl) %>% stringr::str_c(collapse=" "),"\n"),
         "")
}
make_gnrc_fn <- function(name_chr,
                            args_chr_vec){
  if(all(!is.na(args_chr_vec))){
    paste0('function(',paste0(args_chr_vec, collapse = ", "),') standardGeneric("', name_chr,'")')
  }else{
    ""
  }
}
make_gnrc_mthd_pair_ls <- function(name_chr,
                                  args_chr_vec = c("x"),
                                  signature_chr = NA_character_,
                                  package_chr = NA_character_ ,
                                  where_chr = NA_character_,
                                  class_chr,
                                  fn){
  list(generic_chr = make_alg_to_set_gnrc(name_chr,
                                      args_chr_vec = args_chr_vec,
                                      signature_chr = signature_chr,
                                      where_chr = where_chr),
       method_chr = make_alg_to_set_mthd(name_chr,
                                    class_chr = class_chr,
                                    fn = fn,
                                    package_chr = package_chr,
                                    where_chr = where_chr),
       gen_fn_chr = make_gnrc_fn(name_chr,
                                    args_chr_vec = args_chr_vec),
       meth_fn_chr = transform_fn_into_chr(fn))
}
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
make_lines_for_writing_dmtd_fn <- function(fn_name,
                                           fn_text,
                                           fn_type,
                                           class_name,
                                           class_desc){
  ready4fun::write_fn_dmt(fn_name_chr = fn_name,
                          fn_type_chr = fn_type,
                          fn = eval(parse(text = fn_text)),
                          class_name_chr = class_name,
                          details_chr = class_desc)
  writeLines(fn_text)
}
make_ls_of_pkgs_to_imp <- function(current_generics_ls,
                                   fn_name_chr,
                                   ignore_ns_chr){
  package_chr_vec <- current_generics_ls$package_chr_vec[!current_generics_ls$package_chr_vec %in% c(".GlobalEnv")]
  gen_get_exists_lgl_vec <- purrr::map2_lgl(package_chr_vec, names(package_chr_vec), ~ ((.x == fn_name_chr | .y == paste0(fn_name_chr,".",.x)) & !.x %in% ignore_ns_chr))
  gen_get_exists_lgl <- any(gen_get_exists_lgl_vec)
  getter_import_pckg <- ifelse(gen_get_exists_lgl,package_chr_vec[gen_get_exists_lgl_vec],NA_character_)
  gen_set_exists_lgl_vec <- purrr::map2_lgl(package_chr_vec, names(package_chr_vec), ~ ((.x == paste0(fn_name_chr,"<-") | .y == paste0(fn_name_chr,"<-.",.x)) & !.x %in% ignore_ns_chr))
  gen_set_exists_lgl <- any(gen_set_exists_lgl_vec)
  setter_import_pckg <- ifelse(gen_set_exists_lgl,package_chr_vec[gen_set_exists_lgl_vec],NA_character_)
  list(getter_import_pckg = getter_import_pckg[getter_import_pckg != ignore_ns_chr[1]],
       setter_import_pckg = setter_import_pckg[setter_import_pckg != ignore_ns_chr[1]],
       gen_get_exists_lgl = gen_get_exists_lgl,
       gen_set_exists_lgl = gen_set_exists_lgl)
}
make_ls_of_tfd_nms_of_curr_gnrcs <- function(required_pckg_chr_vec,
                                             generic_chr,
                                             ignore_ns_chr){
  current_generics_ls <- get_nms_of_curr_gnrcs(required_pckg_chr_vec = required_pckg_chr_vec,
                                               generic_chr = generic_chr)
  if(is.na(ignore_ns_chr[1])){
    dependencies_chr_vec <- character(0)
  }else{
    dependencies_chr_vec <- gtools::getDependencies(ignore_ns_chr[1])
  }
  if(!required_pckg_chr_vec %>% purrr::discard(is.na) %>% identical(character(0)))
    required_pckg_chr_vec <- required_pckg_chr_vec[!required_pckg_chr_vec %in% dependencies_chr_vec]
  if(current_generics_ls$in_global_lgl){
    ready4fun::unload_packages(package_chr_vec = required_pckg_chr_vec[required_pckg_chr_vec != ignore_ns_chr[1]])
    current_generics_ls <- get_nms_of_curr_gnrcs(required_pckg_chr_vec = required_pckg_chr_vec,
                                                 generic_chr = generic_chr)
  }
  current_generics_ls
}
make_one_row_class_pt_tb <- function(class_type_mk_ls,
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
make_one_row_pt_tb_for_new_r3_cls <- function(x){
  make_one_row_class_pt_tb(list(name_stub = x@name_stub_chr,
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
make_one_row_pt_tb_for_new_r4_cls <- function(x){
  make_one_row_class_pt_tb(list(name_stub = x@name_stub_chr,
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
make_pt_ls <- function(class_slots,
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
make_pt_ls_for_new_r3_cls <- function(class_name_chr,
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
  s3_prototype <- write_scripts_to_mk_r3_clss_pts(type = type_chr,
                                          type_namespace = type_ns_chr,
                                          values = values_chr,
                                          ordered = ordered_lgl,
                                          class_name = class_name_chr,
                                          parent_chr = parent_chr,
                                          prototype_lup = prototype_lup)
  s3_constructor <- write_scripts_to_mk_r3_clss_constructor(type = type_chr,
                                              type_checker_prefix = type_checker_pfx_chr,
                                              type_namespace = type_ns_chr,
                                              class_name = class_name_chr,
                                              s3_prototype = s3_prototype)
  s3_validator <- write_scripts_to_mk_r3_clss_validator(type = type_chr,
                                          class_name = class_name_chr,
                                          s3_prototype = s3_prototype,
                                          min_max_values = min_max_values_dbl_vec,
                                          start_end_values = start_end_values_dbl_vec,
                                          values = values_chr)
  s3_valid_instance <- write_scripts_to_mk_r3_clss_valid_instance(class_name = class_name_chr,
                                                    s3_prototype = s3_prototype,
                                                    s3_constructor = s3_constructor,
                                                    s3_validator = s3_validator)
  s3_checker <- write_scripts_to_mk_r3_clss_checker(class_name = class_name_chr,
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
  include_tags_chr <- get_parent_cls_ns(prototype_lup = prototype_lup,
                                       parent_chr = parent_chr,
                                       dev_pckg_ns_chr = ignore_ns_chr[1]) %>%
    get_nms_of_clss_to_inc(parent_chr = parent_chr,
                                 prespecified_includes_chr = NULL) %>%
    make_dmt_inc_tag(s3_lgl = T)
  list(fn_name_ls = fn_name_ls,
       fn_text_ls = fn_text_ls,
       include_tags_chr = include_tags_chr)
}
make_pt_tb_for_new_r3_cls <- function(x){
  purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r3_cls(.x))
}
make_pt_tb_for_new_r4_cls <- function(x){
  purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r4_cls(.x))
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
