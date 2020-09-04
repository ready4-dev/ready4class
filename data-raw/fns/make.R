make_alg_to_gen_ref_to_cls <- function(class_nm_1L_chr,
                                       pkg_nm_1L_chr = ".GlobalEnv"){
  alg_to_gen_ref_to_cls_1L_chr <- paste0("methods::className(\"",
         class_nm_1L_chr,
         "\",\"",
         pkg_nm_1L_chr,
         "\")")
  return(alg_to_gen_ref_to_cls_1L_chr)
}
make_alg_to_set_gnrc <- function(name_1L_chr,
                                 args_chr = c("x"),
                                 signature_1L_chr = NA_character_,
                                 where_1L_chr = NA_character_){
  alg_to_set_gnrc_1L_chr <- paste0('methods::setGeneric(\"', name_1L_chr,'\"',
         ifelse(is.na(args_chr[1]),
                '',
                paste0(', ',make_gnrc_fn(name_1L_chr,args_chr = args_chr))),
         ifelse(is.na(where_1L_chr[1]),'',paste0(',\nwhere =  ', where_1L_chr)),
         ifelse(is.na(signature_1L_chr[1]),'',paste0(',\nsignature =  \"', signature_1L_chr,'\"')),
         ')' )
  return(alg_to_set_gnrc_1L_chr)
}
make_alg_to_get_pt_val <- function(pt_ns_1L_chr = "",
                                   fn_to_call_1L_chr = "",
                                   default_val_1L_chr = "",
                                   attached_nss_chr = c("base")){
  alg_to_get_pt_val_1L_chr <- paste0(ifelse(pt_ns_1L_chr %in% attached_nss_chr,
                "",
                paste0(pt_ns_1L_chr,"::")),
         fn_to_call_1L_chr,
         ifelse(fn_to_call_1L_chr=="",
                "",
                "("),
         default_val_1L_chr,
         ifelse(fn_to_call_1L_chr=="",
                "",
                ")")
  )
  return(alg_to_get_pt_val_1L_chr)
}
make_alg_to_set_mthd <- function(name_1L_chr,
                                 class_nm_1L_chr,
                                 fn,
                                 pkg_nm_1L_chr = NA_character_ ,
                                 where_1L_chr = NA_character_){
  alg_to_set_mthd_1L_chr <- paste0('methods::setMethod(\"', name_1L_chr, '\"',
         ', ',ifelse(is.na(pkg_nm_1L_chr[1]),paste0('\"',class_nm_1L_chr,'\"'),paste0(make_alg_to_gen_ref_to_cls(class_nm_1L_chr,pkg_nm_1L_chr=pkg_nm_1L_chr))),
         ', ', transform_fn_into_chr(fn),
         ifelse(is.na(where_1L_chr[1]),'',paste0(',\nwhere =  ', where_1L_chr)),
         ')')
  return(alg_to_set_mthd_1L_chr)
}
make_alg_to_set_old_clss <- function(type_chr,
                                     prototype_lup){
  index_of_s3_lgl <- purrr::map_lgl(type_chr,
                                ~ ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                              match_var_nm_1L_chr = "type_chr",
                                                              match_value_xx = .x,
                                                              target_var_nm_1L_chr = "old_class",
                                                              evaluate_lgl = FALSE)
  )
  if(!identical(type_chr[index_of_s3_lgl],character(0))){
    alg_to_set_old_clss_1L_chr <- purrr::map_chr(type_chr[index_of_s3_lgl],
                   ~ paste0("setOldClass(c(\"",
                            .x,
                            "\",\"tbl_df\", \"tbl\", \"data.frame\")",
                            ",where =  ",
                            "globalenv()",
                            ")")) %>% stringr::str_c(sep="",collapse="\n")
  }else{
    alg_to_set_old_clss_1L_chr <- character(0)
  }
  return(alg_to_set_old_clss_1L_chr)
}
make_alg_to_set_validity_of_r4_cls <- function(class_nm_1L_chr,
                                               parent_cls_nm_1L_chr,
                                               slots_of_dif_lnts_chr = NULL,
                                               allowed_vals_ls = NULL,
                                               names_must_match_ls = NULL,
                                               print_validator_1L_lgl = FALSE){
  same_lngth_cond <- allowed_cond_vec <- names_include_vec <- NA_character_
  all_slots <- ready4fun::get_r4_obj_slots(class_nm_1L_chr) %>% names()
  if(!is.null(parent_cls_nm_1L_chr)){
    parental_slots <- ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr) %>% names()
    all_slots <- all_slots[! all_slots %in% parental_slots]
  }
  if(!is.null(slots_of_dif_lnts_chr)){
    same_length_slots <- all_slots[! all_slots %in% slots_of_dif_lnts_chr]
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
  if(!is.null(allowed_vals_ls)){
    allowed_cond_vec <- purrr::map2_chr(names(allowed_vals_ls),
                                        allowed_vals_ls,
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
  if(!is.null(names_must_match_ls)){
    names_include_conc <- purrr::map_chr(names_must_match_ls,
                                         ~ paste0("c(\"",
                                                  .x %>%
                                                    stringr::str_c(sep="",collapse="\",\""),
                                                  "\")"))
    names_include_vec <- purrr::map2_chr(names(names_must_match_ls),
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
  alg_to_set_validity_of_r4_cls_1L_chr <- paste0("methods::setValidity(",
         make_alg_to_gen_ref_to_cls(class_nm_1L_chr),
         ",\n",
         valid_function,
         ",\nwhere =  ",
         "globalenv()",
         ")")
  return(alg_to_set_validity_of_r4_cls_1L_chr)
}
make_alg_to_write_gtr_str_mthds <- function(class_nm_1L_chr,
                                            parent_cls_nm_1L_chr,
                                            print_gtrs_strs_1L_lgl,
                                            output_dir_1L_chr,
                                            nss_to_ignore_chr,
                                            req_pkgs_chr,
                                            parent_ns_ls){
  slot_names_chr <- ready4fun::get_r4_obj_slots(class_nm_1L_chr) %>% names()
  if(is.null(parent_cls_nm_1L_chr)){
    set_only <- ""
  }else{
    set_only  <- ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr,
                                          package_1L_chr = transform_parent_ns_ls(parent_ns_ls)) %>% names()
  }
  alg_to_write_gtr_str_mthds <- paste0("write_gtr_str_mthds_for_slots(",
                      "slot_names_chr = c(\"",
                      slot_names_chr %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",",
                      "set_only = c(\"",
                      set_only %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",parent_cls_nm_1L_chr = \"",
                      parent_cls_nm_1L_chr,"\",",
                      "class_nm_1L_chr = \"", class_nm_1L_chr,
                      "\", print_gtrs_strs_1L_lgl = ",
                      print_gtrs_strs_1L_lgl,
                      ",output_dir_1L_chr = \"",output_dir_1L_chr,"\"",
                      ",nss_to_ignore_chr = c(\"",
                      nss_to_ignore_chr  %>% stringr::str_c(collapse="\",\""),
                      "\")",
                      ",req_pkgs_chr = \"",req_pkgs_chr,"\"",
                      ")")
  return(alg_to_write_gtr_str_mthds)
}
make_child_cls_fn_body <- function(child_ext_fn_1L_chr,
                                   parent_cls_nm_1L_chr,
                                   prototype_lup,
                                   prepend_1L_lgll = T){
  if(!is.null(parent_cls_nm_1L_chr)){
    parent_proto_fn_chr <- get_parent_cls_pt_fn(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                                prototype_lup = prototype_lup)
    child_extension_tb <- eval(parse(text=child_ext_fn_1L_chr))
    new_fn_chr <-paste0("purrr::reduce(names(",
                        child_ext_fn_1L_chr,
                        "),\n.init = ",
                        parent_proto_fn_chr,
                        ",\n ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := eval(parse(text=",
                        child_ext_fn_1L_chr,
                        "[.y]))))")
    if(prepend_1L_lgll)
      new_fn_chr <-paste0(new_fn_chr,
                          "%>% dplyr::select(c(",
                          c(names(child_extension_tb),names(parse(text = parent_proto_fn_chr) %>% eval())) %>%
                            stringr::str_c(collapse = ","),
                          "))")
    child_cls_fn_body_1L_chr <- new_fn_chr
  }else{
    child_cls_fn_body_1L_chr <- child_ext_fn_1L_chr
  }
  return(child_cls_fn_body_1L_chr)
}
make_class_pt_tb_for_r3_and_r4_clss <- function(class_mk_ls){
  class_pt_tb_for_r3_and_r4_clss_tb <- purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ {
                    if(.y=="s3_ls"){
                      fn = make_pt_tb_for_new_r3_cls
                    }else{
                      fn = make_pt_tb_for_new_r4_cls
                    }
                    rlang::exec(fn,.x)
                  })
  return(class_pt_tb_for_r3_and_r4_clss_tb)
}
make_class_pts_tb <- function(class_mk_ls){
  class_pts_tb <- purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ make_one_row_class_pt_tb(.x,
                                             make_s3_lgl = ifelse(.y=="s3_ls",T,F))

  )
  return(class_pts_tb)
}
make_dmt_inc_tag <- function(class_names_chr,
                             s3_1L_lgl = T){
  dmt_inc_tag_1L_chr <- ifelse(!is.null(class_names_chr),
         paste0("#' @include ",get_class_fl_nms(class_names_chr = class_names_chr, s3_1L_lgl = s3_1L_lgl) %>% stringr::str_c(collapse=" "),"\n"),
         "")
  return(dmt_inc_tag_1L_chr)
}
make_gnrc_fn <- function(name_1L_chr,
                            args_chr){
  if(all(!is.na(args_chr))){
    gnrc_fn_1L_chr <- paste0('function(',paste0(args_chr, collapse = ", "),') standardGeneric("', name_1L_chr,'")')
  }else{
    gnrc_fn_1L_chr <- ""
  }
  return(gnrc_fn_1L_chr)
}
make_gnrc_mthd_pair_ls <- function(name_1L_chr,
                                  args_chr = c("x"),
                                  signature_1L_chr = NA_character_,
                                  pkg_nm_1L_chr = NA_character_ ,
                                  where_1L_chr = NA_character_,
                                  class_nm_1L_chr,
                                  fn){
  gnrc_mthd_pair_ls <- list(generic_1L_chr = make_alg_to_set_gnrc(name_1L_chr,
                                      args_chr = args_chr,
                                      signature_1L_chr = signature_1L_chr,
                                      where_1L_chr = where_1L_chr),
       method_chr = make_alg_to_set_mthd(name_1L_chr,
                                    class_nm_1L_chr = class_nm_1L_chr,
                                    fn = fn,
                                    pkg_nm_1L_chr = pkg_nm_1L_chr,
                                    where_1L_chr = where_1L_chr),
       gen_fn_chr = make_gnrc_fn(name_1L_chr,
                                    args_chr = args_chr),
       meth_fn_chr = transform_fn_into_chr(fn))
  return(gnrc_mthd_pair_ls)
}
make_helper_fn <- function(class_nm_1L_chr,
                           parent_cls_nm_1L_chr,
                           class_slots,
                           pt_ls,
                           prototype_lup,
                           parent_ns_ls){
  if(!is.null(parent_cls_nm_1L_chr)){
    child_slots_chr <- class_slots_chr
    class_slots_chr <- get_parent_cls_slot_nms(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, parent_ns_ls = parent_ns_ls)
    parent_proto <- get_parent_cls_pts(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, parent_ns_ls = parent_ns_ls, slot_names_chr = class_slots_chr)
    child_ls_chr <- pt_ls %>% stringr::str_sub(start = 6, end = -2)
    pt_ls <- make_pt_ls(class_slots_chr = class_slots_chr,
                           type_chr = parent_proto,
                           prototype_lup = prototype_lup)
    pt_ls <- paste0(pt_ls %>% stringr::str_sub(end = -2),
                       ",",
                       child_ls_chr,
                       ")")
    class_slots_chr <- c(class_slots_chr, child_slots_chr)
  }
  func_args <- pt_ls %>% stringr::str_replace("list","function") %>% stringr::str_replace_all(",",",\n")
  helper_fn_1L_chr <- paste0(class_nm_1L_chr,
                            " <- ",
                            func_args,
                            "{ \n",
                            "methods::new(\"",
                            class_nm_1L_chr,
                            "\",\n",
                            paste0(class_slots_chr," = ",class_slots_chr) %>% stringr::str_c(sep="",collapse=",\n"),
                            ")\n}")
  return(helper_fn_1L_chr)
}
make_lines_for_writing_dmtd_fn <- function(fn_name,
                                           fn_text,
                                           fn_type,
                                           class_nm_1L_chr,
                                           class_desc){
  ready4fun::write_fn_dmt(fn_name_1L_chr = fn_name,
                          fn_type_1L_chr = fn_type,
                          fn = eval(parse(text = fn_text)),
                          class_name_1L_chr = class_nm_1L_chr,
                          details_1L_chr = class_desc)
  writeLines(fn_text)
}
make_ls_of_pkgs_to_imp <- function(current_generics_ls,
                                   fn_name_1L_chr,
                                   nss_to_ignore_chr){
  packages_chr <- current_generics_ls$packages_chr[!current_generics_ls$packages_chr %in% c(".GlobalEnv")]
  gen_get_exists_lgl_vec <- purrr::map2_lgl(packages_chr, names(packages_chr), ~ ((.x == fn_name_1L_chr | .y == paste0(fn_name_1L_chr,".",.x)) & !.x %in% nss_to_ignore_chr))
  gen_get_exists_lgl <- any(gen_get_exists_lgl_vec)
  getter_import_pckg <- ifelse(gen_get_exists_lgl,packages_chr[gen_get_exists_lgl_vec],NA_character_)
  gen_set_exists_lgl_vec <- purrr::map2_lgl(packages_chr, names(packages_chr), ~ ((.x == paste0(fn_name_1L_chr,"<-") | .y == paste0(fn_name_1L_chr,"<-.",.x)) & !.x %in% nss_to_ignore_chr))
  gen_set_exists_lgl <- any(gen_set_exists_lgl_vec)
  setter_import_pckg <- ifelse(gen_set_exists_lgl,packages_chr[gen_set_exists_lgl_vec],NA_character_)
  ls_of_pkgs_to_imp_ls <- list(getter_import_pckg = getter_import_pckg[getter_import_pckg != nss_to_ignore_chr[1]],
       setter_import_pckg = setter_import_pckg[setter_import_pckg != nss_to_ignore_chr[1]],
       gen_get_exists_lgl = gen_get_exists_lgl,
       gen_set_exists_lgl = gen_set_exists_lgl)
  return(ls_of_pkgs_to_imp_ls)
}
make_ls_of_tfd_nms_of_curr_gnrcs <- function(req_pkgs_chr,
                                             generic_1L_chr,
                                             nss_to_ignore_chr){
  current_generics_ls <- get_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr,
                                               generic_1L_chr = generic_1L_chr)
  if(is.na(nss_to_ignore_chr[1])){
    dependencies_chr_vec <- character(0)
  }else{
    dependencies_chr_vec <- gtools::getDependencies(nss_to_ignore_chr[1])
  }
  if(!req_pkgs_chr %>% purrr::discard(is.na) %>% identical(character(0)))
    req_pkgs_chr <- req_pkgs_chr[!req_pkgs_chr %in% dependencies_chr_vec]
  if(current_generics_ls$in_global_1L_lgl){
    ready4fun::unload_packages(package_chr = req_pkgs_chr[req_pkgs_chr != nss_to_ignore_chr[1]])
    current_generics_ls <- get_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr,
                                                 generic_1L_chr = generic_1L_chr)
  }
  return(current_generics_ls)
}
make_one_row_class_pt_tb <- function(class_type_mk_ls,
                                  make_s3_lgl = T){
  one_row_class_pt_tb <- class_type_mk_ls  %>%
    purrr:::reduce(.init = ready4_class_make_tb(),
                   ~ {
                     testit::assert(paste0("Allowable list element names are: ", names(.x) %>% paste0(collapse = ",")),names(.y) %in% names(.x))
                     rlang::exec(tibble::add_case,.x,!!!.y)
                   }
    ) %>%
    dplyr::mutate(make_s3 = make_s3_lgl)  %>%
    remake_ls_cols()
  if(make_s3_lgl){
    one_row_class_pt_tb <- one_row_class_pt_tb %>%
      dplyr::mutate_at(c("class_slots_ls","inc_clss_ls"),
                       ~ purrr::flatten(.x))
  }
  return(one_row_class_pt_tb)
}
make_one_row_pt_tb_for_new_r3_cls <- function(x){
  one_row_class_pt_tb <- make_one_row_class_pt_tb(list(name_stub_chr = x@name_stub_chr,
                             pt_ls = x@pt_ls,
                             pt_chk_pfx_ls = x@pt_chk_pfx_ls,
                             pt_ns_ls = x@pt_ns_ls,
                             values = x@vals_ls,
                             allowed_vals_ls = x@allowed_vals_ls,
                             min_max_vals = x@min_max_vals_ls,
                             start_end_vals = x@start_end_vals_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             include_classes = x@inc_clss_ls) %>% list(),
                        make_s3_lgl = T)
  return(one_row_class_pt_tb)
}
make_one_row_pt_tb_for_new_r4_cls <- function(x){
  one_row_class_pt_tb <- make_one_row_class_pt_tb(list(name_stub_chr = x@name_stub_chr,
                             prototype = x@pt_ls,
                             values = x@vals_ls,
                             allowed_vals_ls = x@allowed_vals_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             class_slots = x@class_slots_ls,
                             meaningful_names = x@meaningful_nms_ls,
                             include_classes = x@inc_clss_ls) %>% list(),
                        make_s3_lgl = F)
  return(one_row_class_pt_tb)
}
make_pt_ls <- function(class_slots,
                       type_chr = NULL,
                       values = NULL,
                       make_val_string = TRUE,
                       prototype_lup){
  pt_ls <- purrr::map2_chr(class_slots,
                              type_chr,
                              ~ paste0(.x,
                                       ' = ',
                                       ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                                   match_var_nm_1L_chr = "type_chr",
                                                                   match_value_xx = .y,
                                                                   target_var_nm_1L_chr = "value",
                                                                   evaluate_lgl = FALSE)
                              ))
  if(!is.null(values)){
    pt_ls <- purrr::pmap_chr(list(class_slots,
                                     pt_ls,
                                     1:length(pt_ls)),
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
  pt_ls <- pt_ls %>%
    stringr::str_c(sep="",collapse=",") %>%
    paste0("list(",.,")")
  return(pt_ls)
}
make_pt_ls_for_new_r3_cls <- function(class_name_1L_chr,
                                  type_1L_chr,
                                  type_ns_chr,
                                  type_checker_pfx_chr,
                                  values_chr,
                                  ordered_lgl,
                                  parent_cls_nm_1L_chr,
                                  prototype_lup,
                                  min_max_vals_dbl_vec,
                                  start_end_vals_dbl_vec,
                                  nss_to_ignore_chr){
  s3_prototype <- write_scripts_to_mk_r3_cls_pts(type_1L_chr = type_1L_chr,
                                          pt_ns_1L_chr = type_ns_chr,
                                          values = values_chr,
                                          ordered = ordered_lgl,
                                          class_nm_1L_chr = class_name_1L_chr,
                                          parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                          prototype_lup = prototype_lup)
  s3_constructor <- write_scripts_to_mk_r3_cls_constructor(type_1L_chr = type_1L_chr,
                                              type_checker_prefix = type_checker_pfx_chr,
                                              pt_ns_1L_chr = type_ns_chr,
                                              class_nm_1L_chr = class_name_1L_chr,
                                              s3_prototype = s3_prototype)
  s3_validator <- write_scripts_to_mk_r3_cls_validator(type_1L_chr = type_1L_chr,
                                          class_nm_1L_chr = class_name_1L_chr,
                                          s3_prototype = s3_prototype,
                                          min_max_vals = min_max_vals_dbl_vec,
                                          start_end_vals = start_end_vals_dbl_vec,
                                          values = values_chr)
  s3_valid_instance <- write_scripts_to_mk_r3_cls_valid_instance(class_nm_1L_chr = class_name_1L_chr,
                                                    s3_prototype = s3_prototype,
                                                    s3_constructor = s3_constructor,
                                                    s3_validator = s3_validator)
  s3_checker <- write_scripts_to_mk_r3_cls_checker(class_nm_1L_chr = class_name_1L_chr,
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
                                       parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                       dev_pkg_ns_1L_chr = nss_to_ignore_chr[1]) %>%
    get_nms_of_clss_to_inc(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr,
                                 base_set_of_clss_to_inc_chr = NULL) %>%
    make_dmt_inc_tag(s3_1L_lgl = T)
  pt_ls_for_new_r3_cls_ls <- list(fn_name_ls = fn_name_ls,
       fn_text_ls = fn_text_ls,
       include_tags_chr = include_tags_chr)
  return(pt_ls_for_new_r3_cls_ls)
}
make_pt_tb_for_new_r3_cls <- function(x){
  pt_tb_for_new_r3_cls_tb <- purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r3_cls(.x))
  return(pt_tb_for_new_r3_cls_tb)
}
make_pt_tb_for_new_r4_cls <- function(x){
  pt_tb_for_new_r3_cls_tb <-purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r4_cls(.x))
  return(pt_tb_for_new_r3_cls_tb)
}
make_show_mthd_fn <- function(class_nm_1L_chr,
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
  show_mthd_fn_1L_chr <- paste0("methods::setMethod(\"show\",\n",
         make_alg_to_gen_ref_to_cls(class_nm_1L_chr),
         ",\n",
         function_str,
         ',\nwhere =  ',
         'globalenv()',
         "\n)")
  return(show_mthd_fn_1L_chr)
}
