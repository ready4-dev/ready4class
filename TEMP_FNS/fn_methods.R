#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param methods_tb PARAM_DESCRIPTION
#' @param fn_ls PARAM_DESCRIPTION
#' @param package_chr PARAM_DESCRIPTION
#' @param output_dir_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname write_mthds_for_r3_or_r4_clss
#' @export
#' @importFrom purrr pwalk
#' @importFrom dplyr mutate
write_mthds_for_r3_or_r4_clss <- function(methods_tb,
                        fn_ls,
                        package_chr,
                        output_dir_chr){
  purrr::pwalk(methods_tb %>%
                 dplyr::mutate(first_lgl = c(T,rep(F,length(fn_ls)-1))) %>%
                 dplyr::mutate(append_lgl = c(F,rep(T,length(fn_ls)-1))),
               ~ write_std_mthd(fn = fn_ls[[..1]],
                                  fn_name_chr = ..2,
                                  class_chr = ..3,
                                  fn_desc_chr_vec = c(..4,..5),
                                  fn_title_chr = ..6,
                                  fn_out_type_chr = ..7,
                                  package_chr = package_chr,
                                  output_dir_chr = output_dir_chr,
                                  signature_chr = ..8,
                                  append_lgl = ..10,
                                  first_lgl = ..9))
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @param fn_name_chr PARAM_DESCRIPTION
#' @param class_chr PARAM_DESCRIPTION
#' @param fn_desc_chr_vec PARAM_DESCRIPTION
#' @param fn_title_chr PARAM_DESCRIPTION
#' @param fn_out_type_chr PARAM_DESCRIPTION
#' @param package_chr PARAM_DESCRIPTION
#' @param output_dir_chr PARAM_DESCRIPTION
#' @param signature_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param append_lgl PARAM_DESCRIPTION, Default: T
#' @param first_lgl PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[testit]{assert}}
#'  \code{\link[purrr]{keep}}
#' @rdname write_std_mthd
#' @export
#' @importFrom testit assert
#' @importFrom purrr discard
write_std_mthd <- function(fn,
                             fn_name_chr,
                             class_chr,
                             fn_desc_chr_vec,
                             fn_title_chr,
                             fn_out_type_chr,
                             package_chr,
                             output_dir_chr,
                             signature_chr = NA_character_, ## Add required package here.
                             append_lgl = T,
                             first_lgl = T){
  s3_lgl = !isS4(eval(parse(text=paste0(class_chr,"()"))))
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
  current_generics_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(required_pckg_chr_vec = NA_character_, # Add ready4 here
                                                 generic_chr = fn_name_chr,
                                                 ignore_ns_chr = ifelse(package_chr %in% rownames(installed.packages()),
                                                                        package_chr,
                                                                        NA_character_))
  ## NB: Ensure latest ready4 bundle (ready4dev and ready4mod) is installed.
  import_packages_ls <- make_ls_of_pkgs_to_imp(current_generics_ls = current_generics_ls,
                                                fn_name_chr = fn_name_chr,
                                                ignore_ns_chr = ifelse(package_chr %in% rownames(installed.packages()),
                                                                       package_chr,
                                                                       NA_character_))
  generic_exists_lgl = import_packages_ls$gen_get_exists_lgl
  import_chr_vec = import_packages_ls$getter_import_pckg
  write_file_ls <- write_scripts_to_make_gnrc_and_mthd(fn_name_chr = fn_name_chr,
                                  args_chr_vec = c("x",
                                                   ifelse(length(formalArgs(fn))>1,
                                                          "...",
                                                          NA_character_)) %>%
                                    purrr::discard(is.na),
                                  signature_chr = signature_chr,
                                  package_chr = NA_character_,
                                  where_chr = NA_character_,
                                  class_chr = class_chr,
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn_name_chr PARAM_DESCRIPTION
#' @param args_chr_vec PARAM_DESCRIPTION, Default: c("x")
#' @param signature_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param package_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param where_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param class_chr PARAM_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @param fn_type_chr_vec PARAM_DESCRIPTION
#' @param fn_desc_chr_vec PARAM_DESCRIPTION, Default: rep(NA_character_, 2)
#' @param fn_title_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param fn_out_type_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param import_chr_vec PARAM_DESCRIPTION
#' @param write_file_ls PARAM_DESCRIPTION
#' @param output_dir_chr PARAM_DESCRIPTION
#' @param append_lgl PARAM_DESCRIPTION, Default: T
#' @param doc_in_class_lgl PARAM_DESCRIPTION, Default: F
#' @param generic_exists_lgl PARAM_DESCRIPTION
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @param s3_lgl PARAM_DESCRIPTION
#' @param write_lgl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname write_scripts_to_make_gnrc_and_mthd
#' @export
write_scripts_to_make_gnrc_and_mthd <- function(fn_name_chr,
                           args_chr_vec = c("x"),
                           signature_chr = NA_character_,
                           package_chr = NA_character_ ,
                           where_chr = NA_character_,
                           class_chr,
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
                                            package_chr = package_chr,
                                            where_chr = where_chr,
                                            class_chr = class_chr,
                                            fn = fn)
  write_file_ls <- write_script_to_make_gnrc(write_file_ls = write_file_ls,
                                    generic_exists_lgl = generic_exists_lgl,
                                    gen_mthd_pair_ls = gen_mthd_pair_ls,
                                    fn_name_chr = fn_name_chr,
                                    fn_type_chr = fn_type_chr_vec[1],
                                    fn_desc_chr = fn_desc_chr_vec[1],
                                    fn_out_type_chr = NA_character_,
                                    fn_title_chr = fn_title_chr,
                                    output_dir_chr = output_dir_chr,
                                    overwrite_lgl = overwrite_lgl,
                                    s3_lgl = s3_lgl,
                                    write_lgl = write_lgl,
                                    doc_in_class_lgl = doc_in_class_lgl)
  write_file_ls$new_file_lgl <- ifelse(!overwrite_lgl,T,write_file_ls$new_file_lgl)
  write_script_to_make_mthd(write_file_ls = write_file_ls,
               gen_mthd_pair_ls = gen_mthd_pair_ls,
               class_name_chr = class_chr,
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param write_file_ls PARAM_DESCRIPTION
#' @param generic_exists_lgl PARAM_DESCRIPTION
#' @param gen_mthd_pair_ls PARAM_DESCRIPTION
#' @param fn_name_chr PARAM_DESCRIPTION
#' @param fn_type_chr PARAM_DESCRIPTION
#' @param fn_desc_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param fn_out_type_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param fn_title_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param class_name_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param output_dir_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @param s3_lgl PARAM_DESCRIPTION, Default: F
#' @param write_lgl PARAM_DESCRIPTION, Default: T
#' @param doc_in_class_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[ready4fun]{close_open_sinks}}
#' @rdname write_script_to_make_gnrc
#' @export
#' @importFrom stringr str_replace
#' @importFrom ready4fun close_open_sinks
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
    eval(parse(text = gen_mthd_pair_ls$generic_chr))
    if(write_lgl & (!file.exists(write_file_ls$gnr_file) | write_file_ls$new_file_lgl | overwrite_lgl)){
      sink(write_file_ls$gnr_file,
           append = ifelse(fn_type_chr %in% c("gen_std_s3_mthd",
                                              "gen_std_s4_mthd"),F,write_file_ls$new_file_lgl))
      ready4fun::write_fn_dmt(fn_name_chr = fn_name_chr,
                     fn_type_chr = fn_type_chr,
                     fn = gen_mthd_pair_ls$gen_fn_chr,
                     fn_desc_chr = fn_desc_chr,
                     fn_out_type_chr = fn_out_type_chr,
                     fn_title_chr = fn_title_chr,
                     doc_in_class_lgl = doc_in_class_lgl)
      writeLines(gen_mthd_pair_ls$generic_chr %>% stringr::str_replace(paste0(",\nwhere =  ",
                                                                              "globalenv\\(\\)"),""))
      ready4fun::close_open_sinks()
      write_file_ls$new_file_lgl <- T
    }
    write_file_ls$meth_file <- write_file_ls$gnr_file
  }else{
    if(else_lgl){
      write_file_ls$meth_file <- get_class_fl_nms(class_names_chr_vec = class_name_chr,
                                                     s3_lgl = s3_lgl,
                                                     output_dir_chr = output_dir_chr)
    }
  }
  write_file_ls
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param write_file_ls PARAM_DESCRIPTION
#' @param gen_mthd_pair_ls PARAM_DESCRIPTION
#' @param class_name_chr PARAM_DESCRIPTION
#' @param fn_name_chr PARAM_DESCRIPTION
#' @param fn_type_chr PARAM_DESCRIPTION
#' @param fn_desc_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param fn_out_type_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param import_chr_vec PARAM_DESCRIPTION
#' @param write_lgl PARAM_DESCRIPTION, Default: T
#' @param append_lgl PARAM_DESCRIPTION, Default: T
#' @param doc_in_class_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[ready4fun]{close_open_sinks}}
#' @rdname write_script_to_make_mthd
#' @export
#' @importFrom stringr str_replace
#' @importFrom ready4fun close_open_sinks
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
    ready4fun::write_fn_dmt(fn_name_chr = fn_name_chr,
                   fn_type_chr = fn_type_chr,
                   fn = gen_mthd_pair_ls$meth_fn_chr,
                   fn_desc_chr = fn_desc_chr,
                   fn_out_type_chr = fn_out_type_chr,
                   class_name_chr = class_name_chr,
                   import_chr_vec = import_chr_vec,
                   doc_in_class_lgl = doc_in_class_lgl)
    writeLines(gen_mthd_pair_ls$method_chr %>% stringr::str_replace(paste0(",\nwhere =  ",
                                                                           "globalenv\\(\\)"),""))
    ready4fun::close_open_sinks()
  }
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_chr PARAM_DESCRIPTION
#' @param args_chr_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_gnrc_fn
#' @export
make_gnrc_fn <- function(name_chr,
                            args_chr_vec){
  if(all(!is.na(args_chr_vec))){
    paste0('function(',paste0(args_chr_vec, collapse = ", "),') standardGeneric("', name_chr,'")')
  }else{
    ""
  }
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname transform_fn_into_chr
#' @export
transform_fn_into_chr <- function(fn){
  deparse(fn) %>% paste0(collapse="\n")
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_chr PARAM_DESCRIPTION
#' @param args_chr_vec PARAM_DESCRIPTION, Default: c("x")
#' @param signature_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param where_chr PARAM_DESCRIPTION, Default: 'NA'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_alg_to_set_gnrc
#' @export
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_chr PARAM_DESCRIPTION
#' @param class_chr PARAM_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @param package_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param where_chr PARAM_DESCRIPTION, Default: 'NA'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_alg_to_set_mthd
#' @export
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name_chr PARAM_DESCRIPTION
#' @param args_chr_vec PARAM_DESCRIPTION, Default: c("x")
#' @param signature_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param package_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param where_chr PARAM_DESCRIPTION, Default: 'NA'
#' @param class_chr PARAM_DESCRIPTION
#' @param fn PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_gnrc_mthd_pair_ls
#' @export
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param class_type_mk_ls PARAM_DESCRIPTION
#' @param make_s3_lgl PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{flatten}}
#'  \code{\link[testit]{assert}}
#'  \code{\link[rlang]{exec}}
#'  \code{\link[tibble]{add_row}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{mutate_all}}
#' @rdname make_one_row_class_pt_tb
#' @export
#' @importFrom purrr reduce flatten
#' @importFrom testit assert
#' @importFrom rlang exec
#' @importFrom tibble add_case
#' @importFrom dplyr mutate mutate_at
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param class_mk_ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#' @rdname make_class_pts_tb
#' @export
#' @importFrom purrr map2_dfr
make_class_pts_tb <- function(class_mk_ls){
  purrr::map2_dfr(class_mk_ls,
                  names(class_mk_ls),
                  ~ make_one_row_class_pt_tb(.x,
                                          make_s3_lgl = ifelse(.y=="s3_ls",T,F))

  )
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param class_mk_ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#'  \code{\link[rlang]{exec}}
#' @rdname make_class_pt_tb_for_r3_and_r4_clss
#' @export
#' @importFrom purrr map2_dfr
#' @importFrom rlang exec
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname make_pt_tb_for_new_r3_cls
#' @export
#' @importFrom purrr map_dfr
make_pt_tb_for_new_r3_cls <- function(x){
  purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r3_cls(.x))
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname make_pt_tb_for_new_r4_cls
#' @export
#' @importFrom purrr map_dfr
make_pt_tb_for_new_r4_cls <- function(x){
  purrr::map_dfr(x,
                 ~make_one_row_pt_tb_for_new_r4_cls(.x))
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_one_row_pt_tb_for_new_r3_cls
#' @export
make_one_row_pt_tb_for_new_r3_cls <- function(x){
  make_one_row_class_pt_tb(list(name_stub = x@name_stub_chr,
                             prototype = x@prototype_ls,
                             prototype_checker_prefix = x@prototype_chk_pfx_ls,
                             prototype_namespace = x@prototype_ns_ls,
                             values = x@values_ls,
                             allowed_values = x@allowed_vals_ls,
                             min_max_values = x@min_max_values_ls,
                             start_end_values = x@start_end_values_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             include_classes = x@include_classes_ls) %>% list(),
                        make_s3_lgl = T)
}
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_one_row_pt_tb_for_new_r4_cls
#' @export
make_one_row_pt_tb_for_new_r4_cls <- function(x){
  make_one_row_class_pt_tb(list(name_stub = x@name_stub_chr,
                             prototype = x@prototype_ls,
                             values = x@values_ls,
                             allowed_values = x@allowed_vals_ls,
                             class_desc = x@class_desc_chr,
                             parent_class = x@parent_class_chr,
                             class_slots = x@class_slots_ls,
                             meaningful_names = x@meaningful_names_ls,
                             include_classes = x@include_classes_ls) %>% list(),
                        make_s3_lgl = F)
}
