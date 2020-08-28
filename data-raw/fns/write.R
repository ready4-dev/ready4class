write_s3_s4 <- function(methods_tb,
                        fn_ls,
                        package_chr,
                        output_dir_chr){
  purrr::pwalk(methods_tb %>%
                 dplyr::mutate(first_lgl = c(T,rep(F,length(fn_ls)-1))) %>%
                 dplyr::mutate(append_lgl = c(F,rep(T,length(fn_ls)-1))),
               ~ write_std_method(fn = fn_ls[[..1]],
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
write_std_method <- function(fn,
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
  current_generics_ls <- make_and_tf_curr_gen_ls(required_pckg_chr_vec = NA_character_, # Add ready4 here
                                                 generic_chr = fn_name_chr,
                                                 ignore_ns_chr = ifelse(package_chr %in% rownames(installed.packages()),
                                                                        package_chr,
                                                                        NA_character_))
  ## NB: Ensure latest ready4 bundle (ready4dev and ready4mod) is installed.
  import_packages_ls <- make_import_packages_ls(current_generics_ls = current_generics_ls,
                                                fn_name_chr = fn_name_chr,
                                                ignore_ns_chr = ifelse(package_chr %in% rownames(installed.packages()),
                                                                       package_chr,
                                                                       NA_character_))
  generic_exists_lgl = import_packages_ls$gen_get_exists_lgl
  import_chr_vec = import_packages_ls$getter_import_pckg
  write_file_ls <- write_gen_meth(fn_name_chr = fn_name_chr,
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
write_gen_meth <- function(fn_name_chr,
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
  gen_mthd_pair_ls <- make_gen_mthd_pair_ls(name_chr = fn_name_chr,
                                            args_chr_vec = args_chr_vec,
                                            signature_chr = signature_chr,
                                            package_chr = package_chr,
                                            where_chr = where_chr,
                                            class_chr = class_chr,
                                            fn = fn)
  write_file_ls <- write_generic_fn(write_file_ls = write_file_ls,
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
  write_method(write_file_ls = write_file_ls,
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
write_generic_fn <- function(write_file_ls,
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
                              fn = eval(parse(text=gen_mthd_pair_ls$gen_fn_chr)),
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
      write_file_ls$meth_file <- get_class_files_chr(class_names_chr_vec = class_name_chr,
                                                     s3_lgl = s3_lgl,
                                                     output_dir_chr = output_dir_chr)
    }
  }
  write_file_ls
}
write_method <- function(write_file_ls,
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
                            fn = eval(parse(text=gen_mthd_pair_ls$meth_fn_chr)),
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
write_accessors <- function(slot_name_chr,
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
                       setter_ls = list(fn_name_chr = slot_name_chr,
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
                               meth_file = get_class_files_chr(class_names_chr_vec = class_name,
                                                               s3_lgl = F,
                                                               output_dir_chr = output_folder)),
                  ~ write_gen_meth(fn_name_chr = .y[[1]],
                                   args_chr_vec = .y[[2]],
                                   package_chr = ".GlobalEnv",
                                   where_chr = 'globalenv()',
                                   class_chr = class_name,
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
write_fn_txt_and_tags <- function(fn_name,
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

