author.ready4class_constructor_tbl <- function(x,
                                               dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                               name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(),"_"),
                                               output_dir_1L_chr = "R",
                                               delete_cdn_ptrn_chr = NA_character_,
                                               file_exists_cdn_1L_chr = "overwrite",
                                               init_class_pt_lup = NULL,
                                               nss_to_ignore_chr = NA_character_,
                                               req_pkgs_chr = NA_character_,
                                               class_in_cache_cdn_1L_chr = "stop",
                                               abbreviations_lup,
                                               object_type_lup){
  if(is.null(init_class_pt_lup))
    init_class_pt_lup <- prototype_lup
  x <- order_tb(x, name_pfx_1L_chr)
  if(file_exists_cdn_1L_chr == "overwrite"){
    write_to_delete_gnrc_fn_fls(x,output_dir_1L_chr = output_dir_1L_chr)
    purrr::walk(delete_cdn_ptrn_chr,
                ~ write_to_delete_fls_with_ptrn(dir_1L_chr = output_dir_1L_chr,
                                                pattern_1L_chr = .x))
  }
  new_files_chr <- paste0(purrr::map_chr(x$make_s3_lgl,
                                         ~ifelse(.x,"C3_","C4_")),
                          name_pfx_1L_chr,
                          x$name_stub_chr,
                          ".R")
  consent_1L_chr <- ready4fun::make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file",
                                                                ifelse(length(new_files_chr)>1,"s "," "),
                                                                new_files_chr %>%
                                                                  paste0(collapse = ", ") %>%
                                                                  stringi::stri_replace_last(fixed = ",", " and"),
                                                                " to the directory ",
                                                                output_dir_1L_chr,
                                                                " ?"),
                                           options_chr = c("Y", "N"),
                                           force_from_opts_1L_chr = T)
  if(consent_1L_chr == "Y"){
    inst_ready4class_pt_lup <- purrr::reduce(1:nrow(x),
                                             .init = init_class_pt_lup %>% update_lup_for_ns(dev_pkg_ns_1L_chr),
                                             ~ author(.x,
                                                         row_idx_1L_int = .y,
                                                         make_tb = x,
                                                         dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr,
                                                         name_pfx_1L_chr = name_pfx_1L_chr,
                                                         output_dir_1L_chr = output_dir_1L_chr,
                                                         file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
                                                         nss_to_ignore_chr = nss_to_ignore_chr,
                                                         req_pkgs_chr = req_pkgs_chr,
                                                         class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr,
                                                         abbreviations_lup = abbreviations_lup,
                                                         object_type_lup = object_type_lup,
                                                         consent_1L_chr = consent_1L_chr))
  }else{
    inst_ready4class_pt_lup <- NULL
  }
  return(inst_ready4class_pt_lup)
}
author.ready4class_pt_lup <- function(x,
                                      row_idx_1L_int,
                                      make_tb,
                                      dev_pkg_ns_1L_chr,
                                      name_pfx_1L_chr,
                                      output_dir_1L_chr,
                                      file_exists_cdn_1L_chr,
                                      nss_to_ignore_chr = NA_character_,
                                      req_pkgs_chr = NA_character_,
                                      class_in_cache_cdn_1L_chr = "stop",
                                      abbreviations_lup,
                                      object_type_lup,
                                      consent_1L_chr = NULL){
  make_tb <- make_tb %>% dplyr::slice(row_idx_1L_int)
  if(is.null(consent_1L_chr)){
    new_files_chr <- paste0(purrr::map_chr(make_tb$make_s3_lgl,
                                           ~ifelse(.x,"C3_","C4_")),
                            name_pfx_1L_chr,
                            make_tb$name_stub_chr,
                            ".R")
    consent_1L_chr <- ready4fun::make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the file",
                                                                  ifelse(length(new_files_chr)>1,"s "," "),
                                                                  new_files_chr %>%
                                                                    paste0(collapse = ", ") %>%
                                                                    stringi::stri_replace_last(fixed = ",", " and"),
                                                                  " to the directory ",
                                                                  output_dir_1L_chr,
                                                                  " ?"),
                                             options_chr = c("Y", "N"),
                                             force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr == "Y"){
    write_classes(make_tb,
                  name_pfx_1L_chr = name_pfx_1L_chr,
                  output_dir_1L_chr = output_dir_1L_chr,
                  file_exists_cdn_1L_chr = file_exists_cdn_1L_chr,
                  prototype_lup = x,
                  nss_to_ignore_chr = c(dev_pkg_ns_1L_chr, nss_to_ignore_chr),
                  req_pkgs_chr = req_pkgs_chr,
                  class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr,
                  abbreviations_lup = abbreviations_lup,
                  object_type_lup = object_type_lup,
                  consent_1L_chr = consent_1L_chr)
    new_pt_lup <- make(make_tb,
                           dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr,
                           prefix = name_pfx_1L_chr)
    classes_to_add_chr <- new_pt_lup %>% dplyr::pull(type_chr)
    inst_ready4class_pt_lup <- x %>%
      dplyr::filter(!type_chr %in% classes_to_add_chr)  %>%
      dplyr::bind_rows(new_pt_lup)
  }else{
    inst_ready4class_pt_lup <- NULL
  }
  return(inst_ready4class_pt_lup)
}
author.ready4class_manifest <- function(x,
                                        dv_url_pfx_1L_chr = NULL,
                                        init_class_pt_lup = NULL,
                                        key_1L_chr = NULL,
                                        list_generics_1L_lgl = F,
                                        nss_to_ignore_chr = NA_character_,
                                        req_pkgs_chr = NA_character_,
                                        self_serve_1L_lgl = F,
                                        self_serve_fn_ls = NULL,
                                        server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(init_class_pt_lup)){
    if(is.null(x$pkg_setup_r3$subsequent_ls$prototype_lup)){
      x$pkg_setup_r3 <- ready4fun::add_new_cls_pts(x$pkg_setup_r3)
    }
    init_class_pt_lup <- x$pkg_setup_r3$subsequent_ls$prototype_lup
  }else{
    x$pkg_setup_r3$subsequent_ls$prototype_lup <- init_class_pt_lup
  }
  x$pkg_setup_r3$subsequent_ls$cls_fn_ls <- ready4fun::make_pt_ready4fun_fn_ls(args_ls = list(x = x$constructor_tbl_r3,
                                                                                              dev_pkg_ns_1L_chr = x$pkg_setup_r3$initial_ls$pkg_desc_ls$Package,
                                                                                              name_pfx_1L_chr = paste0(x$pkg_setup_r3$initial_ls$pkg_desc_ls$Package,"_"),
                                                                                              output_dir_1L_chr = paste0(x$pkg_setup_r3$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                                                                                              delete_cdn_ptrn_chr = NA_character_,
                                                                                              file_exists_cdn_1L_chr = "overwrite",
                                                                                              init_class_pt_lup = init_class_pt_lup,
                                                                                              nss_to_ignore_chr = nss_to_ignore_chr,
                                                                                              req_pkgs_chr = req_pkgs_chr,
                                                                                              class_in_cache_cdn_1L_chr = "stop",
                                                                                              abbreviations_lup = x$pkg_setup_r3$subsequent_ls$abbreviations_lup,
                                                                                              object_type_lup = x$pkg_setup_r3$subsequent_ls$object_type_lup),
                                                                               fn =  author.ready4class_constructor_tbl) %>%
    ready4fun::ready4fun_fn_ls()
  x_ready4fun_pkg_setup <- ready4fun::author(x$pkg_setup_r3,
                                             dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                             key_1L_chr = key_1L_chr,
                                             list_generics_1L_lgl = list_generics_1L_lgl,
                                             self_serve_1L_lgl = self_serve_1L_lgl,
                                             self_serve_fn_ls = self_serve_fn_ls,
                                             server_1L_chr = server_1L_chr)
  return(x_ready4fun_pkg_setup)
  }


