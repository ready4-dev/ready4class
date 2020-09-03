make_and_update.ready4_class_make_tb  <- function(x,
                                                  dev_pckg_namespace = ready4fun::get_dev_pkg_nm(),
                                                  name_prefix = paste0(ready4fun::get_dev_pkg_nm(),"_"),
                                                  output_dir = "R",
                                                  write_to_delete_fls_with_ptrn_pattern_chr_vec = NA_character_,
                                                  file_exists_logic = "overwrite",
                                                  init_class_pt_lup = NULL,
                                                  ignore_ns_chr = NA_character_,
                                                  req_pkgs_chr = NA_character_,
                                                  class_in_cache_logic_chr = "stop"){
  if(is.null(init_class_pt_lup))
    init_class_pt_lup <- prototype_lup
  x <- order_tb(x, name_prefix)
  if(file_exists_logic == "overwrite"){
    write_to_delete_gnrc_fn_fls(x,output_dir = output_dir)
    purrr::walk(write_to_delete_fls_with_ptrn_pattern_chr_vec,
                ~ write_to_delete_fls_with_ptrn(dir_chr = output_dir,
                              pattern_chr = .x))
  }
  purrr::reduce(1:nrow(x),
                .init = init_class_pt_lup %>% update_lup_for_ns(dev_pckg_namespace),
                ~ add_class(.x,
                            tb_row_idx = .y,
                            make_tb = x,
                            dev_pckg_namespace = dev_pckg_namespace,
                            name_prefix = name_prefix,
                            output_dir = output_dir,
                            file_exists_logic = file_exists_logic,
                            ignore_ns_chr = ignore_ns_chr,
                            req_pkgs_chr = req_pkgs_chr,
                            class_in_cache_logic_chr = class_in_cache_logic_chr))
}


