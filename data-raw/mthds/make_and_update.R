make_and_update.ready4_class_make_tb  <- function(x,
                                                  dev_pckg_namespace,
                                                  name_prefix,
                                                  output_dir,
                                                  delete_files_pattern_chr_vec = NA_character_,
                                                  file_exists_logic,
                                                  init_class_pt_lup = NULL,
                                                  ignore_ns_chr = NA_character_,
                                                  required_pckg_chr_vec = NA_character_,
                                                  class_in_cache_logic_chr = "stop"){
  if(is.null(init_class_pt_lup))
    init_class_pt_lup <- prototype_lup
  x <- order_tb(x, name_prefix)
  if(file_exists_logic == "overwrite"){
    delete_getters_setters(x,output_dir = output_dir)
    purrr::walk(delete_files_pattern_chr_vec,
                ~ delete_files(dir_chr = output_dir,
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
                            required_pckg_chr_vec = required_pckg_chr_vec,
                            class_in_cache_logic_chr = class_in_cache_logic_chr))
}


