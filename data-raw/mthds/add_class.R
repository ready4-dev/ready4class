add_class.ready4_class_pt_lup <- function(x,
                                          tb_row_idx,
                                          make_tb,
                                          dev_pckg_namespace,
                                          name_prefix,
                                          output_dir,
                                          file_exists_logic,
                                          ignore_ns_chr = NA_character_,
                                          req_pkgs_chr = NA_character_,
                                          class_in_cache_logic_chr = "stop"){
  make_tb <- make_tb %>% dplyr::slice(tb_row_idx)
  make_classes(make_tb,
               name_prefix = name_prefix,
               output_dir = output_dir,
               file_exists_logic = file_exists_logic,
               prototype_lup = x,
               ignore_ns_chr = c(dev_pckg_namespace, ignore_ns_chr),
               req_pkgs_chr = req_pkgs_chr,
               class_in_cache_logic_chr = class_in_cache_logic_chr)
  new_pt_lup <- make_lup(make_tb,
                         dev_pckg_namespace = dev_pckg_namespace,
                         prefix = name_prefix)
  classes_to_add_vec <- new_pt_lup %>% dplyr::pull(type)
  inst_of_ready4_class_pt_lup <- x %>%
    dplyr::filter(!type %in% classes_to_add_vec)  %>%
    dplyr::bind_rows(new_pt_lup)
  return(inst_of_ready4_class_pt_lup)
}
