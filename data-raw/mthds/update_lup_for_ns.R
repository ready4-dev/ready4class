update_lup_for_ns.ready4_class_pt_lup <- function(x,
                                                namespace_contexts){
  namespace_contexts <- c("base",namespace_contexts) %>% unique()
  x %>%
    dplyr::mutate(value = purrr::pmap_chr(dplyr::select(x,
                                                        type_namespace,
                                                        function_to_call,
                                                        default_value),
                                          ~ make_alg_to_get_pt_val(type_namespace = ..1,
                                                             function_to_call = ..2,
                                                             default_value = ..3,
                                                             namespace_contexts = namespace_contexts))
    )
}


