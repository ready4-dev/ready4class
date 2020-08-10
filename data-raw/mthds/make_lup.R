make_lup <- function(x,
                     ...){
  UseMethod("make_lup",x)
}
make_lup.ready4_class_make_tb <- function(x,
                                          dev_pckg_namespace,
                                          prefix){
  x %>%
    dplyr::mutate(type = paste0(prefix,name_stub),
                  type_namespace = dev_pckg_namespace,
                  value = "",
                  function_to_call = type,
                  default_value = "",
                  old_class = make_s3) %>%
    dplyr::select(type,  value, type_namespace, function_to_call,default_value,old_class) %>%
    ready4_class_pt_lup() %>%
    update_lup_for_ns(namespace_contexts = dev_pckg_namespace)

}

