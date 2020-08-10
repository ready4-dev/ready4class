gen_pt_value_chr <- function(type_namespace = "",
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
