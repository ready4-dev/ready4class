resolve_parent_ns_chr <- function(parent_ns_ls){
  if(is.null(parent_ns_ls$untransformed_chr)){
    parent_ns_ls$transformed_chr
  }else{
    ifelse(parent_ns_ls$untransformed_chr=="base",
           "base",
           parent_ns_ls$transformed_chr)
  }

}
