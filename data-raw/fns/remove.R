remove_class_from_cache <- function(class_nm_1L_chr,
                                    class_in_cache_cdn_1L_chr = "stop"){
  keep_going_1L_lgl <- methods::isClass(class_nm_1L_chr)
  if(class_in_cache_cdn_1L_chr == "stop" &  keep_going_1L_lgl){
    stop(paste0("A class of name \"",class_nm_1L_chr, "\" is currently in memory. You may wish to confirm that you want to create a class of this name. To do so, rerun with the class_in_cache_locig_chr argument set to 'overwrite'"))
  }
  if(class_in_cache_cdn_1L_chr == "overwrite"){
    a<-1
    while(keep_going_1L_lgl){
      a<-a+1
      print(1)
      keep_going_1L_lgl <- methods::removeClass(class_nm_1L_chr)
    }
  }
}
