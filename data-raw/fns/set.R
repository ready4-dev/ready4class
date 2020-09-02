set_old_classes_extend_tb <- function(type,
                                      prototype_lup){
  index_of_s3 <- purrr::map_lgl(type,
                                ~ ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup,
                                                              match_var_nm_chr = "type",
                                                              match_value_xx = .x,
                                                              target_var_nm_chr = "old_class",
                                                              evaluate_lgl = FALSE)
  )
  if(!identical(type[index_of_s3],character(0))){
    purrr::map_chr(type[index_of_s3],
                   ~ paste0("setOldClass(c(\"",
                            .x,
                            "\",\"tbl_df\", \"tbl\", \"data.frame\")",
                            ",where =  ",
                            "globalenv()",
                            ")")) %>% stringr::str_c(sep="",collapse="\n")
  }else{
    character(0)
  }
  # purrr::walk(type[index_of_s3],
  #             ~ setOldClass(.x, where = globalenv()))

}
set_ready_class <- function(class_name,
                            class_slots,
                            type,
                            proto_ls,
                            parent,
                            print_set_class,
                            class_desc,
                            output_file_class,
                            include_classes,
                            prototype_lup,
                            helper_lgl = F,
                            parent_ns_ls){
  slot_str <- purrr::map2_chr(class_slots,
                              type,
                              ~ paste0(.x,
                                       ' = "',
                                       .y,
                                       '"')) %>%
    stringr::str_c(sep="",collapse=",") %>%
    paste0("c(",.,")")
  slots <- eval(parse(text = slot_str))
  old_class_tb_extension <- set_old_classes_extend_tb(type = type,
                                                      prototype_lup = prototype_lup)
  if(!identical(old_class_tb_extension,character(0))){
    eval(parse(text = old_class_tb_extension)) ## CHECK
  }else{
    old_class_tb_extension <- ""
  }
  prototype <- eval(parse(text = proto_ls))
  if(is.null(parent)){
    st_class_fn <- paste0("methods::setClass(",
                          make_className_chr(class_name),
                          ",\nslots = ",
                          slot_str,
                          ",\nprototype =  ",
                          proto_ls,
                          ",\nwhere =  ",
                          "globalenv()",
                          ")")
  }else{
    st_class_fn <- paste0("methods::setClass(",
                          make_className_chr(class_name,
                                             package_chr = resolve_parent_ns_chr(parent_ns_ls) %>%
                                               ready4fun::update_ns()),
                          ",\ncontains = \"",
                          parent,
                          "\",\nslots = ",
                          slot_str,
                          ",\nprototype =  ",
                          proto_ls,
                          ",\nwhere =  ",
                          "globalenv()",
                          ")")
    parent_slots_chr_vec <- get_parent_slot_names(parent_chr = parent,
                                                  parent_ns_ls = parent_ns_ls)
    parent_prototype_chr_vec <- get_parent_prototypes(parent_chr = parent,
                                                      parent_ns_ls = parent_ns_ls,
                                                      slot_names_chr_vec = parent_slots_chr_vec)
    parent_prototype_chr_vec <- `names<-`(parent_prototype_chr_vec,parent_slots_chr_vec)
    slots <- c(slots, parent_prototype_chr_vec)
    slots <- slots[!duplicated(names(slots))]
  }
  slots_tags <- paste0("#' @slot ",
                       names(slots),
                       " ",
                       slots,
                       "\n",
                       collapse="")
  included_classes_chr_vec <- get_included_classes_chr_vec(parent_chr = parent,
                                                           parent_ns_ls = parent_ns_ls,
                                                           prespecified_includes_chr = include_classes)
  include_tags_chr <- make_include_tag_chr(included_classes_chr_vec, s3_lgl = F)
  if(print_set_class){
    sink(output_file_class)
    writeLines(paste0(paste0("#' ",class_name,"\n"),
                      paste0("#' @name ",class_name,"\n"),
                      "#' @description An S4 class to represent ",
                      class_desc,
                      "\n",
                      include_tags_chr,
                      old_class_tb_extension %>%
                        stringr::str_replace_all(paste0(",where =  ",
                                                        "globalenv\\(\\)"),""),
                      ifelse(old_class_tb_extension=="","","\n"),
                      slots_tags,
                      ifelse(!ifelse(is.null(parent_ns_ls$transformed_chr),
                                    F,
                                    ifelse(is.na(parent_ns_ls$transformed_chr),
                                           F,
                                           parent_ns_ls$transformed_chr!="")),
                             "",
                             paste0("#' @import ",parent_ns_ls$transformed_chr,"\n")),
                      ifelse(helper_lgl,"",paste0("#' @exportClass ",class_name,"\n")),
                      ifelse(helper_lgl,"",paste0(class_name," <- ")),
                      st_class_fn %>%
                        stringr::str_replace(paste0(",\nwhere =  ",
                                                    "globalenv\\(\\)"),"") %>%
                        simplify_class_name(package_chr = ifelse(is.null(parent),
                                                                 ".GlobalEnv",
                                                                 resolve_parent_ns_chr(parent_ns_ls) %>%
                                                                   ready4fun::update_ns())),
                      "\n"))
    ready4fun::close_open_sinks()
  }
  eval(parse(text = st_class_fn))
}
