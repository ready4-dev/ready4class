# devtools::load_all()
# library(magrittr)
# write_scripts_to_mk_r3_clss(name_stub = "sp_abbreviations_lup",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = "TEST_DES",
#               parent = NULL,
#               type = "tibble",
#               type_checker_prefix = "is_",
#               type_namespace = "tibble",
#               values = list(long_name_chr = "character(0)",
#                             short_name_chr = "character(0)"),
#               allowed_values = NULL,
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
# write_scripts_to_mk_r3_clss(name_stub = "test_list",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = "TEST_DES",
#               parent = NULL,
#               type = "list",
#               type_checker_prefix = "is.",
#               type_namespace = "",
#               values = list(long_name_chr = "character(0)",
#                             val = "numeric(0)"),
#               allowed_values = NULL,
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
# write_scripts_to_mk_r3_clss(name_stub = "longitude_coord",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = NULL,
#               parent = NULL,
#               type = "double",
#               type_checker_prefix = "is.",
#               values = NULL,
#               allowed_values = NULL,
#               min_max_values = c(-180,180),
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
# write_scripts_to_mk_r3_clss(name_stub = "longitude_coord",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = NULL,
#               parent = NULL,
#               type = "double",
#               type_checker_prefix = "is.",
#               values = NULL,
#               allowed_values = NULL,
#               min_max_values = c(-180,180),
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
# write_scripts_to_mk_r3_clss(name_stub = "accom_status",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = NULL,
#               parent = NULL,
#               type = "factor",
#               type_checker_prefix = "is.",
#               values = c("stable","risk","issue","homeless"),
#               ordered = TRUE,
#               allowed_values = NULL,
#               min_max_values = c(-180,180),
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
# write_scripts_to_mk_r3_clss(name_stub = "agent_id",
#               name_prefix = "rfwn_",
#               output_folder = "s3_classes",
#               class_desc = NULL,
#               parent = NULL,
#               type = "character",
#               type_checker_prefix = "is.",
#               values = c("stable","risk","issue","homeless"),
#               ordered = TRUE,
#               allowed_values = NULL,
#               min_max_values = c(5,8),
#               start_end_values = c("agt_",NA_character_),
#               prototype_tb = NULL,
#               containing_namespace = NULL,
#               names_include = NULL,
#               file_exists_logic = "overwrite")
##

# proto_ls <- make_pt_ls(class_slots = class_slots,
#                            type = type,
#                            containing_namespace = containing_namespace,
#                            values = values,
#                            prototype_tb = prototype_tb)
# class_name <- paste0(name_prefix,name_stub)
# output_file_class_1L_chr <- paste0(output_folder,"/",class_name,".R")
# ##
# write_to_mk_r4_cls(class_name = class_name,
#                 class_slots = class_slots,
#                 type = type,
#                 proto_ls = proto_ls,
#                 parent = parent,
#                 print_set_class = print_set_class,
#                 class_desc = class_desc,
#                 output_file_class_1L_chr = output_file_class_1L_chr,
#                 include_classes = include_classes,
#                 prototype_tb = prototype_tb)
# ##
# helper_function <- make_helper_fn(class_name = class_name,
#                                        parent = parent,
#                                        class_slots = class_slots,
#                                        proto_ls = proto_ls,
#                                        containing_namespace = containing_namespace,
#                                        prototype_tb = prototype_tb)
# eval(parse(text=helper_function))
# if(print_helper){
#   sink(output_file_class_1L_chr, append = TRUE)
#   create_fun_tags(class_name,
#                   fn_type = "set_class")
#   writeLines(helper_function %>% stringr::str_replace("<<-","<-"))
#   sink()
# }
# ##
# accessors <- make_alg_to_write_gtr_str_mthds(class_name = class_name,
#                                     parent = parent,
#                                     print_accessors = print_accessors,
#                                     output_folder = output_folder)
# eval(parse(text=accessors))
# ##
# valid_txt <- make_alg_to_set_validity_of_r4_cls(class_name = class_name,
#                             parent = parent,
#                             not_same_length = not_same_length,
#                             allowed_values = allowed_values,
#                             names_include = names_include)
# if(print_validator)
#   sink(output_file_class_1L_chr, append = TRUE)
# writeLines(paste0("#'\n",
#                   "#' @param Object An S4 object of class: ",
#                   class_name,
#                   "\n",
#                   valid_txt %>% stringr::str_replace(paste0(",\nwhere =  ",
#                                                             "globalenv\\(\\)"),"")))
# sink()
# eval(parse(text=valid_txt))
# ##
# if(!is.null(meaningful_names)){
#   meaningful_txt <- make_show_mthd_fn(class_name = class_name,
#                                            meaningful_names = meaningful_names)
#   eval(parse(text = meaningful_txt))
#   if(print_meaningful_names){
#     sink(output_file_class_1L_chr, append = TRUE)
#     writeLines(paste0("#'\n",
#                       "#' Extends 'show' generic from the methods package to ",
#                       class_name,
#                       " objects\n",
#                       meaningful_txt %>%
#                         stringr::str_replace(paste0(",\nwhere =  ",
#                                                     "globalenv\\(\\)"),"") %>%
#                         stringr::str_replace_all("\\\\n\\\",","\\\\n\\\",\n") %>%
#                         stringr::str_replace("\\nsep","sep")))
#     sink()
#   }
# }
