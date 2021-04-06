library(ready4use)
abbreviations_lup <- ready4fun::get_rds_from_dv("abbreviations_lup")
# abbreviations_lup <- abbreviations_lup %>%
#   ready4fun::update_abbr_lup(short_name_chr = c("alg","cdn","chkr","cls","col","curr","dif","gen","gnrc","idx","inc","inhtc","inst","lnt","mk","mthd","ptrn",
#                                      "ready4_constructor_tbl","ready4_class_pt_lup",
#                                      "ref","ret","tf","tfd","unvd","val","vld","vldd"),
#                   long_name_chr = c("algorithm","condition","checker","class","column","current","different","generate","generic","index","include","inheritance","instance","length","make","method","pattern",
#                                     "ready4 S3 class CLASS CONSTRUCTOR TABLE",
#                                     "ready4 S3 class CLASS PROTOTYPE LOOKUP TABLE",
#                                     "reference","return","transform","transformed","unvalidated","value","valid","validated"),
#                   no_plural_chr = c("Class Constructor Table ready4 S3 class",
#                                     "Class Prototype Lookup Table ready4 S3 class",
#                                     "transformed","unvalidated","valid","validated"),
#                   custom_plural_ls = list(class = "classes", index = "indices"))
abbreviations_lup %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
fn_type_lup_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb")
# fn_type_lup_tb <- fn_type_lup_tb %>%
#   ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
#                                                                                   fn_type_lup_tb = fn_type_lup_tb),
#                                      fn_type_desc_chr = c("Adds information about a class.",
#                                                           "Applies a Make method and then updates the output of that method.",
#                                                           "Orders a tibble.",
#                                                           "Remakes list columns.",
#                                                           "Updates a lookup table with namespace data.",
#                                                           "Writes new classes.",
#                                                           "Makes new classes and creates or updates a class prototype lookup table."),
#                                      is_generic_lgl = T,
#                                      is_method_lgl = T)
fn_type_lup_tb %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "fn_type_lup_tb",
                            desc_1L_chr = "Function type lookup table")
prototype_lup <- tibble::tibble(type_chr = c("character", "list", "logical", "numeric", "POSIXt", "sf","tbl_df","integer"), ## CHANGED FROM POSIXt
                                val_chr = c("NA_character_", "list(list())","NA","NA_real_",".POSIXct(NA_character_)", "st_sf(sf::st_sfc())","tibble::tibble()","NA_integer_"),
                                pt_ns_chr = c("base", "base", "base", "base", "base", "sf", "tibble","base"),
                                fn_to_call_chr = c("", "list", "", "", ".POSIXct", "st_sf", "tibble",""),
                                default_val_chr = c("NA_character_", "list()", "NA", "NA_real_", "NA_character_", "sf::st_sfc()", "","NA_integer_"),
                                old_class_lgl = FALSE)
prototype_lup %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "prototype_lup",
                            desc_1L_chr = "Class prototypes lookup table")
# Remember to review and then publish website via website
