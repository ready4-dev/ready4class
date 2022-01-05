exhibit.ready4class_pt_lup <- function(x,
                                       caption_1L_chr = NULL,
                                       mkdn_tbl_ref_1L_chr = NULL,
                                       output_type_1L_chr = "HTML",
                                       use_lbls_as_col_nms_1L_lgl = T){
  var_desc_chr = c("Class",
                   "Value",
                   "Namespace",
                   "Function",
                   "Default",
                   "Is Old Class")
  # if(is.null(caption_1L_chr)){
  #   caption_1L_chr <- knitr::opts_current$get("tab.cap")
  # }
  # if(is.null(mkdn_tbl_ref_1L_chr)){
  #   mkdn_tbl_ref_1L_chr <- paste0("tab:",knitr::opts_current$get("tab.id"))
  # }
  # if(use_lbls_as_col_nms_1L_lgl){
  #   x <- x %>%
  #     ready4use::add_labels_from_dictionary(dictionary_tb = ready4use::make_pt_ready4use_dictionary(colnames(x),
  #                                                                                                   var_desc_chr = c("Class",
  #                                                                                                                    "Value",
  #                                                                                                                    "Namespace",
  #                                                                                                                    "Function",
  #                                                                                                                    "Default",
  #                                                                                                                    "Is Old Class")))
  # }
  x %>%
    ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr,
                                 mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                                 output_type_1L_chr = output_type_1L_chr,
                                 use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                                 var_desc_chr = var_desc_chr
                                 # use_rdocx_1L_lgl = ifelse(output_type_1L_chr=="Word",T,F)
    )
}
