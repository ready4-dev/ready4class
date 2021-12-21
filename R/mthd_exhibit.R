#' Exhibit (print to console) an object
#' @description exhibit.ready4class_pt_lup() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Class prototype lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Class prototype lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @return NULL
#' @rdname exhibit-methods
#' @export 
#' @importFrom knitr opts_current
#' @importFrom ready4use add_labels_from_dictionary make_pt_ready4use_dictionary
#' @importFrom ready4show print_table
#' @importFrom ready4 exhibit
exhibit.ready4class_pt_lup <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T) 
{
    if (is.null(caption_1L_chr)) {
        caption_1L_chr <- knitr::opts_current$get("tab.cap")
    }
    if (is.null(mkdn_tbl_ref_1L_chr)) {
        mkdn_tbl_ref_1L_chr <- paste0("tab:", knitr::opts_current$get("tab.id"))
    }
    if (use_lbls_as_col_nms_1L_lgl) {
        x <- x %>% ready4use::add_labels_from_dictionary(dictionary_tb = ready4use::make_pt_ready4use_dictionary(colnames(x), 
            var_desc_chr = c("Class", "Value", "Namespace", "Function", 
                "Default", "Is Old Class")))
    }
    x %>% ready4show::print_table(caption_1L_chr = knitr::opts_current$get("tab.cap"), 
        mkdn_tbl_ref_1L_chr = paste0("tab:", knitr::opts_current$get("tab.id")), 
        output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        use_rdocx_1L_lgl = ifelse(output_type_1L_chr == "Word", 
            T, F))
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4class_pt_lup-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4class_pt_lup", package = "ready4class"), exhibit.ready4class_pt_lup)
