#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4class_pt_lup() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Class prototype lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4class_pt_lup`, a Class prototype lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @seealso [ready4show::print_from_chunk()]
#' @importFrom ready4 exhibit
exhibit.ready4class_pt_lup <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr = c("Class", "Value", "Namespace", "Function", 
        "Default", "Is Old Class")
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4class_pt_lup-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4class_pt_lup", package = "ready4class"), exhibit.ready4class_pt_lup)
