#' Author method applied to ready4 S3 class Manifest..
#' @description author.ready4class_manifest() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class Manifest. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class Manifest.
#' @param constructor_r3 Constructor (a ready4 S3)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: F
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return X (ready4 S4 class for package metadata required for package set-up.)
#' @rdname author-methods
#' @export 
#' @importFrom ready4fun make_pt_ready4fun_fn_ls ready4fun_fn_ls author
author.ready4class_manifest <- function (x, constructor_r3, dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, 
    list_generics_1L_lgl = F, self_serve_1L_lgl = F, self_serve_fn_ls = NULL, 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    x$pkg_setup_ls$subsequent_ls$cls_fn_ls <- ready4fun::make_pt_ready4fun_fn_ls(args_ls = list(x = constructor_r3), 
        fn = write_classes_and_make_lup.ready4class_constructor_tbl) %>% 
        ready4fun::ready4fun_fn_ls()
    x_ready4fun_pkg_setup <- ready4fun::author(x$pkg_setup_ls, 
        dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
        list_generics_1L_lgl = list_generics_1L_lgl, self_serve_1L_lgl = self_serve_1L_lgl, 
        self_serve_fn_ls = self_serve_fn_ls, server_1L_chr = server_1L_chr)
    return(x_ready4fun_pkg_setup)
}
#' @rdname author-methods
#' @aliases author,ready4class_manifest-method
methods::setMethod("author", "ready4class_manifest", author.ready4class_manifest)
