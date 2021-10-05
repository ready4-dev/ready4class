
#' ready4 S3 class Manifest.
#' @description Create a new valid instance of the ready4 S3 class Manifest.
#' @param x A prototype for the ready4 S3 class Manifest., Default: make_pt_ready4class_manifest()
#' @return A validated instance of the ready4 S3 class Manifest.
#' @details ready4 S3 class Manifest.
#' @rdname ready4class_manifest
#' @export 
ready4class_manifest <- function(x = make_pt_ready4class_manifest()){ 
validate_ready4class_manifest(make_new_ready4class_manifest(x))
}
#' Make new ready4class package manifest ready4 S3 class Manifest.
#' @description Create a new unvalidated instance of the ready4 S3 class Manifest.
#' @param x A prototype for the ready4 S3 class Manifest.
#' @return An unvalidated instance of the ready4 S3 class Manifest.
#' @details ready4 S3 class Manifest.
#' @rdname make_new_ready4class_manifest
#' @export 
make_new_ready4class_manifest <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4class_manifest",setdiff(make_pt_ready4class_manifest() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4class package manifest ready4 S3 class Manifest.
#' @description Create a new prototype for the ready4 S3 class Manifest.
#' @param manifest_r3 Manifest (a ready4 S3), Default: ready4fun::ready4fun_manifest()
#' @param constructor_r3 Constructor (a ready4 S3), Default: ready4class_constructor()
#' @return A prototype for ready4 S3 class Manifest.
#' @details ready4 S3 class Manifest.
#' @rdname make_pt_ready4class_manifest
#' @export 
#' @importFrom ready4fun ready4fun_manifest update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4class_manifest <- function(manifest_r3 = ready4fun::ready4fun_manifest(),
constructor_r3 = ready4class_constructor()){ 
args_ls <- list(manifest_r3 = manifest_r3,
constructor_r3 = constructor_r3) %>% ready4fun::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4class package manifest ready4 S3 class Manifest.
#' @description Validate an instance of the ready4 S3 class Manifest.
#' @param x An unvalidated instance of the ready4 S3 class Manifest.
#' @return A prototpe for ready4 S3 class Manifest.
#' @details ready4 S3 class Manifest.
#' @rdname validate_ready4class_manifest
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4fun transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
validate_ready4class_manifest <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4class_manifest())],
names(make_pt_ready4class_manifest())))!=length(names(make_pt_ready4class_manifest()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4class_manifest()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4class_manifest() %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4class_manifest())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4class_manifest() %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' Is ready4class package manifest ready4 S3 class Manifest.
#' @description Check whether an object is a valid instance of the ready4 S3 class Manifest.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class Manifest.
#' @details ready4 S3 class Manifest.
#' @rdname is_ready4class_manifest
#' @export 
is_ready4class_manifest <- function(x) inherits(validate_ready4class_manifest(x), "ready4class_manifest")
