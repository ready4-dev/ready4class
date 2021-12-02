
setOldClass(c("ready4class_pt_lup","tbl_df", "tbl", "data.frame"))
#' Prototype Lookup Table of class metadata.
#' @description Create a new valid instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @param x A prototype for the ready4 S3 class Prototype Lookup Table of class metadata., Default: make_pt_ready4class_pt_lup()
#' @return A validated instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @details ready4 S3 class Prototype Lookup Table.
#' @rdname ready4class_pt_lup
#' @export 
ready4class_pt_lup <- function(x = make_pt_ready4class_pt_lup()){ 
validate_ready4class_pt_lup(make_new_ready4class_pt_lup(x))
}
#' make new ready4class package prototype lookup table ready4 S3 class Prototype Lookup Table of class metadata.
#' @description Create a new unvalidated instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @param x A prototype for the ready4 S3 class Prototype Lookup Table of class metadata.
#' @return An unvalidated instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @details ready4 S3 class Prototype Lookup Table.
#' @rdname make_new_ready4class_pt_lup
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4class_pt_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4class_pt_lup",setdiff(make_pt_ready4class_pt_lup() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4class package prototype lookup table ready4 S3 class Prototype Lookup Table of class metadata.
#' @param type_chr Type (a character vector), Default: character(0)
#' @param val_chr Value (a character vector), Default: character(0)
#' @param pt_ns_chr Prototype namespace (a character vector), Default: character(0)
#' @param fn_to_call_chr Function to call (a character vector), Default: character(0)
#' @param default_val_chr Default value (a character vector), Default: character(0)
#' @param old_class_lgl Old class (a logical vector), Default: logical(0)
#' @return A prototype for ready4 S3 class Prototype Lookup Table of class metadata.
#' 
#' @rdname ready4class_pt_lup
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4class_pt_lup <- function(type_chr = character(0),
val_chr = character(0),
pt_ns_chr = character(0),
fn_to_call_chr = character(0),
default_val_chr = character(0),
old_class_lgl = logical(0)){ 
args_ls <- list(type_chr = type_chr,
val_chr = val_chr,
pt_ns_chr = pt_ns_chr,
fn_to_call_chr = fn_to_call_chr,
default_val_chr = default_val_chr,
old_class_lgl = old_class_lgl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4class package prototype lookup table ready4 S3 class Prototype Lookup Table of class metadata.
#' @description Validate an instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @param x An unvalidated instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' @return A prototpe for ready4 S3 class Prototype Lookup Table of class metadata.
#' @details ready4 S3 class Prototype Lookup Table.
#' @rdname validate_ready4class_pt_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4class_pt_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4class_pt_lup())],
names(make_pt_ready4class_pt_lup())))!=length(names(make_pt_ready4class_pt_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4class_pt_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4class_pt_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4class_pt_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4class_pt_lup() %>% 
dplyr::summarise_all(class) %>% 
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
#' is ready4class package prototype lookup table ready4 S3 class Prototype Lookup Table of class metadata.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class Prototype Lookup Table of class metadata.
#' 
#' @rdname ready4class_pt_lup
#' @export 
is_ready4class_pt_lup <- function(x) inherits(validate_ready4class_pt_lup(x), "ready4class_pt_lup")
