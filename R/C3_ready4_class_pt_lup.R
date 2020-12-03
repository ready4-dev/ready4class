
setOldClass(c("ready4_class_pt_lup","tbl_df", "tbl", "data.frame"))
#' readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @description Create a new valid instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param x A prototype for the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE, Default: make_prototype_ready4_class_pt_lup()
#' @return A validated instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @details Readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.
#' @rdname ready4_class_pt_lup
#' @export 

ready4_class_pt_lup <- function(x = make_prototype_ready4_class_pt_lup()){ 
validate_ready4_class_pt_lup(make_new_ready4_class_pt_lup(x))
}
#' Make new readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @description Create a new unvalidated instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param x A prototype for the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @return An unvalidated instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @details Readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.
#' @rdname make_new_ready4_class_pt_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_class_pt_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_class_pt_lup",setdiff(make_prototype_ready4_class_pt_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @description Create a new prototype for the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param type_chr Type (a character vector), Default: character(0)
#' @param val_chr Value (a character vector), Default: character(0)
#' @param pt_ns_chr Prototype namespace (a character vector), Default: character(0)
#' @param fn_to_call_chr Function to call (a character vector), Default: character(0)
#' @param default_val_chr Default value (a character vector), Default: character(0)
#' @param old_class_lgl Old class (a logical vector), Default: logical(0)
#' @return A prototype for readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @details Readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.
#' @rdname make_prototype_ready4_class_pt_lup
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_class_pt_lup <- function(type_chr = character(0),
val_chr = character(0),
pt_ns_chr = character(0),
fn_to_call_chr = character(0),
default_val_chr = character(0),
old_class_lgl = logical(0)){ 
tibble::tibble(type_chr = type_chr,
val_chr = val_chr,
pt_ns_chr = pt_ns_chr,
fn_to_call_chr = fn_to_call_chr,
default_val_chr = default_val_chr,
old_class_lgl = old_class_lgl)
}
#' Validate readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @description Validate an instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param x An unvalidated instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @return A prototpe for readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @details Readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.
#' @rdname validate_ready4_class_pt_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_class_pt_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_class_pt_lup())],
names(make_prototype_ready4_class_pt_lup())))!=length(names(make_prototype_ready4_class_pt_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_class_pt_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_class_pt_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_class_pt_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_class_pt_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_class_pt_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @description Check whether an object is a valid instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE
#' @details Readyforwhatsnext S3 class CLASS PROTOTYPE LOOKUP TABLE class of metadata of prototype classes.
#' @rdname is_ready4_class_pt_lup
#' @export 

is_ready4_class_pt_lup <- function(x) inherits(validate_ready4_class_pt_lup(x), "ready4_class_pt_lup")
