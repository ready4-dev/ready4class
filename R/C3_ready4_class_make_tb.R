
#' NA
#' @description Create a new valid instance of the S3 class: ready4_class_make_tb
#' @param x A prototype for the ready4_class_make_tb class, Default: make_prototype_ready4_class_make_tb()
#' @return A validated instance of the ready4_class_make_tb class
#' @details MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname ready4_class_make_tb
#' @export 

ready4_class_make_tb <- function(x = make_prototype_ready4_class_make_tb()){ 
validate_ready4_class_make_tb(new_ready4_class_make_tb(x))
}
#' NA
#' @description Create a new unvalidated instance of the S3 class: ready4_class_make_tb
#' @param x A prototype for the ready4_class_make_tb class
#' @return An unvalidated instance of the ready4_class_make_tb class
#' @details MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname new_ready4_class_make_tb
#' @export 
#' @importFrom tibble is_tibble
new_ready4_class_make_tb <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_class_make_tb",setdiff(make_prototype_ready4_class_make_tb() %>% class(),class(x))),
class(x))
x
}
#' NA
#' @description Create a new prototype for S3 class: ready4_class_make_tb

#' @return A prototype for ready4_class_make_tb class
#' @details MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname make_prototype_ready4_class_make_tb
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_class_make_tb <- function(){ 
tibble::tibble(make_s3_lgl = logical(0),
name_stub_chr = character(0),
pt_ls = list(),
pt_chkr_pfx_ls = list(),
pt_ns_ls = list(),
vals_ls = list(),
allowed_vals_ls = list(),
min_max_vals_ls = list(),
start_end_vals_ls = list(),
class_desc_chr = character(0),
parent_class_chr = character(0),
slots_ls = list(),
meaningful_nms_ls = list(),
inc_clss_ls = list())
}
#' NA
#' @description Validate an instance of the S3 class: ready4_class_make_tb
#' @param x An unvalidated instance of the ready4_class_make_tb class
#' @return A prototpe for ready4_class_make_tb class
#' @details MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname validate_ready4_class_make_tb
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_class_make_tb <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_class_make_tb())],
names(make_prototype_ready4_class_make_tb())))!=length(names(make_prototype_ready4_class_make_tb()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_class_make_tb()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_class_make_tb() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_class_make_tb())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_class_make_tb() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_class_make_tb() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' NA
#' @description Check whether an object is a valid instance of the S3 class: ready4_class_make_tb
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4_class_make_tb class
#' @details MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname is_ready4_class_make_tb
#' @export 

is_ready4_class_make_tb <- function(x) inherits(validate_ready4_class_make_tb(x), "ready4_class_make_tb")
