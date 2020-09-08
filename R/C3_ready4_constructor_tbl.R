
#' CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @description Create a new valid instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @param x A prototype for the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class, Default: make_prototype_ready4_constructor_tbl()
#' @return A validated instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @details CLASS CONSTRUCTOR TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname ready4_constructor_tbl
#' @export 

ready4_constructor_tbl <- function(x = make_prototype_ready4_constructor_tbl()){ 
validate_ready4_constructor_tbl(make_new_ready4_constructor_tbl(x))
}
#' Make new CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @description Create a new unvalidated instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @param x A prototype for the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @return An unvalidated instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @details CLASS CONSTRUCTOR TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname make_new_ready4_constructor_tbl
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_constructor_tbl <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_constructor_tbl",setdiff(make_prototype_ready4_constructor_tbl() %>% class(),class(x))),
class(x))
x
}
#' Make prototype CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @description Create a new prototype for the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class

#' @return A prototype for CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @details CLASS CONSTRUCTOR TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname make_prototype_ready4_constructor_tbl
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_constructor_tbl <- function(){ 
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
#' Validate CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @description Validate an instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @param x An unvalidated instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @return A prototpe for CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @details CLASS CONSTRUCTOR TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname validate_ready4_constructor_tbl
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_constructor_tbl <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_constructor_tbl())],
names(make_prototype_ready4_constructor_tbl())))!=length(names(make_prototype_ready4_constructor_tbl()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_constructor_tbl()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_constructor_tbl())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @description Check whether an object is a valid instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the CLASS CONSTRUCTOR TABLE readyforwhatsnext S3 class
#' @details CLASS CONSTRUCTOR TABLE Readyforwhatsnext S3 class of metadata required to make new classes.
#' @rdname is_ready4_constructor_tbl
#' @export 

is_ready4_constructor_tbl <- function(x) inherits(validate_ready4_constructor_tbl(x), "ready4_constructor_tbl")
