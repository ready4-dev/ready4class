
setOldClass(c("ready4_constructor_tbl","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @description Create a new valid instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @param x A prototype for the ready4 S3 class CLASS CONSTRUCTOR TABLE, Default: make_pt_ready4_constructor_tbl()
#' @return A validated instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @details ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.
#' @rdname ready4_constructor_tbl
#' @export 

ready4_constructor_tbl <- function(x = make_pt_ready4_constructor_tbl()){ 
validate_ready4_constructor_tbl(make_new_ready4_constructor_tbl(x))
}
#' Make new ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @description Create a new unvalidated instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @param x A prototype for the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @return An unvalidated instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @details ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.
#' @rdname make_new_ready4_constructor_tbl
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_constructor_tbl <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_constructor_tbl",setdiff(make_pt_ready4_constructor_tbl() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @description Create a new prototype for the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @param make_s3_lgl Make S3 (a logical vector), Default: logical(0)
#' @param name_stub_chr Name stub (a character vector), Default: character(0)
#' @param pt_ls Prototype (a list), Default: list()
#' @param pt_chkr_pfx_ls Prototype checker prefix (a list), Default: list()
#' @param pt_ns_ls Prototype namespace (a list), Default: list()
#' @param vals_ls Values (a list), Default: list()
#' @param allowed_vals_ls Allowed values (a list), Default: list()
#' @param min_max_vals_ls Min max values (a list), Default: list()
#' @param start_end_vals_ls Start end values (a list), Default: list()
#' @param class_desc_chr Class description (a character vector), Default: character(0)
#' @param parent_class_chr Parent class (a character vector), Default: character(0)
#' @param slots_ls Slots (a list), Default: list()
#' @param meaningful_nms_ls Meaningful names (a list), Default: list()
#' @param inc_clss_ls Include classes (a list), Default: list()
#' @return A prototype for ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @details ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.
#' @rdname make_pt_ready4_constructor_tbl
#' @export 
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_constructor_tbl <- function(make_s3_lgl = logical(0),
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
inc_clss_ls = list()){ 
args_ls <- list(make_s3_lgl = make_s3_lgl,
name_stub_chr = name_stub_chr,
pt_ls = pt_ls,
pt_chkr_pfx_ls = pt_chkr_pfx_ls,
pt_ns_ls = pt_ns_ls,
vals_ls = vals_ls,
allowed_vals_ls = allowed_vals_ls,
min_max_vals_ls = min_max_vals_ls,
start_end_vals_ls = start_end_vals_ls,
class_desc_chr = class_desc_chr,
parent_class_chr = parent_class_chr,
slots_ls = slots_ls,
meaningful_nms_ls = meaningful_nms_ls,
inc_clss_ls = inc_clss_ls) %>% update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @description Validate an instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @param x An unvalidated instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @return A prototpe for ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @details ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.
#' @rdname validate_ready4_constructor_tbl
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_constructor_tbl <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_constructor_tbl())],
names(make_pt_ready4_constructor_tbl())))!=length(names(make_pt_ready4_constructor_tbl()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_constructor_tbl()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_constructor_tbl())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_constructor_tbl() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @description Check whether an object is a valid instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class CLASS CONSTRUCTOR TABLE
#' @details ready4 S3 class CLASS CONSTRUCTOR TABLE of metadata required to make new classes.
#' @rdname is_ready4_constructor_tbl
#' @export 

is_ready4_constructor_tbl <- function(x) inherits(validate_ready4_constructor_tbl(x), "ready4_constructor_tbl")
