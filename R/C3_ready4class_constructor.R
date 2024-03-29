
setOldClass(c("ready4class_constructor","tbl_df", "tbl", "data.frame"))
#' Class constructor table
#' @description Create a new valid instance of the Class constructor table
#' @param x A prototype for the Class constructor table, Default: make_pt_ready4class_constructor()
#' @return A validated instance of the Class constructor table
#' @details Class constructor table
#' @rdname ready4class_constructor
#' @export 
ready4class_constructor <- function(x = make_pt_ready4class_constructor()){ 
validate_ready4class_constructor(make_new_ready4class_constructor(x))
}
#' make new ready4class constructor Class constructor table
#' @description Create a new unvalidated instance of the Class constructor table
#' @param x A prototype for the Class constructor table
#' @return An unvalidated instance of the Class constructor table
#' @details Class constructor table
#' @rdname make_new_ready4class_constructor
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4class_constructor <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4class_constructor",setdiff(make_pt_ready4class_constructor() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4class constructor Class constructor table
#' @param make_s3_lgl Make S3 (a logical vector), Default: logical(0)
#' @param name_stub_chr Name stub (a character vector), Default: character(0)
#' @param pt_ls Prototype (a list), Default: list()
#' @param pt_chkr_pfx_ls Prototype checker prefix (a list), Default: list()
#' @param pt_ns_ls Prototype namespace (a list), Default: list()
#' @param vals_ls Values (a list), Default: list()
#' @param allowed_vals_ls Allowed values (a list), Default: list()
#' @param min_max_vals_ls Minimum maximum values (a list), Default: list()
#' @param start_end_vals_ls Start end values (a list), Default: list()
#' @param class_desc_chr Class description (a character vector), Default: character(0)
#' @param parent_class_chr Parent class (a character vector), Default: character(0)
#' @param slots_ls Slots (a list), Default: list()
#' @param meaningful_nms_ls Meaningful names (a list), Default: list()
#' @param inc_clss_ls Include classes (a list), Default: list()
#' @param asserts_ls Asserts (a list), Default: list()
#' @return A prototype for Class constructor table
#' 
#' @rdname ready4class_constructor
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4class_constructor <- function(make_s3_lgl = logical(0),
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
inc_clss_ls = list(),
asserts_ls = list()){ 
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
inc_clss_ls = inc_clss_ls,
asserts_ls = asserts_ls) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4class constructor Class constructor table
#' @description Validate an instance of the Class constructor table
#' @param x An unvalidated instance of the Class constructor table
#' @return A prototpe for Class constructor table
#' @details Class constructor table
#' @rdname validate_ready4class_constructor
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4class_constructor <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4class_constructor())],
names(make_pt_ready4class_constructor())))!=length(names(make_pt_ready4class_constructor()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4class_constructor()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4class_constructor() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4class_constructor())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4class_constructor() %>% 
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
#' is ready4class constructor Class constructor table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Class constructor table
#' 
#' @rdname ready4class_constructor
#' @export 
is_ready4class_constructor <- function(x) inherits(validate_ready4class_constructor(x), "ready4class_constructor")
