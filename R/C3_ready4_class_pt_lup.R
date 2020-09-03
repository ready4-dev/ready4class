
#' 
#' @description Create a new valid instance of the S3 class: ready4_class_pt_lup
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_class_pt_lup()
#' @return A validated instance of the ready4_class_pt_lup class
#' @details PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.
#' @rdname ready4_class_pt_lup
#' @export 

ready4_class_pt_lup <- function(x = make_prototype_ready4_class_pt_lup()){ 
validate_ready4_class_pt_lup(new_ready4_class_pt_lup(x))
}
#' 
#' @description Create a new unvalidated instance of the S3 class: new_ready4_class_pt_lup
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_class_pt_lup class
#' @details PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.
#' @rdname new_ready4_class_pt_lup
#' @export 
#' @importFrom tibble is_tibble
new_ready4_class_pt_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_class_pt_lup",setdiff(make_prototype_ready4_class_pt_lup() %>% class(),class(x))),
class(x))
x
}
#' 
#' @description Create a new prototype for S3 class: make_prototype_ready4_class_pt_lup

#' @return A prototpe for ready4_class_pt_lup class
#' @details PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.
#' @rdname make_prototype_ready4_class_pt_lup
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_class_pt_lup <- function(){ 
tibble::tibble(type = character(0),
value = character(0),
type_namespace = character(0),
function_to_call = character(0),
default_value = character(0),
old_class = logical(0))
}
#' 
#' @description Validate an instance of the S3 class: validate_ready4_class_pt_lup
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_class_pt_lup class
#' @details PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.
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
#' 
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_class_pt_lup
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_class_pt_lup class
#' @details PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.
#' @rdname is_ready4_class_pt_lup
#' @export 

is_ready4_class_pt_lup <- function(x) inherits(validate_ready4_class_pt_lup(x), "ready4_class_pt_lup")
