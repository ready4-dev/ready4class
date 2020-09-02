#' Validate ready
#' @description validate_ready() is a Validate function that validates an object. Specifically, this function implements an algorithm to validate ready.NA
#' @param class_name PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION
#' @param not_same_length PARAM_DESCRIPTION, Default: NULL
#' @param allowed_values PARAM_DESCRIPTION, Default: NULL
#' @param names_include PARAM_DESCRIPTION, Default: NULL
#' @param print_validator PARAM_DESCRIPTION, Default: FALSE
#' @return NULL
#' @rdname validate_ready
#' @export 
#' @importFrom ready4fun get_r4_obj_slots_chr_vec
#' @importFrom purrr map_chr map2_chr
#' @importFrom stringr str_c str_replace_all str_replace
#' @importFrom stringi stri_replace_last
#' @keywords internal
validate_ready <- function (class_name, parent, not_same_length = NULL, allowed_values = NULL, 
    names_include = NULL, print_validator = FALSE) 
{
    same_lngth_cond <- allowed_cond_vec <- names_include_vec <- NA_character_
    all_slots <- ready4fun::get_r4_obj_slots_vec(class_name) %>% 
        names()
    if (!is.null(parent)) {
        parental_slots <- ready4fun::get_r4_obj_slots_vec(parent) %>% 
            names()
        all_slots <- all_slots[!all_slots %in% parental_slots]
    }
    if (!is.null(not_same_length)) {
        same_length_slots <- all_slots[!all_slots %in% not_same_length]
        if (!identical(same_length_slots, character(0))) {
            slot_ls <- purrr::map_chr(same_length_slots, ~paste0("object@", 
                .x)) %>% stringr::str_c(sep = "", collapse = ",") %>% 
                paste0("list(", ., ")")
            same_lngth_cond <- paste0("if(length(unique(lengths(", 
                slot_ls, "))) > 1)\n", "msg <- c(msg, ", "\"", 
                same_length_slots %>% stringr::str_c(sep = "", 
                  collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                  replacement = " and"), " must all be of the same length.\")")
        }
    }
    if (!is.null(allowed_values)) {
        allowed_cond_vec <- purrr::map2_chr(names(allowed_values), 
            allowed_values, ~paste0("if(!identical(", "object@", 
                .x, "[! object@", .x, " %in% ", ifelse(is.character(.y), 
                  "\"", ""), .y, ifelse(is.character(.y), "\"", 
                  ""), "],character(0))){\n", "msg <- c(msg, ", 
                "\"", .x, " slot can only include the following values: ", 
                .y, "\")\n}"))
    }
    if (!is.null(names_include)) {
        names_include_conc <- purrr::map_chr(names_include, ~paste0("c(\"", 
            .x %>% stringr::str_c(sep = "", collapse = "\",\""), 
            "\")"))
        names_include_vec <- purrr::map2_chr(names(names_include), 
            names_include_conc, ~paste0("if(!identical(", .y, 
                "[", .y, " %in% ", "names(object@", .x, ")],", 
                .y, ")){\n", "msg <- c(msg, ", "\"", .x, " slot object names can only include the following values: ", 
                .y %>% stringr::str_replace_all("\"", "") %>% 
                  stringr::str_replace("c\\(", "") %>% stringr::str_replace("\\)", 
                  ""), "\")\n}"))
    }
    valid_function <- paste0("function(object){\n", "msg <- NULL\n", 
        ifelse(is.na(same_lngth_cond), "", paste0(same_lngth_cond, 
            "\n")), ifelse(is.na(allowed_cond_vec), "", allowed_cond_vec), 
        ifelse(is.na(names_include_vec), "", names_include_vec), 
        "if (is.null(msg)) TRUE else msg", "\n}")
    paste0("methods::setValidity(", make_className_chr(class_name), 
        ",\n", valid_function, ",\nwhere =  ", "globalenv()", 
        ")")
}
