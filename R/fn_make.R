#' Make algorithm to generate reference to class
#' @description make_alg_to_gen_ref_to_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to generate reference to class. The function returns Algorithm to generate reference to class (a character vector of length one).
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: '.GlobalEnv'
#' @return Algorithm to generate reference to class (a character vector of length one)
#' @rdname make_alg_to_gen_ref_to_cls
#' @export 

#' @keywords internal
make_alg_to_gen_ref_to_cls <- function (class_nm_1L_chr, pkg_nm_1L_chr = ".GlobalEnv") 
{
    alg_to_gen_ref_to_cls_1L_chr <- paste0("methods::className(\"", 
        class_nm_1L_chr, "\",\"", pkg_nm_1L_chr, "\")")
    return(alg_to_gen_ref_to_cls_1L_chr)
}
#' Make algorithm to get prototype value
#' @description make_alg_to_get_pt_val() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to get prototype value. The function returns Algorithm to get prototype value (a character vector of length one).
#' @param pt_ns_1L_chr Prototype namespace (a character vector of length one), Default: ''
#' @param fn_to_call_1L_chr Function to call (a character vector of length one), Default: ''
#' @param default_val_1L_chr Default value (a character vector of length one), Default: ''
#' @param attached_nss_chr Attached namespaces (a character vector), Default: c("base")
#' @return Algorithm to get prototype value (a character vector of length one)
#' @rdname make_alg_to_get_pt_val
#' @export 

#' @keywords internal
make_alg_to_get_pt_val <- function (pt_ns_1L_chr = "", fn_to_call_1L_chr = "", default_val_1L_chr = "", 
    attached_nss_chr = c("base")) 
{
    alg_to_get_pt_val_1L_chr <- paste0(ifelse(pt_ns_1L_chr %in% 
        attached_nss_chr, "", paste0(pt_ns_1L_chr, "::")), fn_to_call_1L_chr, 
        ifelse(fn_to_call_1L_chr == "", "", "("), default_val_1L_chr, 
        ifelse(fn_to_call_1L_chr == "", "", ")"))
    return(alg_to_get_pt_val_1L_chr)
}
#' Make algorithm to set generic
#' @description make_alg_to_set_gnrc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to set generic. The function returns Algorithm to set generic (a character vector of length one).
#' @param name_1L_chr Name (a character vector of length one)
#' @param args_chr Arguments (a character vector), Default: c("x")
#' @param signature_1L_chr Signature (a character vector of length one), Default: 'NA'
#' @param where_1L_chr Where (a character vector of length one), Default: 'NA'
#' @return Algorithm to set generic (a character vector of length one)
#' @rdname make_alg_to_set_gnrc
#' @export 

#' @keywords internal
make_alg_to_set_gnrc <- function (name_1L_chr, args_chr = c("x"), signature_1L_chr = NA_character_, 
    where_1L_chr = NA_character_) 
{
    alg_to_set_gnrc_1L_chr <- paste0("methods::setGeneric(\"", 
        name_1L_chr, "\"", ifelse(is.na(args_chr[1]), "", paste0(", ", 
            make_gnrc_fn(name_1L_chr, args_chr = args_chr))), 
        ifelse(is.na(where_1L_chr[1]), "", paste0(",\nwhere =  ", 
            where_1L_chr)), ifelse(is.na(signature_1L_chr[1]), 
            "", paste0(",\nsignature =  \"", signature_1L_chr, 
                "\"")), ")")
    return(alg_to_set_gnrc_1L_chr)
}
#' Make algorithm to set method
#' @description make_alg_to_set_mthd() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to set method. The function returns Algorithm to set method (a character vector of length one).
#' @param name_1L_chr Name (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param fn Function (a function), Default: NULL
#' @param fn_nm_1L_chr Function name (a character vector of length one), Default: 'NA'
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'NA'
#' @param where_1L_chr Where (a character vector of length one), Default: 'NA'
#' @return Algorithm to set method (a character vector of length one)
#' @rdname make_alg_to_set_mthd
#' @export 

#' @keywords internal
make_alg_to_set_mthd <- function (name_1L_chr, class_nm_1L_chr, fn = NULL, fn_nm_1L_chr = NA_character_, 
    pkg_nm_1L_chr = NA_character_, where_1L_chr = NA_character_) 
{
    alg_to_set_mthd_1L_chr <- paste0("methods::setMethod(\"", 
        name_1L_chr, "\"", ", ", ifelse(is.na(pkg_nm_1L_chr[1]), 
            paste0("\"", class_nm_1L_chr, "\""), paste0(make_alg_to_gen_ref_to_cls(class_nm_1L_chr, 
                pkg_nm_1L_chr = pkg_nm_1L_chr))), ", ", ifelse(!is.na(fn_nm_1L_chr), 
            fn_nm_1L_chr, transform_fn_into_chr(fn)), ifelse(is.na(where_1L_chr[1]), 
            "", paste0(",\nwhere =  ", where_1L_chr)), ")")
    return(alg_to_set_mthd_1L_chr)
}
#' Make algorithm to set old classes
#' @description make_alg_to_set_old_clss() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to set old classes. The function returns Algorithm to set old classes (a character vector of length one).
#' @param type_chr Type (a character vector)
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @return Algorithm to set old classes (a character vector of length one)
#' @rdname make_alg_to_set_old_clss
#' @export 
#' @importFrom purrr map_lgl map_chr
#' @importFrom ready4fun get_from_lup_obj
#' @importFrom stringr str_c
#' @keywords internal
make_alg_to_set_old_clss <- function (type_chr, prototype_lup = NULL) 
{
    if (is.null(prototype_lup)) {
        index_of_s3_lgl <- T
    }
    else {
        index_of_s3_lgl <- purrr::map_lgl(type_chr, ~ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
            match_var_nm_1L_chr = "type_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "old_class_lgl", evaluate_lgl = FALSE))
    }
    if (!identical(type_chr[index_of_s3_lgl], character(0))) {
        alg_to_set_old_clss_1L_chr <- purrr::map_chr(type_chr[index_of_s3_lgl], 
            ~paste0("setOldClass(c(\"", .x, "\",\"tbl_df\", \"tbl\", \"data.frame\")", 
                ifelse(!is.null(prototype_lup), ",where =  ", 
                  ""), ifelse(!is.null(prototype_lup), "globalenv()", 
                  ""), ")")) %>% stringr::str_c(sep = "", collapse = "\n")
    }
    else {
        alg_to_set_old_clss_1L_chr <- character(0)
    }
    return(alg_to_set_old_clss_1L_chr)
}
#' Make algorithm to set validity of ready4 S4 class
#' @description make_alg_to_set_validity_of_r4_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to set validity of ready4 s4 class. The function returns Algorithm to set validity of ready4 S4 class (a character vector of length one).
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param slots_of_dif_lnts_chr Slots of different lengths (a character vector), Default: NULL
#' @param allowed_vals_ls Allowed values (a list), Default: NULL
#' @param names_must_match_ls Names must match (a list), Default: NULL
#' @param print_validator_1L_lgl Print validator (a logical vector of length one), Default: FALSE
#' @param asserts_ls Asserts (a list), Default: NULL
#' @return Algorithm to set validity of ready4 S4 class (a character vector of length one)
#' @rdname make_alg_to_set_validity_of_r4_cls
#' @export 
#' @importFrom ready4fun get_r4_obj_slots
#' @importFrom purrr map_chr map2_chr
#' @importFrom stringr str_c str_replace_all str_replace
#' @importFrom stringi stri_replace_last
#' @keywords internal
make_alg_to_set_validity_of_r4_cls <- function (class_nm_1L_chr, parent_cls_nm_1L_chr, slots_of_dif_lnts_chr = NULL, 
    allowed_vals_ls = NULL, names_must_match_ls = NULL, print_validator_1L_lgl = FALSE, 
    asserts_ls = NULL) 
{
    same_lnt_cdn_1L_chr <- allowed_cdn_chr <- names_inc_chr <- NA_character_
    all_slots <- ready4fun::get_r4_obj_slots(class_nm_1L_chr) %>% 
        names()
    if (!is.null(parent_cls_nm_1L_chr)) {
        parental_slots <- ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr) %>% 
            names()
        all_slots <- all_slots[!all_slots %in% parental_slots]
    }
    if (!is.null(slots_of_dif_lnts_chr)) {
        same_length_slots <- all_slots[!all_slots %in% slots_of_dif_lnts_chr]
        if (!identical(same_length_slots, character(0))) {
            slot_ls <- purrr::map_chr(same_length_slots, ~paste0("object@", 
                .x)) %>% stringr::str_c(sep = "", collapse = ",") %>% 
                paste0("list(", ., ")")
            same_lnt_cdn_1L_chr <- paste0("if(length(unique(lengths(", 
                slot_ls, "))) > 1)\n", "msg <- c(msg, ", "\"", 
                same_length_slots %>% stringr::str_c(sep = "", 
                  collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                  replacement = " and"), " must all be of the same length.\")")
        }
    }
    if (!is.null(allowed_vals_ls)) {
        allowed_cdn_chr <- purrr::map2_chr(names(allowed_vals_ls), 
            allowed_vals_ls, ~paste0("if(!identical(", "object@", 
                .x, "[! object@", .x, " %in% ", ifelse(is.character(.y), 
                  "\"", ""), .y, ifelse(is.character(.y), "\"", 
                  ""), "],character(0))){\n", "msg <- c(msg, ", 
                "\"", .x, " slot can only include the following values: ", 
                .y, "\")\n}"))
    }
    if (!is.null(names_must_match_ls)) {
        names_include_conc <- purrr::map_chr(names_must_match_ls, 
            ~paste0("c(\"", .x %>% stringr::str_c(sep = "", collapse = "\",\""), 
                "\")"))
        names_inc_chr <- purrr::map2_chr(names(names_must_match_ls), 
            names_include_conc, ~paste0("if(!identical(", .y, 
                "[", .y, " %in% ", "names(object@", .x, ")],", 
                .y, ")){\n", "msg <- c(msg, ", "\"", .x, " slot object names can only include the following values: ", 
                .y %>% stringr::str_replace_all("\"", "") %>% 
                  stringr::str_replace("c\\(", "") %>% stringr::str_replace("\\)", 
                  ""), "\")\n}"))
    }
    valid_function <- paste0("function(object){\n", "msg <- NULL\n", 
        ifelse(is.na(same_lnt_cdn_1L_chr), "", paste0(same_lnt_cdn_1L_chr, 
            "\n")), ifelse(is.na(allowed_cdn_chr), "", allowed_cdn_chr %>% 
            paste0(collapse = "")), ifelse(is.na(names_inc_chr), 
            "", names_inc_chr %>% paste0(collapse = "")), ifelse(!is.null(asserts_ls), 
            purrr::map_chr(asserts_ls, ~{
                paste0("rlang::exec(", .x$assert_fn_1L_chr, ",object,", 
                  "!!!", deparse(.x$args_ls), ")\n")
            }) %>% paste0(collapse = ""), ""), "if (is.null(msg)) TRUE else msg", 
        "\n}")
    alg_to_set_validity_of_r4_cls_1L_chr <- paste0("methods::setValidity(", 
        make_alg_to_gen_ref_to_cls(class_nm_1L_chr), ",\n", valid_function, 
        ",\nwhere =  ", "globalenv()", ")")
    return(alg_to_set_validity_of_r4_cls_1L_chr)
}
#' Make algorithm to write getter setter methods
#' @description make_alg_to_write_gtr_str_mthds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make algorithm to write getter setter methods. The function is called for its side effects and does not return a value.
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param print_gtrs_strs_1L_lgl Print getters setters (a logical vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Require packages (a character vector)
#' @param parent_ns_ls Parent namespace (a list)
#' @return Algorithm to write getter setter (methods)
#' @rdname make_alg_to_write_gtr_str_mthds
#' @export 
#' @importFrom ready4fun get_r4_obj_slots
#' @importFrom stringr str_c
#' @keywords internal
make_alg_to_write_gtr_str_mthds <- function (class_nm_1L_chr, parent_cls_nm_1L_chr, print_gtrs_strs_1L_lgl, 
    output_dir_1L_chr, nss_to_ignore_chr, req_pkgs_chr, parent_ns_ls) 
{
    slot_names_chr <- ready4fun::get_r4_obj_slots(class_nm_1L_chr) %>% 
        names()
    if (is.null(parent_cls_nm_1L_chr)) {
        set_only_chr <- ""
    }
    else {
        set_only_chr <- ready4fun::get_r4_obj_slots(parent_cls_nm_1L_chr, 
            package_1L_chr = transform_parent_ns_ls(parent_ns_ls)) %>% 
            names()
    }
    alg_to_write_gtr_str_mthds <- paste0("write_gtr_str_mthds_for_slots(", 
        "slot_names_chr = c(\"", slot_names_chr %>% stringr::str_c(collapse = "\",\""), 
        "\")", ",", "set_only_chr = c(\"", set_only_chr %>% stringr::str_c(collapse = "\",\""), 
        "\")", ",parent_cls_nm_1L_chr = \"", parent_cls_nm_1L_chr, 
        "\",", "class_nm_1L_chr = \"", class_nm_1L_chr, "\", print_gtrs_strs_1L_lgl = ", 
        print_gtrs_strs_1L_lgl, ",output_dir_1L_chr = \"", output_dir_1L_chr, 
        "\"", ",nss_to_ignore_chr = c(\"", nss_to_ignore_chr %>% 
            stringr::str_c(collapse = "\",\""), "\")", ",req_pkgs_chr = \"", 
        req_pkgs_chr, "\"", ")")
    return(alg_to_write_gtr_str_mthds)
}
#' Make class prototype tibble for ready4 S3 and ready4 S4 classes
#' @description make_class_pt_tb_for_r3_and_r4_clss() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make class prototype tibble for ready4 s3 and ready4 s4 classes. The function returns Class prototype tibble for ready4 S3 and ready4 S4 classes (a tibble).
#' @param class_mk_ls Class make (a list)
#' @return Class prototype tibble for ready4 S3 and ready4 S4 classes (a tibble)
#' @rdname make_class_pt_tb_for_r3_and_r4_clss
#' @export 
#' @importFrom purrr map2_dfr
#' @importFrom rlang exec
#' @keywords internal
make_class_pt_tb_for_r3_and_r4_clss <- function (class_mk_ls) 
{
    class_pt_tb_for_r3_and_r4_clss_tb <- purrr::map2_dfr(class_mk_ls, 
        names(class_mk_ls), ~{
            if (.y == "s3_ls") {
                fn = make_pt_tb_for_new_r3_cls
            }
            else {
                fn = make_pt_tb_for_new_r4_cls
            }
            rlang::exec(fn, .x)
        })
    return(class_pt_tb_for_r3_and_r4_clss_tb)
}
#' Make class prototypes tibble
#' @description make_class_pts_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make class prototypes tibble. The function returns Class prototypes (a tibble).
#' @param class_mk_ls Class make (a list)
#' @return Class prototypes (a tibble)
#' @rdname make_class_pts_tb
#' @export 
#' @importFrom purrr map2_dfr
#' @keywords internal
make_class_pts_tb <- function (class_mk_ls) 
{
    class_pts_tb <- purrr::map2_dfr(class_mk_ls, names(class_mk_ls), 
        ~make_one_row_class_pt_tb(.x, make_s3_lgl = ifelse(.y == 
            "s3_ls", T, F)))
    return(class_pts_tb)
}
#' Make documentation include tag
#' @description make_dmt_inc_tag() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make documentation include tag. The function returns Documentation include tag (a character vector of length one).
#' @param class_names_chr Class names (a character vector)
#' @param s3_1L_lgl S3 (a logical vector of length one), Default: T
#' @return Documentation include tag (a character vector of length one)
#' @rdname make_dmt_inc_tag
#' @export 
#' @importFrom stringr str_c
#' @keywords internal
make_dmt_inc_tag <- function (class_names_chr, s3_1L_lgl = T) 
{
    dmt_inc_tag_1L_chr <- ifelse(!is.null(class_names_chr), paste0("#' @include ", 
        get_class_fl_nms(class_names_chr = class_names_chr, s3_1L_lgl = s3_1L_lgl) %>% 
            stringr::str_c(collapse = " "), "\n"), "")
    return(dmt_inc_tag_1L_chr)
}
#' Make function prototype to check ready4 S3 class inheritance
#' @description make_fn_pt_to_check_r3_cls_inhtc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function prototype to check ready4 s3 class inheritance. The function is called for its side effects and does not return a value.
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param s3_validator_ls S3 validator (a list)
#' @return Function prototype to check ready4 S3 class (an inheritance)
#' @rdname make_fn_pt_to_check_r3_cls_inhtc
#' @export 

#' @keywords internal
make_fn_pt_to_check_r3_cls_inhtc <- function (class_nm_1L_chr, s3_validator_ls) 
{
    name_of_fn_to_check_if_is_valid_instance <- paste0("is_", 
        class_nm_1L_chr)
    fn_to_check_if_is_valid_instance <- paste0(name_of_fn_to_check_if_is_valid_instance, 
        " <- function(x) inherits(", s3_validator_ls$fn_name_1L_chr, 
        "(x), \"", class_nm_1L_chr, "\")")
    fn_pt_to_check_r3_cls_inhtc <- list(fn_name_1L_chr = name_of_fn_to_check_if_is_valid_instance, 
        fn_body_1L_chr = fn_to_check_if_is_valid_instance)
    return(fn_pt_to_check_r3_cls_inhtc)
}
#' Make function prototype to make ready4 S3 class prototype
#' @description make_fn_pt_to_make_r3_cls_pt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function prototype to make ready4 s3 class prototype. The function is called for its side effects and does not return a value.
#' @param type_1L_chr Type (a character vector of length one)
#' @param pt_ns_1L_chr Prototype namespace (a character vector of length one)
#' @param vals_ls Values (a list)
#' @param ordered_1L_lgl Ordered (a logical vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param prototype_lup Prototype (a lookup table)
#' @return Function prototype to make ready4 S3 class (a prototype)
#' @rdname make_fn_pt_to_make_r3_cls_pt
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom purrr map map_chr map2_chr
#' @importFrom stringr str_c
#' @keywords internal
make_fn_pt_to_make_r3_cls_pt <- function (type_1L_chr, pt_ns_1L_chr, vals_ls, ordered_1L_lgl, 
    class_nm_1L_chr, parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    prototype_lup) 
{
    if (type_1L_chr %in% c("tibble", "list")) {
        if (!is.null(parent_cls_nm_1L_chr)) {
            parent_proto_fn_chr <- get_parent_cls_pt_fn(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
                dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, prototype_lup = prototype_lup)
            vals_ls <- append(parse(text = parent_proto_fn_chr) %>% 
                eval() %>% as.list %>% purrr::map(~deparse(.x)), 
                vals_ls)
        }
        args_1L_chr <- paste0("args_ls <- list(", purrr::map_chr(names(vals_ls), 
            ~paste0(.x, " = ", .x)) %>% stringr::str_c(sep = "", 
            collapse = ",\n"), ") %>% ", ifelse(dev_pkg_ns_1L_chr == 
            "ready4fun", "", "ready4fun::"), "update_pt_fn_args_ls()\n")
        main_body_1L_chr <- paste0("rlang::exec(", ifelse(type_1L_chr == 
            "tibble", "tibble::tibble", "list"), ",!!!args_ls", 
            ")")
        fn_call_to_create_prototype <- paste0(args_1L_chr, main_body_1L_chr)
    }
    else {
        if (type_1L_chr == "factor") {
            fn_call_to_create_prototype <- paste0("factor(x = character(),\nlevels=c(\"", 
                vals_ls %>% stringr::str_c(sep = "", collapse = "\",\"\n"), 
                "\"),\nordered_1L_lgl=", ordered_1L_lgl, ")")
        }
        else {
            fn_call_to_create_prototype <- paste0(pt_ns_1L_chr, 
                ifelse(pt_ns_1L_chr == "", "", "::"), type_1L_chr, 
                "(0)")
        }
    }
    name_of_fn_to_make_pt <- paste0("make_pt_", class_nm_1L_chr)
    fn_to_make_pt <- paste0(name_of_fn_to_make_pt, " <- function(", 
        ifelse(type_1L_chr %in% c("tibble", "list"), purrr::map2_chr(names(vals_ls), 
            vals_ls, ~paste0(.x, " = ", .y)) %>% stringr::str_c(sep = "", 
            collapse = ",\n"), ""), "){ \n", fn_call_to_create_prototype, 
        "\n}")
    fn_pt_to_make_r3_cls_pt <- list(fn_name_1L_chr = name_of_fn_to_make_pt, 
        fn_body_1L_chr = fn_to_make_pt)
    return(fn_pt_to_make_r3_cls_pt)
}
#' Make function prototype to make unvalidated ready4 S3 class instance
#' @description make_fn_pt_to_make_unvld_r3_cls_inst() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function prototype to make unvalidated ready4 s3 class instance. The function is called for its side effects and does not return a value.
#' @param type_1L_chr Type (a character vector of length one)
#' @param pt_chkr_pfx_1L_chr Prototype checker prefix (a character vector of length one)
#' @param pt_ns_1L_chr Prototype namespace (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param s3_prototype_ls S3 prototype (a list)
#' @return Function prototype to make unvalidated ready4 S3 class (an instance)
#' @rdname make_fn_pt_to_make_unvld_r3_cls_inst
#' @export 

#' @keywords internal
make_fn_pt_to_make_unvld_r3_cls_inst <- function (type_1L_chr, pt_chkr_pfx_1L_chr, pt_ns_1L_chr, class_nm_1L_chr, 
    s3_prototype_ls) 
{
    name_of_fn_to_construct_instance <- paste0("make_new_", class_nm_1L_chr)
    stop_cndn_in_constructor <- ifelse(type_1L_chr == "factor", 
        "TRUE", paste0(pt_ns_1L_chr, ifelse(pt_ns_1L_chr == "", 
            "", "::"), pt_chkr_pfx_1L_chr, type_1L_chr, "(x)"))
    fn_to_construct_instance <- paste0(name_of_fn_to_construct_instance, 
        " <- function(x){ \n", "stopifnot(", stop_cndn_in_constructor, 
        ")\n", "class(x) <- append(", "c(\"", class_nm_1L_chr, 
        "\",setdiff(", paste0(s3_prototype_ls$fn_name_1L_chr, 
            "()"), " %>% class(),class(x)))", ",\nclass(x))\nx\n}")
    fn_pt_to_make_unvld_r3_cls_inst <- list(fn_name_1L_chr = name_of_fn_to_construct_instance, 
        fn_body_1L_chr = fn_to_construct_instance)
    return(fn_pt_to_make_unvld_r3_cls_inst)
}
#' Make function prototype to make valid ready4 S3 class instance
#' @description make_fn_pt_to_make_vld_r3_cls_inst() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function prototype to make valid ready4 s3 class instance. The function is called for its side effects and does not return a value.
#' @param type_1L_chr Type (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param s3_prototype_ls S3 prototype (a list)
#' @param min_max_vals_dbl Minimum maximum values (a double vector)
#' @param start_end_vals_dbl Start end values (a double vector)
#' @param vals_ls Values (a list)
#' @param asserts_ls Asserts (a list), Default: NULL
#' @return Function prototype to make valid ready4 S3 class (an instance)
#' @rdname make_fn_pt_to_make_vld_r3_cls_inst
#' @export 
#' @importFrom purrr compact map2_chr map_chr
#' @importFrom stringr str_c
#' @keywords internal
make_fn_pt_to_make_vld_r3_cls_inst <- function (type_1L_chr, class_nm_1L_chr, s3_prototype_ls, min_max_vals_dbl, 
    start_end_vals_dbl, vals_ls, asserts_ls = NULL) 
{
    name_of_fn_to_validate_instance <- paste0("validate_", class_nm_1L_chr)
    validator_stop_cond_ls <- validator_stop_msg_call_ls <- NULL
    if (type_1L_chr %in% c("tibble", "list")) {
        stop_cndn_in_validator_1 <- paste0("sum(stringr::str_detect(names(x)[names(x) %in% names(", 
            s3_prototype_ls$fn_name_1L_chr, "())],\n", "names(", 
            s3_prototype_ls$fn_name_1L_chr, "())))!=length(names(", 
            s3_prototype_ls$fn_name_1L_chr, "()))")
        tb_or_ls_class_summary <- ifelse(type_1L_chr == "list", 
            "lapply(class) %>% tibble::as_tibble() ", "dplyr::summarise_all(class) ")
        var_class_lup <- paste0(s3_prototype_ls$fn_name_1L_chr, 
            "() %>% \n", tb_or_ls_class_summary, "%>% \n tidyr::gather(variable,class)")
        stop_cndn_in_validator_2 <- paste0("!identical(", var_class_lup, 
            " %>% \n", "dplyr::arrange(variable),\n", "x %>% \n", 
            tb_or_ls_class_summary, "%>% \n tidyr::gather(variable,class) %>% \n", 
            "dplyr::filter(variable %in% names(", s3_prototype_ls$fn_name_1L_chr, 
            "())) %>% ", "dplyr::arrange(variable)", ")")
        obj_components_chr <- c(toupper(type_1L_chr), ifelse(type_1L_chr == 
            "list", "elements", "columns"))
        stop_msg_call_in_validator_1 <- paste0("paste0(\"", obj_components_chr[1], 
            " must include ", obj_components_chr[2], " named: \",\n", 
            "names(", s3_prototype_ls$fn_name_1L_chr, "()) %>% stringr::str_c(sep=\"\", collapse = \", \"))")
        stop_msg_call_in_validator_2 <- paste0("paste0(\"", obj_components_chr[1], 
            " ", obj_components_chr[2], " should be of the following classes: \",\n", 
            "purrr::map2_chr(", var_class_lup, " %>% \ndplyr::pull(1),\n ", 
            var_class_lup, " %>% \ndplyr::pull(2),\n ", "~ paste0(.x,\": \",.y)) %>% \n", 
            "stringr::str_c(sep=\"\", collapse = \", \"))")
        validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1, 
            b = stop_cndn_in_validator_2)
        validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1, 
            b = stop_msg_call_in_validator_2)
    }
    else {
        if (!is.null(min_max_vals_dbl)) {
            stop_cndn_in_validator_1 <- stop_msg_call_in_validator_1 <- stop_cndn_in_validator_2 <- stop_msg_call_in_validator_2 <- NULL
            if (!is.na(min_max_vals_dbl[1])) {
                stop_cndn_in_validator_1 <- paste0("any(", ifelse(type_1L_chr == 
                  "character", "stringr::str_length(x)", "x[!is.na(x)]"), 
                  " < ", min_max_vals_dbl[1], ")")
                stop_msg_call_in_validator_1 <- paste0("\"All non-missing values in valid ", 
                  class_nm_1L_chr, " object must be ", ifelse(type_1L_chr == 
                    "character", "of length ", ""), "greater than or equal to ", 
                  min_max_vals_dbl[1], ".\"")
            }
            if (!is.na(min_max_vals_dbl[2])) {
                stop_cndn_in_validator_2 <- paste0("any(", ifelse(type_1L_chr == 
                  "character", "stringr::str_length(x)", "x[!is.na(x)]"), 
                  " > ", min_max_vals_dbl[2], ")")
                stop_msg_call_in_validator_2 <- paste0("\"All non-missing values in valid ", 
                  class_nm_1L_chr, " object must be ", ifelse(type_1L_chr == 
                    "character", "of length ", ""), "less than or equal to ", 
                  min_max_vals_dbl[2], ".\"")
            }
            validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1, 
                b = stop_cndn_in_validator_2) %>% purrr::compact()
            validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1, 
                b = stop_msg_call_in_validator_2) %>% purrr::compact()
        }
        if (!is.null(start_end_vals_dbl)) {
            stop_cndn_in_validator_1 <- stop_msg_call_in_validator_1 <- stop_cndn_in_validator_2 <- stop_msg_call_in_validator_2 <- NULL
            if (!is.na(start_end_vals_dbl[1])) {
                stop_cndn_in_validator_1 <- paste0("any(purrr::map_lgl(x, ~ !startsWith(.x,\"", 
                  start_end_vals_dbl[1], "\")))")
                stop_msg_call_in_validator_1 <- paste0("\"All values in valid ", 
                  class_nm_1L_chr, " object must start with '", 
                  start_end_vals_dbl[1], "'.\"")
            }
            if (!is.na(start_end_vals_dbl[2])) {
                stop_cndn_in_validator_2 <- paste0("any(purrr::map_lgl(x, ~ !endsWith(.x,\"", 
                  start_end_vals_dbl[2], "\")))")
                stop_msg_call_in_validator_2 <- paste0("\"All values in valid ", 
                  class_nm_1L_chr, " object must end with '", 
                  start_end_vals_dbl[2], "'.\"")
            }
            validator_stop_cond_ls <- append(validator_stop_cond_ls, 
                list(a = stop_cndn_in_validator_1, b = stop_cndn_in_validator_2) %>% 
                  purrr::compact())
            validator_stop_msg_call_ls <- append(validator_stop_msg_call_ls, 
                list(a = stop_msg_call_in_validator_1, b = stop_msg_call_in_validator_2) %>% 
                  purrr::compact())
        }
        if (type_1L_chr == "factor") {
            stop_cndn_in_validator_1 <- paste0("!identical(setdiff(x,c(\"", 
                vals_ls %>% stringr::str_c(collapse = "\",\""), 
                "\")),character(0))")
            stop_msg_call_in_validator_1 <- paste0("\"Levels in valid ", 
                class_nm_1L_chr, " object are: ", vals_ls %>% 
                  stringr::str_c(collapse = ","), ".\"")
            validator_stop_cond_ls <- list(a = stop_cndn_in_validator_1)
            validator_stop_msg_call_ls <- list(a = stop_msg_call_in_validator_1)
        }
    }
    fn_to_validate_instance <- paste0(name_of_fn_to_validate_instance, 
        " <- function(x){\n", purrr::map2_chr(validator_stop_cond_ls, 
            validator_stop_msg_call_ls, ~paste0("if(", .x, "){\n", 
                "stop(", .y, ",\ncall. = FALSE)\n}\n")) %>% stringr::str_c(sep = "", 
            collapse = "\n "), ifelse(!is.null(asserts_ls), purrr::map_chr(asserts_ls, 
            ~{
                paste0("rlang::exec(", .x$assert_fn_1L_chr, ",x,", 
                  "!!!", deparse(.x$args_ls), ")\n")
            }) %>% paste0(collapse = ""), ""), "\nx}")
    fn_pt_to_make_vld_r3_cls_inst <- list(fn_name_1L_chr = name_of_fn_to_validate_instance, 
        fn_body_1L_chr = fn_to_validate_instance)
    return(fn_pt_to_make_vld_r3_cls_inst)
}
#' Make function prototype to make validated ready4 S3 class instance
#' @description make_fn_pt_to_make_vldd_r3_cls_inst() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function prototype to make validated ready4 s3 class instance. The function is called for its side effects and does not return a value.
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param s3_prototype_ls S3 prototype (a list)
#' @param s3_constructor_ls S3 constructor (a list)
#' @param s3_validator_ls S3 validator (a list)
#' @return Function prototype to make validated ready4 S3 class (an instance)
#' @rdname make_fn_pt_to_make_vldd_r3_cls_inst
#' @export 

#' @keywords internal
make_fn_pt_to_make_vldd_r3_cls_inst <- function (class_nm_1L_chr, s3_prototype_ls, s3_constructor_ls, 
    s3_validator_ls) 
{
    fn_call_to_make_valid_instance <- paste0(s3_validator_ls$fn_name_1L_chr, 
        "(", s3_constructor_ls$fn_name_1L_chr, "(x))")
    name_of_fn_to_make_valid_instance <- class_nm_1L_chr
    fn_to_make_valid_instance <- paste0(name_of_fn_to_make_valid_instance, 
        " <- function(x = ", s3_prototype_ls$fn_name_1L_chr, 
        "()){ \n", fn_call_to_make_valid_instance, "\n}")
    fn_pt_to_make_vldd_r3_cls_inst <- list(fn_name_1L_chr = name_of_fn_to_make_valid_instance, 
        fn_body_1L_chr = fn_to_make_valid_instance)
    return(fn_pt_to_make_vldd_r3_cls_inst)
}
#' Make generic function
#' @description make_gnrc_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make generic function. The function returns Generic function (a character vector of length one).
#' @param name_1L_chr Name (a character vector of length one)
#' @param args_chr Arguments (a character vector)
#' @return Generic function (a character vector of length one)
#' @rdname make_gnrc_fn
#' @export 

#' @keywords internal
make_gnrc_fn <- function (name_1L_chr, args_chr) 
{
    if (all(!is.na(args_chr))) {
        gnrc_fn_1L_chr <- paste0("function(", paste0(args_chr, 
            collapse = ", "), ") standardGeneric(\"", name_1L_chr, 
            "\")")
    }
    else {
        gnrc_fn_1L_chr <- ""
    }
    return(gnrc_fn_1L_chr)
}
#' Make generic method pair list
#' @description make_gnrc_mthd_pair_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make generic method pair list. The function returns Generic method pair (a list).
#' @param name_1L_chr Name (a character vector of length one)
#' @param args_chr Arguments (a character vector), Default: c("x")
#' @param signature_1L_chr Signature (a character vector of length one), Default: 'NA'
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'NA'
#' @param where_1L_chr Where (a character vector of length one), Default: 'NA'
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param fn Function (a function)
#' @return Generic method pair (a list)
#' @rdname make_gnrc_mthd_pair_ls
#' @export 

#' @keywords internal
make_gnrc_mthd_pair_ls <- function (name_1L_chr, args_chr = c("x"), signature_1L_chr = NA_character_, 
    pkg_nm_1L_chr = NA_character_, where_1L_chr = NA_character_, 
    class_nm_1L_chr, fn) 
{
    gnrc_mthd_pair_ls <- list(generic_1L_chr = make_alg_to_set_gnrc(name_1L_chr, 
        args_chr = args_chr, signature_1L_chr = signature_1L_chr, 
        where_1L_chr = where_1L_chr), method_chr = make_alg_to_set_mthd(name_1L_chr, 
        class_nm_1L_chr = class_nm_1L_chr, fn = fn, pkg_nm_1L_chr = pkg_nm_1L_chr, 
        where_1L_chr = where_1L_chr), gen_fn_chr = make_gnrc_fn(name_1L_chr, 
        args_chr = args_chr), meth_fn_chr = transform_fn_into_chr(fn))
    return(gnrc_mthd_pair_ls)
}
#' Make helper function
#' @description make_helper_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make helper function. The function returns Helper function (a character vector of length one).
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param slots_chr Slots (a character vector)
#' @param pt_ls Prototype (a list)
#' @param prototype_lup Prototype (a lookup table)
#' @param parent_ns_ls Parent namespace (a list)
#' @return Helper function (a character vector of length one)
#' @rdname make_helper_fn
#' @export 
#' @importFrom stringr str_sub str_replace str_replace_all str_c
#' @keywords internal
make_helper_fn <- function (class_nm_1L_chr, parent_cls_nm_1L_chr, slots_chr, pt_ls, 
    prototype_lup, parent_ns_ls) 
{
    if (!is.null(parent_cls_nm_1L_chr)) {
        child_slots_chr <- slots_chr
        slots_chr <- get_parent_cls_slot_nms(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            parent_ns_ls = parent_ns_ls)
        parent_proto <- get_parent_cls_pts(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            parent_ns_ls = parent_ns_ls, slot_names_chr = slots_chr)
        child_ls_chr <- pt_ls %>% stringr::str_sub(start = 6, 
            end = -2)
        pt_ls <- make_pt_ls(slots_chr = slots_chr, type_chr = parent_proto, 
            prototype_lup = prototype_lup)
        pt_ls <- paste0(pt_ls %>% stringr::str_sub(end = -2), 
            ",", child_ls_chr, ")")
        slots_chr <- c(slots_chr, child_slots_chr)
    }
    func_args <- pt_ls %>% stringr::str_replace("list", "function") %>% 
        stringr::str_replace_all(",", ",\n")
    helper_fn_1L_chr <- paste0(class_nm_1L_chr, " <- ", func_args, 
        "{ \n", "methods::new(\"", class_nm_1L_chr, "\",\n", 
        paste0(slots_chr, " = ", slots_chr) %>% stringr::str_c(sep = "", 
            collapse = ",\n"), ")\n}")
    return(helper_fn_1L_chr)
}
#' Make lines for writing documented function
#' @description make_lines_for_writing_dmtd_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make lines for writing documented function. The function is called for its side effects and does not return a value.
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_body_1L_chr Function body (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param class_desc_1L_chr Class description (a character vector of length one)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @return NULL
#' @rdname make_lines_for_writing_dmtd_fn
#' @export 
#' @importFrom ready4fun make_lines_for_fn_dmt
#' @keywords internal
make_lines_for_writing_dmtd_fn <- function (fn_name_1L_chr, fn_body_1L_chr, fn_type_1L_chr, class_nm_1L_chr, 
    class_desc_1L_chr, abbreviations_lup, object_type_lup) 
{
    ready4fun::make_lines_for_fn_dmt(fn_name_1L_chr = fn_name_1L_chr, 
        fn_type_1L_chr = fn_type_1L_chr, fn_title_1L_chr = fn_name_1L_chr, 
        fn = eval(parse(text = fn_body_1L_chr)), class_name_1L_chr = class_nm_1L_chr, 
        details_1L_chr = class_desc_1L_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup)
    writeLines(fn_body_1L_chr)
}
#' Make list of packages to import
#' @description make_ls_of_pkgs_to_imp() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list of packages to import. The function returns Packages to import (a list).
#' @param curr_gnrcs_ls Current generics (a list)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @return Packages to import (a list)
#' @rdname make_ls_of_pkgs_to_imp
#' @export 
#' @importFrom purrr map2_lgl
#' @keywords internal
make_ls_of_pkgs_to_imp <- function (curr_gnrcs_ls, fn_name_1L_chr, nss_to_ignore_chr) 
{
    packages_chr <- curr_gnrcs_ls$packages_chr[!curr_gnrcs_ls$packages_chr %in% 
        c(".GlobalEnv")]
    gnrc_gtr_exists_lgl <- purrr::map2_lgl(packages_chr, names(packages_chr), 
        ~((.x == fn_name_1L_chr | .y == paste0(fn_name_1L_chr, 
            ".", .x)) & !.x %in% nss_to_ignore_chr))
    gnrc_gtr_exists_1L_lgl <- any(gnrc_gtr_exists_lgl)
    gtr_imps_chr <- ifelse(gnrc_gtr_exists_1L_lgl, packages_chr[gnrc_gtr_exists_lgl], 
        NA_character_)
    gnrc_str_exists_lgl <- purrr::map2_lgl(packages_chr, names(packages_chr), 
        ~((.x == paste0(fn_name_1L_chr, "<-") | .y == paste0(fn_name_1L_chr, 
            "<-.", .x)) & !.x %in% nss_to_ignore_chr))
    gnrc_str_exists_1L_lgl <- any(gnrc_str_exists_lgl)
    str_imps_chr <- ifelse(gnrc_str_exists_1L_lgl, packages_chr[gnrc_str_exists_lgl], 
        NA_character_)
    pkgs_to_imp_ls <- list(gtr_imps_chr = gtr_imps_chr[gtr_imps_chr != 
        nss_to_ignore_chr[1]], str_imps_chr = str_imps_chr[str_imps_chr != 
        nss_to_ignore_chr[1]], gnrc_gtr_exists_1L_lgl = gnrc_gtr_exists_1L_lgl, 
        gnrc_str_exists_1L_lgl = gnrc_str_exists_1L_lgl)
    return(pkgs_to_imp_ls)
}
#' Make list of transformed names of current generics
#' @description make_ls_of_tfd_nms_of_curr_gnrcs() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list of transformed names of current generics. The function returns Current generics (a list).
#' @param req_pkgs_chr Require packages (a character vector)
#' @param generic_1L_chr Generic (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @return Current generics (a list)
#' @rdname make_ls_of_tfd_nms_of_curr_gnrcs
#' @export 
#' @importFrom gtools getDependencies
#' @importFrom purrr discard
#' @importFrom ready4fun unload_packages
#' @keywords internal
make_ls_of_tfd_nms_of_curr_gnrcs <- function (req_pkgs_chr, generic_1L_chr, nss_to_ignore_chr) 
{
    curr_gnrcs_ls <- get_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr, 
        generic_1L_chr = generic_1L_chr)
    if (is.na(nss_to_ignore_chr[1])) {
        dependencies_chr <- character(0)
    }
    else {
        dependencies_chr <- gtools::getDependencies(nss_to_ignore_chr[1])
    }
    if (!req_pkgs_chr %>% purrr::discard(is.na) %>% identical(character(0))) 
        req_pkgs_chr <- req_pkgs_chr[!req_pkgs_chr %in% dependencies_chr]
    if (curr_gnrcs_ls$in_global_1L_lgl) {
        ready4fun::unload_packages(package_chr = req_pkgs_chr[req_pkgs_chr != 
            nss_to_ignore_chr[1]])
        curr_gnrcs_ls <- get_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr, 
            generic_1L_chr = generic_1L_chr)
    }
    return(curr_gnrcs_ls)
}
#' Make one row class prototype tibble
#' @description make_one_row_class_pt_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make one row class prototype tibble. The function returns One row class prototype (a tibble).
#' @param class_type_mk_ls Class type make (a list)
#' @param make_s3_1L_lgl Make S3 (a logical vector of length one), Default: T
#' @return One row class prototype (a tibble)
#' @rdname make_one_row_class_pt_tb
#' @export 
#' @importFrom purrr reduce flatten
#' @importFrom testit assert
#' @importFrom rlang exec
#' @importFrom tibble add_case
#' @importFrom dplyr mutate mutate_at
#' @keywords internal
make_one_row_class_pt_tb <- function (class_type_mk_ls, make_s3_1L_lgl = T) 
{
    one_row_class_pt_tb <- class_type_mk_ls %>% purrr::reduce(.init = ready4class_constructor_tbl(), 
        ~{
            testit::assert(paste0("Allowable list element names are: ", 
                names(.x) %>% paste0(collapse = ",")), names(.y) %in% 
                names(.x))
            rlang::exec(tibble::add_case, .x, !!!.y)
        }) %>% dplyr::mutate(make_s3_lgl = make_s3_1L_lgl) %>% 
        remake_ls_cols()
    if (make_s3_1L_lgl) {
        one_row_class_pt_tb <- one_row_class_pt_tb %>% dplyr::mutate_at(c("slots_ls", 
            "inc_clss_ls"), ~purrr::flatten(.x))
    }
    return(one_row_class_pt_tb)
}
#' Make one row prototype tibble for new ready4 S3 class
#' @description make_one_row_pt_tb_for_new_r3_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make one row prototype tibble for new ready4 s3 class. The function returns One row class prototype (a tibble).
#' @param x An object
#' @return One row class prototype (a tibble)
#' @rdname make_one_row_pt_tb_for_new_r3_cls
#' @export 

#' @keywords internal
make_one_row_pt_tb_for_new_r3_cls <- function (x) 
{
    one_row_class_pt_tb <- make_one_row_class_pt_tb(list(name_stub_chr = x@name_stub_chr, 
        pt_ls = x@pt_ls, pt_chkr_pfx_ls = x@pt_chkr_pfx_ls, pt_ns_ls = x@pt_ns_ls, 
        vals_ls = x@vals_ls, allowed_vals_ls = x@allowed_vals_ls, 
        min_max_vals_ls = x@min_max_vals_ls, start_end_vals_ls = x@start_end_vals_ls, 
        class_desc_chr = x@class_desc_chr, parent_class_chr = x@parent_class_chr, 
        inc_clss_ls = x@inc_clss_ls) %>% list(), make_s3_1L_lgl = T)
    return(one_row_class_pt_tb)
}
#' Make one row prototype tibble for new ready4 S4 class
#' @description make_one_row_pt_tb_for_new_r4_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make one row prototype tibble for new ready4 s4 class. The function returns One row class prototype (a tibble).
#' @param x An object
#' @return One row class prototype (a tibble)
#' @rdname make_one_row_pt_tb_for_new_r4_cls
#' @export 

#' @keywords internal
make_one_row_pt_tb_for_new_r4_cls <- function (x) 
{
    one_row_class_pt_tb <- make_one_row_class_pt_tb(list(name_stub_chr = x@name_stub_chr, 
        pt_ls = x@pt_ls, vals_ls = x@vals_ls, allowed_vals_ls = x@allowed_vals_ls, 
        class_desc_chr = x@class_desc_chr, parent_class_chr = x@parent_class_chr, 
        slots_ls = x@slots_ls, meaningful_nms_ls = x@meaningful_nms_ls, 
        inc_clss_ls = x@inc_clss_ls) %>% list(), make_s3_1L_lgl = F)
    return(one_row_class_pt_tb)
}
#' Make prototype list
#' @description make_pt_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prototype list. The function returns Prototype (a list).
#' @param slots_chr Slots (a character vector)
#' @param type_chr Type (a character vector), Default: NULL
#' @param vals_ls Values (a list), Default: NULL
#' @param make_val_1L_lgl Make value (a logical vector of length one), Default: TRUE
#' @param prototype_lup Prototype (a lookup table)
#' @return Prototype (a list)
#' @rdname make_pt_ls
#' @export 
#' @importFrom purrr map2_chr pmap_chr
#' @importFrom ready4fun get_from_lup_obj
#' @importFrom stringr str_c
#' @keywords internal
make_pt_ls <- function (slots_chr, type_chr = NULL, vals_ls = NULL, make_val_1L_lgl = TRUE, 
    prototype_lup) 
{
    pt_ls <- purrr::map2_chr(slots_chr, type_chr, ~paste0(.x, 
        " = ", ready4fun::get_from_lup_obj(data_lookup_tb = prototype_lup, 
            match_var_nm_1L_chr = "type_chr", match_value_xx = .y, 
            target_var_nm_1L_chr = "val_chr", evaluate_lgl = FALSE)))
    if (!is.null(vals_ls)) {
        pt_ls <- purrr::pmap_chr(list(slots_chr, pt_ls, 1:length(pt_ls)), 
            ~{
                if (..3 %in% 1:length(vals_ls)) {
                  paste0(..1, " = ", ifelse(make_val_1L_lgl, 
                    "\"", ""), vals_ls[[..3]], ifelse(make_val_1L_lgl, 
                    "\"", ""))
                }
                else {
                  ..2
                }
            })
    }
    pt_ls <- pt_ls %>% stringr::str_c(sep = "", collapse = ",") %>% 
        paste0("list(", ., ")")
    return(pt_ls)
}
#' Make prototype list for new ready4 S3 class
#' @description make_pt_ls_for_new_r3_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prototype list for new ready4 s3 class. The function returns Prototype list for new ready4 S3 class (a list).
#' @param class_name_1L_chr Class name (a character vector of length one)
#' @param type_1L_chr Type (a character vector of length one)
#' @param pt_ns_1L_chr Prototype namespace (a character vector of length one)
#' @param pt_chkr_pfx_1L_chr Prototype checker prefix (a character vector of length one)
#' @param vals_ls Values (a list)
#' @param ordered_1L_lgl Ordered (a logical vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param prototype_lup Prototype (a lookup table)
#' @param min_max_vals_dbl Minimum maximum values (a double vector)
#' @param start_end_vals_dbl Start end values (a double vector)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param asserts_ls Asserts (a list), Default: NULL
#' @return Prototype list for new ready4 S3 class (a list)
#' @rdname make_pt_ls_for_new_r3_cls
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @keywords internal
make_pt_ls_for_new_r3_cls <- function (class_name_1L_chr, type_1L_chr, pt_ns_1L_chr, pt_chkr_pfx_1L_chr, 
    vals_ls, ordered_1L_lgl, parent_cls_nm_1L_chr, prototype_lup, 
    min_max_vals_dbl, start_end_vals_dbl, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    nss_to_ignore_chr, asserts_ls = NULL) 
{
    s3_prototype_ls <- make_fn_pt_to_make_r3_cls_pt(type_1L_chr = type_1L_chr, 
        pt_ns_1L_chr = pt_ns_1L_chr, vals_ls = vals_ls, ordered_1L_lgl = ordered_1L_lgl, 
        class_nm_1L_chr = class_name_1L_chr, parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
        dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, prototype_lup = prototype_lup)
    s3_constructor_ls <- make_fn_pt_to_make_unvld_r3_cls_inst(type_1L_chr = type_1L_chr, 
        pt_chkr_pfx_1L_chr = pt_chkr_pfx_1L_chr, pt_ns_1L_chr = pt_ns_1L_chr, 
        class_nm_1L_chr = class_name_1L_chr, s3_prototype_ls = s3_prototype_ls)
    s3_validator_ls <- make_fn_pt_to_make_vld_r3_cls_inst(type_1L_chr = type_1L_chr, 
        class_nm_1L_chr = class_name_1L_chr, s3_prototype_ls = s3_prototype_ls, 
        min_max_vals_dbl = min_max_vals_dbl, start_end_vals_dbl = start_end_vals_dbl, 
        vals_ls = vals_ls, asserts_ls = asserts_ls)
    s3_valid_instance <- make_fn_pt_to_make_vldd_r3_cls_inst(class_nm_1L_chr = class_name_1L_chr, 
        s3_prototype_ls = s3_prototype_ls, s3_constructor_ls = s3_constructor_ls, 
        s3_validator_ls = s3_validator_ls)
    s3_checker <- make_fn_pt_to_check_r3_cls_inhtc(class_nm_1L_chr = class_name_1L_chr, 
        s3_validator_ls = s3_validator_ls)
    fn_name_ls <- list(valid_instance = s3_valid_instance$fn_name_1L_chr, 
        unvalidated_instance = s3_constructor_ls$fn_name_1L_chr, 
        prototype = s3_prototype_ls$fn_name_1L_chr, validator = s3_validator_ls$fn_name_1L_chr, 
        checker = s3_checker$fn_name_1L_chr)
    fn_body_1L_chr_ls <- list(valid_instance = s3_valid_instance$fn_body_1L_chr, 
        unvalidated_instance = s3_constructor_ls$fn_body_1L_chr, 
        prototype = s3_prototype_ls$fn_body_1L_chr, validator = s3_validator_ls$fn_body_1L_chr, 
        checker = s3_checker$fn_body_1L_chr)
    include_tags_chr <- get_parent_cls_ns(prototype_lup = prototype_lup, 
        parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr = nss_to_ignore_chr[1]) %>% 
        get_nms_of_clss_to_inc(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            base_set_of_clss_to_inc_chr = NULL) %>% make_dmt_inc_tag(s3_1L_lgl = T)
    pt_ls_for_new_r3_cls_ls <- list(fn_name_ls = fn_name_ls, 
        fn_body_1L_chr_ls = fn_body_1L_chr_ls, include_tags_chr = include_tags_chr)
    return(pt_ls_for_new_r3_cls_ls)
}
#' Make prototype tibble for new ready4 S3 class
#' @description make_pt_tb_for_new_r3_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prototype tibble for new ready4 s3 class. The function returns Prototype tibble for new ready4 S3 class (a tibble).
#' @param x An object
#' @return Prototype tibble for new ready4 S3 class (a tibble)
#' @rdname make_pt_tb_for_new_r3_cls
#' @export 
#' @importFrom purrr map_dfr
#' @keywords internal
make_pt_tb_for_new_r3_cls <- function (x) 
{
    pt_tb_for_new_r3_cls_tb <- purrr::map_dfr(x, ~make_one_row_pt_tb_for_new_r3_cls(.x))
    return(pt_tb_for_new_r3_cls_tb)
}
#' Make prototype tibble for new ready4 S4 class
#' @description make_pt_tb_for_new_r4_cls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prototype tibble for new ready4 s4 class. The function returns Prototype tibble for new ready4 S3 class (a tibble).
#' @param x An object
#' @return Prototype tibble for new ready4 S3 class (a tibble)
#' @rdname make_pt_tb_for_new_r4_cls
#' @export 
#' @importFrom purrr map_dfr
#' @keywords internal
make_pt_tb_for_new_r4_cls <- function (x) 
{
    pt_tb_for_new_r3_cls_tb <- purrr::map_dfr(x, ~make_one_row_pt_tb_for_new_r4_cls(.x))
    return(pt_tb_for_new_r3_cls_tb)
}
#' Make show method function
#' @description make_show_mthd_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make show method function. The function returns Show method function (a character vector of length one).
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param meaningful_nms_ls Meaningful names (a list)
#' @return Show method function (a character vector of length one)
#' @rdname make_show_mthd_fn
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom stringr str_c
#' @keywords internal
make_show_mthd_fn <- function (class_nm_1L_chr, meaningful_nms_ls) 
{
    descriptive_str <- purrr::map2_chr(names(meaningful_nms_ls), 
        meaningful_nms_ls, ~paste0("\"  ", .x, ":   \", format(object@", 
            .y, "),  \"\\n\"")) %>% stringr::str_c(sep = "", 
        collapse = ",")
    function_str <- paste0("function(object){\n", "cat(is(object)[[1]], ", 
        "\"\\n\",", descriptive_str, ",\nsep = \"\")}")
    show_mthd_fn_1L_chr <- paste0("methods::setMethod(\"show\",\n", 
        make_alg_to_gen_ref_to_cls(class_nm_1L_chr), ",\n", function_str, 
        ",\nwhere =  ", "globalenv()", "\n)")
    return(show_mthd_fn_1L_chr)
}
