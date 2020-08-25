#' Create accessors
#' @description create_accessors() is a Create function that creates a new R object. Specifically, this function implements an algorithm to create accessors.NA
#' @param slot_name_chr Slot name (a character vector of length 1)
#' @param set_only PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION
#' @param class_name PARAM_DESCRIPTION
#' @param print_accessors PARAM_DESCRIPTION
#' @param output_folder PARAM_DESCRIPTION
#' @param ignore_ns_chr Ignore namespace (a character vector of length 1)
#' @param required_pckg_chr_vec Required pckg (a character vector)
#' @return NULL
#' @rdname create_accessors
#' @export 

#' @keywords internal
create_accessors <- function (slot_name_chr, set_only, parent, class_name, print_accessors, 
    output_folder, ignore_ns_chr, required_pckg_chr_vec) 
{
    current_generics_ls <- make_and_tf_curr_gen_ls(required_pckg_chr_vec = required_pckg_chr_vec, 
        generic_chr = slot_name_chr, ignore_ns_chr = ignore_ns_chr)
    import_packages_ls <- make_import_packages_ls(current_generics_ls = current_generics_ls, 
        fn_name_chr = slot_name_chr, ignore_ns_chr = ignore_ns_chr)
    write_accessors(slot_name_chr = slot_name_chr, set_only = set_only, 
        import_packages_ls = import_packages_ls, class_name = class_name, 
        print_accessors = print_accessors, output_folder = output_folder)
}
#' Create accessors rec
#' @description create_accessors_rec() is a Create function that creates a new R object. Specifically, this function implements an algorithm to create accessors rec.NA
#' @param slot_names_chr_vec Slot names (a character vector)
#' @param set_only PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION
#' @param class_name PARAM_DESCRIPTION
#' @param print_accessors PARAM_DESCRIPTION
#' @param output_folder PARAM_DESCRIPTION
#' @param ignore_ns_chr Ignore namespace (a character vector of length 1)
#' @param required_pckg_chr_vec Required pckg (a character vector)
#' @return NULL
#' @rdname create_accessors_rec
#' @export 
#' @importFrom purrr walk
#' @keywords internal
create_accessors_rec <- function (slot_names_chr_vec, set_only, parent, class_name, print_accessors, 
    output_folder, ignore_ns_chr, required_pckg_chr_vec) 
{
    purrr::walk(slot_names_chr_vec, ~create_accessors(.x, set_only = .x %in% 
        set_only, parent = parent, class_name = class_name, print_accessors = print_accessors, 
        output_folder = output_folder, ignore_ns_chr = ignore_ns_chr, 
        required_pckg_chr_vec = required_pckg_chr_vec))
}
#' Create ready accessors
#' @description create_ready_accessors() is a Create function that creates a new R object. Specifically, this function implements an algorithm to create ready accessors.NA
#' @param class_name PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION
#' @param print_accessors PARAM_DESCRIPTION
#' @param output_folder PARAM_DESCRIPTION
#' @param ignore_ns_chr Ignore namespace (a character vector of length 1)
#' @param required_pckg_chr_vec Required pckg (a character vector)
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname create_ready_accessors
#' @export 
#' @importFrom stringr str_c
#' @keywords internal
create_ready_accessors <- function (class_name, parent, print_accessors, output_folder, 
    ignore_ns_chr, required_pckg_chr_vec, parent_ns_ls) 
{
    slot_names_chr_vec <- get_r4_obj_slots_chr_vec(class_name) %>% 
        names()
    if (is.null(parent)) {
        set_only <- ""
    }
    else {
        set_only <- get_r4_obj_slots_chr_vec(parent, package_chr = resolve_parent_ns_chr(parent_ns_ls)) %>% 
            names()
    }
    accessors <- paste0("create_accessors_rec(", "slot_names_chr_vec = c(\"", 
        slot_names_chr_vec %>% stringr::str_c(collapse = "\",\""), 
        "\")", ",", "set_only = c(\"", set_only %>% stringr::str_c(collapse = "\",\""), 
        "\")", ",parent = \"", parent, "\",", "class_name = \"", 
        class_name, "\", print_accessors = ", print_accessors, 
        ",output_folder = \"", output_folder, "\"", ",ignore_ns_chr = c(\"", 
        ignore_ns_chr %>% stringr::str_c(collapse = "\",\""), 
        "\")", ",required_pckg_chr_vec = \"", required_pckg_chr_vec, 
        "\"", ")")
    return(accessors)
}
#' Create ready helper
#' @description create_ready_helper() is a Create function that creates a new R object. Specifically, this function implements an algorithm to create ready helper.NA
#' @param class_name PARAM_DESCRIPTION
#' @param parent PARAM_DESCRIPTION
#' @param class_slots PARAM_DESCRIPTION
#' @param proto_ls Proto (a list)
#' @param prototype_lup Prototype (a lookup table)
#' @param parent_ns_ls Parent namespace (a list)
#' @return NULL
#' @rdname create_ready_helper
#' @export 
#' @importFrom stringr str_sub str_replace str_replace_all str_c
#' @keywords internal
create_ready_helper <- function (class_name, parent, class_slots, proto_ls, prototype_lup, 
    parent_ns_ls) 
{
    if (!is.null(parent)) {
        child_slots_chr <- class_slots
        class_slots <- get_parent_slot_names(parent_chr = parent, 
            parent_ns_ls = parent_ns_ls)
        parent_proto <- get_parent_prototypes(parent_chr = parent, 
            parent_ns_ls = parent_ns_ls, slot_names_chr_vec = class_slots)
        child_ls_chr <- proto_ls %>% stringr::str_sub(start = 6, 
            end = -2)
        proto_ls <- get_proto_list(class_slots = class_slots, 
            type = parent_proto, prototype_lup = prototype_lup)
        proto_ls <- paste0(proto_ls %>% stringr::str_sub(end = -2), 
            ",", child_ls_chr, ")")
        class_slots <- c(class_slots, child_slots_chr)
    }
    func_args <- proto_ls %>% stringr::str_replace("list", "function") %>% 
        stringr::str_replace_all(",", ",\n")
    helper_function <- paste0(class_name, " <- ", func_args, 
        "{ \n", "methods::new(\"", class_name, "\",\n", paste0(class_slots, 
            " = ", class_slots) %>% stringr::str_c(sep = "", 
            collapse = ",\n"), ")\n}")
    return(helper_function)
}
#' Create ready show mthd
#' @description create_ready_show_mthd() is a Create function that creates a new R object. Specifically, this function implements an algorithm to create ready show mthd.NA
#' @param class_name PARAM_DESCRIPTION
#' @param meaningful_names PARAM_DESCRIPTION
#' @return NULL
#' @rdname create_ready_show_mthd
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom stringr str_c
#' @keywords internal
create_ready_show_mthd <- function (class_name, meaningful_names) 
{
    descriptive_str <- purrr::map2_chr(names(meaningful_names), 
        meaningful_names, ~paste0("\"  ", .x, ":   \", format(object@", 
            .y, "),  \"\\n\"")) %>% stringr::str_c(sep = "", 
        collapse = ",")
    function_str <- paste0("function(object){\n", "cat(is(object)[[1]], ", 
        "\"\\n\",", descriptive_str, ",\nsep = \"\")}")
    paste0("methods::setMethod(\"show\",\n", make_className_chr(class_name), 
        ",\n", function_str, ",\nwhere =  ", "globalenv()", "\n)")
}
