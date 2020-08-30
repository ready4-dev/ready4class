#' Write accessors
#' @description write_accessors() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write accessors.NA
#' @param slot_name_chr Slot name (a character vector of length 1)
#' @param set_only PARAM_DESCRIPTION
#' @param import_packages_ls Import packages (a list)
#' @param class_name PARAM_DESCRIPTION
#' @param print_accessors PARAM_DESCRIPTION
#' @param output_folder PARAM_DESCRIPTION
#' @return NULL
#' @rdname write_accessors
#' @export 
#' @importFrom purrr reduce
#' @keywords internal
write_accessors <- function (slot_name_chr, set_only, import_packages_ls, class_name, 
    print_accessors, output_folder) 
{
    assign_to_slot_chr <- paste0(slot_name_chr, "<-")
    if (!set_only) {
        purrr::reduce(list(getter_ls = list(fn_name_chr = slot_name_chr, 
            args_chr_vec = c("x"), fn = eval(parse(text = paste0("function(x){", 
                "x@", slot_name_chr, "}"))), fn_type_chr_vec = c("gen_get_slot", 
                "meth_get_slot"), import_chr_vec = import_packages_ls$getter_import_pckg), 
            setter_ls = list(fn_name_chr = assign_to_slot_chr, 
                args_chr_vec = c("x", "value"), fn = eval(parse(text = paste0("function(x, value) {", 
                  "\nx@", slot_name_chr, " <- value", "\nmethods::validObject(x)", 
                  "\nx", "\n}"))), fn_type_chr_vec = c("gen_set_slot", 
                  "meth_set_slot"), import_chr_vec = import_packages_ls$setter_import_pckg)), 
            .init = list(new_file_lgl = F, gnr_file = paste0(output_folder, 
                "/gnrc_", slot_name_chr, ".R"), meth_file = ifelse(import_packages_ls$gen_get_exists_lgl, 
                paste0(output_folder, "/gs_", slot_name_chr, 
                  ".R"), paste0(output_folder, "/gnrc_", slot_name_chr, 
                  ".R"))), ~write_gen_meth(fn_name_chr = .y[[1]], 
                args_chr_vec = .y[[2]], package_chr = ".GlobalEnv", 
                where_chr = "globalenv()", class_chr = class_name, 
                fn = .y[[3]], fn_type_chr_vec = .y[[4]], import_chr_vec = .y[[5]], 
                write_file_ls = .x, output_dir_chr = output_folder, 
                append_lgl = T, doc_in_class_lgl = F, generic_exists_lgl = import_packages_ls$gen_get_exists_lgl, 
                s3_lgl = F, write_lgl = print_accessors))
    }
}
#' Write function txt and tags
#' @description write_fn_txt_and_tags() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a function txt and tags.NA
#' @param fn_name PARAM_DESCRIPTION
#' @param fn_text PARAM_DESCRIPTION
#' @param fn_type PARAM_DESCRIPTION
#' @param class_name PARAM_DESCRIPTION
#' @param class_desc PARAM_DESCRIPTION
#' @return NULL
#' @rdname write_fn_txt_and_tags
#' @export 
#' @importFrom ready4fun write_fn_dmt
#' @keywords internal
write_fn_txt_and_tags <- function (fn_name, fn_text, fn_type, class_name, class_desc) 
{
    ready4fun::write_fn_dmt(fn_name_chr = fn_name, fn_type_chr = fn_type, 
        fn = eval(parse(text = fn_text)), class_name_chr = class_name, 
        details_chr = class_desc)
    writeLines(fn_text)
}
#' Write gen meth
#' @description write_gen_meth() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write gen meth.NA
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param args_chr_vec Arguments (a character vector), Default: c("x")
#' @param signature_chr Signature (a character vector of length 1), Default: 'NA'
#' @param package_chr Package (a character vector of length 1), Default: 'NA'
#' @param where_chr Where (a character vector of length 1), Default: 'NA'
#' @param class_chr Class (a character vector of length 1)
#' @param fn Function (a function)
#' @param fn_type_chr_vec Function type (a character vector)
#' @param fn_desc_chr_vec Function description (a character vector), Default: rep(NA_character_, 2)
#' @param fn_title_chr Function title (a character vector of length 1), Default: 'NA'
#' @param fn_out_type_chr Function out type (a character vector of length 1), Default: 'NA'
#' @param import_chr_vec Import (a character vector)
#' @param write_file_ls Write file (a list)
#' @param output_dir_chr Output directory (a character vector of length 1)
#' @param append_lgl Append (a logical vector of length 1), Default: T
#' @param doc_in_class_lgl Document in class (a logical vector of length 1), Default: F
#' @param generic_exists_lgl Generic exists (a logical vector of length 1)
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: F
#' @param s3_lgl S3 (a logical vector of length 1)
#' @param write_lgl Write (a logical vector of length 1)
#' @return NULL
#' @rdname write_gen_meth
#' @export 

#' @keywords internal
write_gen_meth <- function (fn_name_chr, args_chr_vec = c("x"), signature_chr = NA_character_, 
    package_chr = NA_character_, where_chr = NA_character_, class_chr, 
    fn, fn_type_chr_vec, fn_desc_chr_vec = rep(NA_character_, 
        2), fn_title_chr = NA_character_, fn_out_type_chr = NA_character_, 
    import_chr_vec, write_file_ls, output_dir_chr, append_lgl = T, 
    doc_in_class_lgl = F, generic_exists_lgl, overwrite_lgl = F, 
    s3_lgl, write_lgl) 
{
    gen_mthd_pair_ls <- make_gen_mthd_pair_ls(name_chr = fn_name_chr, 
        args_chr_vec = args_chr_vec, signature_chr = signature_chr, 
        package_chr = package_chr, where_chr = where_chr, class_chr = class_chr, 
        fn = fn)
    write_file_ls <- write_generic_fn(write_file_ls = write_file_ls, 
        generic_exists_lgl = generic_exists_lgl, gen_mthd_pair_ls = gen_mthd_pair_ls, 
        fn_name_chr = fn_name_chr, fn_type_chr = fn_type_chr_vec[1], 
        fn_desc_chr = fn_desc_chr_vec[1], fn_out_type_chr = NA_character_, 
        fn_title_chr = fn_title_chr, class_name_chr = class_chr, 
        output_dir_chr = output_dir_chr, overwrite_lgl = overwrite_lgl, 
        s3_lgl = s3_lgl, write_lgl = write_lgl, doc_in_class_lgl = doc_in_class_lgl)
    write_file_ls$new_file_lgl <- ifelse(!overwrite_lgl, T, write_file_ls$new_file_lgl)
    write_method(write_file_ls = write_file_ls, gen_mthd_pair_ls = gen_mthd_pair_ls, 
        class_name_chr = class_chr, fn_name_chr = fn_name_chr, 
        fn_type_chr = fn_type_chr_vec[2], fn_desc_chr = fn_desc_chr_vec[2], 
        fn_out_type_chr = fn_out_type_chr, import_chr_vec = import_chr_vec, 
        write_lgl = write_lgl, append_lgl = append_lgl, doc_in_class_lgl = doc_in_class_lgl)
    write_file_ls
}
#' Write generic
#' @description write_generic_fn() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write generic.NA
#' @param write_file_ls Write file (a list)
#' @param generic_exists_lgl Generic exists (a logical vector of length 1)
#' @param gen_mthd_pair_ls Gen mthd pair (a list)
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_desc_chr Function description (a character vector of length 1), Default: 'NA'
#' @param fn_out_type_chr Function out type (a character vector of length 1), Default: 'NA'
#' @param fn_title_chr Function title (a character vector of length 1), Default: 'NA'
#' @param class_name_chr Class name (a character vector of length 1), Default: 'NA'
#' @param output_dir_chr Output directory (a character vector of length 1), Default: 'NA'
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: F
#' @param s3_lgl S3 (a logical vector of length 1), Default: F
#' @param write_lgl Write (a logical vector of length 1), Default: T
#' @param doc_in_class_lgl Document in class (a logical vector of length 1), Default: F
#' @return NULL
#' @rdname write_generic_fn
#' @export 
#' @importFrom ready4fun write_fn_dmt close_open_sinks
#' @importFrom stringr str_replace
#' @keywords internal
write_generic_fn <- function (write_file_ls, generic_exists_lgl, gen_mthd_pair_ls, 
    fn_name_chr, fn_type_chr, fn_desc_chr = NA_character_, fn_out_type_chr = NA_character_, 
    fn_title_chr = NA_character_, class_name_chr = NA_character_, 
    output_dir_chr = NA_character_, overwrite_lgl = F, s3_lgl = F, 
    write_lgl = T, doc_in_class_lgl = F) 
{
    else_lgl <- write_file_ls$new_file_lgl
    if (!generic_exists_lgl) {
        eval(parse(text = gen_mthd_pair_ls$generic_chr))
        if (write_lgl & (!file.exists(write_file_ls$gnr_file) | 
            write_file_ls$new_file_lgl | overwrite_lgl)) {
            sink(write_file_ls$gnr_file, append = ifelse(fn_type_chr %in% 
                c("gen_std_s3_mthd", "gen_std_s4_mthd"), F, write_file_ls$new_file_lgl))
            ready4fun::write_fn_dmt(fn_name_chr = fn_name_chr, 
                fn_type_chr = fn_type_chr, fn = eval(parse(text = gen_mthd_pair_ls$gen_fn_chr)), 
                fn_desc_chr = fn_desc_chr, fn_out_type_chr = fn_out_type_chr, 
                fn_title_chr = fn_title_chr, doc_in_class_lgl = doc_in_class_lgl)
            writeLines(gen_mthd_pair_ls$generic_chr %>% stringr::str_replace(paste0(",\nwhere =  ", 
                "globalenv\\(\\)"), ""))
            ready4fun::close_open_sinks()
            write_file_ls$new_file_lgl <- T
        }
        write_file_ls$meth_file <- write_file_ls$gnr_file
    }
    else {
        if (!file.exists(write_file_ls$gnr_file)) {
            write_file_ls$meth_file <- paste0(output_dir_chr, 
                ifelse(fn_type_chr %in% c("gen_std_s3_mthd", 
                  "gen_std_s4_mthd"), "/mthd_", "/gs_"), fn_name_chr, 
                ".R")
            if (!file.exists(write_file_ls$meth_file)) 
                file.create(write_file_ls$meth_file)
        }
        else {
            write_file_ls$meth_file <- write_file_ls$gnr_file
        }
    }
    write_file_ls
}
#' Write method
#' @description write_method() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write method.NA
#' @param write_file_ls Write file (a list)
#' @param gen_mthd_pair_ls Gen mthd pair (a list)
#' @param class_name_chr Class name (a character vector of length 1)
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_desc_chr Function description (a character vector of length 1), Default: 'NA'
#' @param fn_out_type_chr Function out type (a character vector of length 1), Default: 'NA'
#' @param import_chr_vec Import (a character vector)
#' @param write_lgl Write (a logical vector of length 1), Default: T
#' @param append_lgl Append (a logical vector of length 1), Default: T
#' @param doc_in_class_lgl Document in class (a logical vector of length 1), Default: F
#' @return NULL
#' @rdname write_method
#' @export 
#' @importFrom ready4fun write_fn_dmt close_open_sinks
#' @importFrom stringr str_replace
#' @keywords internal
write_method <- function (write_file_ls, gen_mthd_pair_ls, class_name_chr, fn_name_chr, 
    fn_type_chr, fn_desc_chr = NA_character_, fn_out_type_chr = NA_character_, 
    import_chr_vec, write_lgl = T, append_lgl = T, doc_in_class_lgl = F) 
{
    eval(parse(text = gen_mthd_pair_ls$method_chr))
    if (write_lgl) {
        sink(write_file_ls$meth_file, append = ifelse(identical(write_file_ls$gen_file, 
            write_file_ls$meth_file), T, ifelse(fn_type_chr %in% 
            c("gen_std_s3_mthd", "gen_std_s4_mthd"), T, write_file_ls$new_file_lgl)))
        ready4fun::write_fn_dmt(fn_name_chr = fn_name_chr, fn_type_chr = fn_type_chr, 
            fn = eval(parse(text = gen_mthd_pair_ls$meth_fn_chr)), 
            fn_desc_chr = fn_desc_chr, fn_out_type_chr = fn_out_type_chr, 
            class_name_chr = class_name_chr, import_chr_vec = import_chr_vec, 
            doc_in_class_lgl = doc_in_class_lgl)
        writeLines(gen_mthd_pair_ls$method_chr %>% stringr::str_replace(paste0(",\nwhere =  ", 
            "globalenv\\(\\)"), ""))
        ready4fun::close_open_sinks()
    }
}
#' Write S3
#' @description write_s3_s4() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a S3.NA
#' @param methods_tb Methods (a tibble)
#' @param fn_ls Function list (a list of functions)
#' @param package_chr Package (a character vector of length 1)
#' @param output_dir_chr Output directory (a character vector of length 1)
#' @return NULL
#' @rdname write_s3_s4
#' @export 
#' @importFrom purrr pwalk
#' @importFrom dplyr mutate
#' @keywords internal
write_s3_s4 <- function (methods_tb, fn_ls, package_chr, output_dir_chr) 
{
    purrr::pwalk(methods_tb %>% dplyr::mutate(first_lgl = c(T, 
        rep(F, length(fn_ls) - 1))) %>% dplyr::mutate(append_lgl = c(F, 
        rep(T, length(fn_ls) - 1))), ~write_std_method(fn = fn_ls[[..1]], 
        fn_name_chr = ..2, class_chr = ..3, fn_desc_chr_vec = c(..4, 
            ..5), fn_title_chr = ..6, fn_out_type_chr = ..7, 
        package_chr = package_chr, output_dir_chr = output_dir_chr, 
        signature_chr = ..8, append_lgl = ..10, first_lgl = ..9))
}
#' Write standard method
#' @description write_std_method() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a standard method.NA
#' @param fn Function (a function)
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param class_chr Class (a character vector of length 1)
#' @param fn_desc_chr_vec Function description (a character vector)
#' @param fn_title_chr Function title (a character vector of length 1)
#' @param fn_out_type_chr Function out type (a character vector of length 1)
#' @param package_chr Package (a character vector of length 1)
#' @param output_dir_chr Output directory (a character vector of length 1)
#' @param signature_chr Signature (a character vector of length 1), Default: 'NA'
#' @param append_lgl Append (a logical vector of length 1), Default: T
#' @param first_lgl First (a logical vector of length 1), Default: T
#' @return NULL
#' @rdname write_std_method
#' @export 
#' @importFrom testit assert
#' @importFrom purrr discard
#' @keywords internal
write_std_method <- function (fn, fn_name_chr, class_chr, fn_desc_chr_vec, fn_title_chr, 
    fn_out_type_chr, package_chr, output_dir_chr, signature_chr = NA_character_, 
    append_lgl = T, first_lgl = T) 
{
    s3_lgl = !isS4(eval(parse(text = paste0(class_chr, "()"))))
    testit::assert("x" %in% formalArgs(fn))
    fn_type_chr_vec <- paste0(c("gen_", "meth_"), "std_", ifelse(s3_lgl, 
        "s3", "s4"), "_mthd")
    write_file_ls <- list(new_file_lgl = F, gnr_file = paste0(output_dir_chr, 
        "/gnrc_", fn_name_chr, ".R"), meth_file = paste0(output_dir_chr, 
        "/meth_", fn_name_chr, ".R"))
    current_generics_ls <- make_and_tf_curr_gen_ls(required_pckg_chr_vec = NA_character_, 
        generic_chr = fn_name_chr, ignore_ns_chr = ifelse(package_chr %in% 
            rownames(installed.packages()), package_chr, NA_character_))
    import_packages_ls <- make_import_packages_ls(current_generics_ls = current_generics_ls, 
        fn_name_chr = fn_name_chr, ignore_ns_chr = ifelse(package_chr %in% 
            rownames(installed.packages()), package_chr, NA_character_))
    generic_exists_lgl <- import_packages_ls$gen_get_exists_lgl
    import_chr_vec <- import_packages_ls$getter_import_pckg[import_packages_ls$getter_import_pckg != 
        package_chr]
    if (identical(import_chr_vec, character(0))) 
        import_chr_vec <- NA_character_
    write_file_ls <- write_gen_meth(fn_name_chr = fn_name_chr, 
        args_chr_vec = c("x", ifelse(length(formalArgs(fn)) > 
            1, "...", NA_character_)) %>% purrr::discard(is.na), 
        signature_chr = signature_chr, package_chr = NA_character_, 
        where_chr = NA_character_, class_chr = class_chr, fn = fn, 
        fn_type_chr_vec = fn_type_chr_vec, fn_desc_chr_vec = fn_desc_chr_vec, 
        fn_title_chr = fn_title_chr, fn_out_type_chr = fn_out_type_chr, 
        import_chr_vec = import_chr_vec, write_file_ls = write_file_ls, 
        output_dir_chr = output_dir_chr, append_lgl = append_lgl, 
        doc_in_class_lgl = F, generic_exists_lgl = generic_exists_lgl, 
        overwrite_lgl = !append_lgl, s3_lgl = s3_lgl, write_lgl = T)
    write_file_ls
}
