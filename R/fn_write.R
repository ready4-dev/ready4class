#' Write getter setter methods for ready4 S4
#' @description write_gtr_str_mthds_for_r4() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write getter setter methods for ready4 s4. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param slot_nm_1L_chr Slot name (a character vector of length one)
#' @param set_only_1L_lgl Set only (a logical vector of length one)
#' @param pkgs_to_imp_ls Packages to import (a list)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param print_gtrs_strs_1L_lgl Print getters setters (a logical vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_gtr_str_mthds_for_r4
#' @export 
#' @importFrom ready4fun get_rds_from_pkg_dmt
#' @importFrom purrr reduce
#' @keywords internal
write_gtr_str_mthds_for_r4 <- function (slot_nm_1L_chr, set_only_1L_lgl, pkgs_to_imp_ls, class_nm_1L_chr, 
    print_gtrs_strs_1L_lgl, output_dir_1L_chr, fn_types_lup = NULL, 
    object_type_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = "ready4-dev/ready4")
    assign_to_slot_chr <- paste0(slot_nm_1L_chr, "<-")
    if (!set_only_1L_lgl) {
        purrr::reduce(list(getter_ls = list(fn_name_1L_chr = slot_nm_1L_chr, 
            args_chr = c("x"), fn = eval(parse(text = paste0("function(x){", 
                "x@", slot_nm_1L_chr, "}"))), fn_type_chr = c("gen_get_slot", 
                "meth_get_slot"), imports_chr = pkgs_to_imp_ls$gtr_imps_chr), 
            setter_ls = list(fn_name_1L_chr = assign_to_slot_chr, 
                args_chr = c("x", "value"), fn = eval(parse(text = paste0("function(x, value) {", 
                  "\nx@", slot_nm_1L_chr, " <- value", "\nmethods::validObject(x)", 
                  "\nx", "\n}"))), fn_type_chr = c("gen_set_slot", 
                  "meth_set_slot"), imports_chr = pkgs_to_imp_ls$str_imps_chr)), 
            .init = list(new_file_lgl = F, gnr_file = paste0(output_dir_1L_chr, 
                "/gnrc_", slot_nm_1L_chr, ".R"), meth_file = ifelse(pkgs_to_imp_ls$gnrc_gtr_exists_1L_lgl, 
                paste0(output_dir_1L_chr, "/gs_", slot_nm_1L_chr, 
                  ".R"), paste0(output_dir_1L_chr, "/gnrc_", 
                  slot_nm_1L_chr, ".R"))), ~write_scripts_to_make_gnrc_and_mthd(fn_name_1L_chr = .y[[1]], 
                args_chr = .y[[2]], pkg_nm_1L_chr = ".GlobalEnv", 
                where_chr = "globalenv()", class_nm_1L_chr = class_nm_1L_chr, 
                fn = .y[[3]], fn_type_chr = .y[[4]], imports_chr = .y[[5]], 
                write_file_ls = .x, output_dir_1L_chr = output_dir_1L_chr, 
                append_1L_lgl = T, doc_in_class_1L_lgl = F, gnrc_exists_1L_lgl = pkgs_to_imp_ls$gnrc_gtr_exists_1L_lgl, 
                s3_1L_lgl = F, write_1L_lgl = print_gtrs_strs_1L_lgl, 
                fn_types_lup = fn_types_lup, object_type_lup = object_type_lup))
    }
}
#' Write getter setter methods for slots
#' @description write_gtr_str_mthds_for_slots() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write getter setter methods for slots. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param slot_names_chr Slot names (a character vector)
#' @param set_only_chr Set only (a character vector)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param print_gtrs_strs_1L_lgl Print getters setters (a logical vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Require packages (a character vector)
#' @param fn_types_lup Function types (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @return NULL
#' @rdname write_gtr_str_mthds_for_slots
#' @export 
#' @importFrom purrr map_chr walk
#' @importFrom stringr str_replace
#' @keywords internal
write_gtr_str_mthds_for_slots <- function (slot_names_chr, set_only_chr, parent_cls_nm_1L_chr, 
    class_nm_1L_chr, print_gtrs_strs_1L_lgl, output_dir_1L_chr, 
    nss_to_ignore_chr, req_pkgs_chr, fn_types_lup, object_type_lup) 
{
    req_pkgs_chr <- purrr::map_chr(req_pkgs_chr, ~stringr::str_replace(.x, 
        "NA", NA_character_))
    nss_to_ignore_chr <- purrr::map_chr(nss_to_ignore_chr, ~stringr::str_replace(.x, 
        "NA", NA_character_))
    purrr::walk(slot_names_chr, ~write_slot_gtr_str_mthds(.x, 
        set_only_1L_lgl = .x %in% set_only_chr, parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
        class_nm_1L_chr = class_nm_1L_chr, print_gtrs_strs_1L_lgl = print_gtrs_strs_1L_lgl, 
        output_dir_1L_chr = output_dir_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr, 
        req_pkgs_chr = req_pkgs_chr, fn_types_lup = fn_types_lup, 
        object_type_lup = object_type_lup))
}
#' Write methods for ready4 S3 or ready4 S4 classes
#' @description write_mthds_for_r3_or_r4_clss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write methods for ready4 s3 or ready4 s4 classes. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param methods_tb Methods (a tibble)
#' @param fn_ls Function list (a list of functions)
#' @param fn_types_lup Function types (a lookup table)
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @return NULL
#' @rdname write_mthds_for_r3_or_r4_clss
#' @export 
#' @importFrom purrr pwalk
#' @importFrom dplyr mutate
#' @keywords internal
write_mthds_for_r3_or_r4_clss <- function (methods_tb, fn_ls, fn_types_lup, pkg_nm_1L_chr, output_dir_1L_chr) 
{
    purrr::pwalk(methods_tb %>% dplyr::mutate(first_lgl = c(T, 
        rep(F, length(fn_ls) - 1))) %>% dplyr::mutate(append_lgl = c(F, 
        rep(T, length(fn_ls) - 1))), ~write_std_mthd(fn = fn_ls[[..1]], 
        fn_name_1L_chr = ..2, class_nm_1L_chr = ..3, fn_desc_chr = c(..4, 
            ..5), fn_title_1L_chr = ..6, fn_outp_type_1L_chr = ..7, 
        pkg_nm_1L_chr = pkg_nm_1L_chr, output_dir_1L_chr = output_dir_1L_chr, 
        signature_1L_chr = ..8, append_1L_lgl = ..10, first_1L_lgl = ..9, 
        fn_types_lup = fn_types_lup))
}
#' Write ready4 S4 methods
#' @description write_r4_mthds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write ready4 s4 methods. The function returns a S4 methods (a list).
#' @param fns_dir_1L_chr Functions directory (a character vector of length one), Default: 'data-raw/s4_fns'
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param import_from_chr Import from (a character vector), Default: character(0)
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'R'
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: character(0)
#' @return a S4 methods (a list)
#' @rdname write_r4_mthds
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm make_gnrc_imports
#' @importFrom purrr map2 pluck
#' @keywords internal
write_r4_mthds <- function (fns_dir_1L_chr = "data-raw/s4_fns", fn_types_lup = NULL, 
    import_from_chr = character(0), output_dir_1L_chr = "R", 
    pkg_nm_1L_chr = character(0)) 
{
    if (identical(pkg_nm_1L_chr, character(0))) 
        pkg_nm_1L_chr <- ready4fun::get_dev_pkg_nm()
    if (identical(import_from_chr, character(0))) 
        import_from_chr <- ready4fun::make_gnrc_imports()
    s4_mthds_ls <- make_s4_mthds_ls(fns_dir_1L_chr)
    if (!is.null(s4_mthds_ls)) {
        written_files_ls_ls <- purrr::map2(s4_mthds_ls$mthds_ls, 
            names(s4_mthds_ls$mthds_ls), ~{
                fn_name_1L_chr <- .y
                classes_chr <- names(.x)
                fns_chr <- unname(.x)
                purrr::map2(classes_chr, fns_chr, ~{
                  class_nm_1L_chr <- .x
                  fn <- s4_mthds_ls$env_ls$fns_env %>% purrr::pluck(.y)
                  fn_desc_chr <- rep(paste0(fn_name_1L_chr, " method applied to ", 
                    class_nm_1L_chr), 2)
                  fn_outp_type_1L_chr <- ""
                  write_std_mthd(fn, fn_name_1L_chr = fn_name_1L_chr, 
                    class_nm_1L_chr = class_nm_1L_chr, fn_desc_chr = fn_desc_chr, 
                    fn_outp_type_1L_chr = fn_outp_type_1L_chr, 
                    fn_types_lup = fn_types_lup, pkg_nm_1L_chr = pkg_nm_1L_chr, 
                    output_dir_1L_chr = output_dir_1L_chr, import_from_chr = import_from_chr)
                })
            })
    }
    else {
        written_files_ls_ls <- NULL
    }
    return(s4_mthds_ls)
}
#' Write script to make generic
#' @description write_script_to_make_gnrc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write script to make generic. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param write_file_ls Write file (a list)
#' @param gnrc_exists_1L_lgl Generic exists (a logical vector of length one)
#' @param gen_mthd_pair_ls Generate method pair (a list)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_desc_1L_chr Function description (a character vector of length one), Default: 'NA'
#' @param fn_outp_type_1L_chr Function output type (a character vector of length one), Default: 'NA'
#' @param fn_title_1L_chr Function title (a character vector of length one), Default: 'NA'
#' @param class_nm_1L_chr Class name (a character vector of length one), Default: 'NA'
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'NA'
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param s3_1L_lgl S3 (a logical vector of length one), Default: F
#' @param write_1L_lgl Write (a logical vector of length one), Default: T
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one), Default: F
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname write_script_to_make_gnrc
#' @export 
#' @importFrom ready4fun get_rds_from_pkg_dmt make_lines_for_fn_dmt close_open_sinks
#' @importFrom ready4 make_prompt
#' @importFrom stringr str_replace str_remove
#' @keywords internal
write_script_to_make_gnrc <- function (write_file_ls, gnrc_exists_1L_lgl, gen_mthd_pair_ls, 
    fn_name_1L_chr, fn_type_1L_chr, fn_desc_1L_chr = NA_character_, 
    fn_outp_type_1L_chr = NA_character_, fn_title_1L_chr = NA_character_, 
    class_nm_1L_chr = NA_character_, output_dir_1L_chr = NA_character_, 
    overwrite_1L_lgl = F, s3_1L_lgl = F, write_1L_lgl = T, doc_in_class_1L_lgl = F, 
    fn_types_lup = NULL, object_type_lup = NULL, consent_1L_chr = NULL) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = "ready4-dev/ready4")
    else_lgl <- write_file_ls$new_file_lgl
    if (!gnrc_exists_1L_lgl) {
        eval(parse(text = gen_mthd_pair_ls$generic_1L_chr))
        if (write_1L_lgl & (!file.exists(write_file_ls$gnr_file) | 
            write_file_ls$new_file_lgl | overwrite_1L_lgl)) {
            if (is.null(consent_1L_chr)) {
                consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                  write_file_ls$gnr_file, " ?"), options_chr = c("Y", 
                  "N"), force_from_opts_1L_chr = T)
            }
            if (consent_1L_chr == "Y") {
                sink(write_file_ls$gnr_file, append = ifelse(fn_type_1L_chr %in% 
                  c("gen_std_s3_mthd", "gen_std_s4_mthd"), F, 
                  write_file_ls$new_file_lgl))
                ready4fun::make_lines_for_fn_dmt(fn_name_1L_chr = fn_name_1L_chr, 
                  fn_type_1L_chr = fn_type_1L_chr, fn = eval(parse(text = gen_mthd_pair_ls$gen_fn_chr)), 
                  fn_desc_1L_chr = fn_desc_1L_chr, fn_out_type_1L_chr = fn_outp_type_1L_chr, 
                  fn_title_1L_chr = fn_title_1L_chr, fn_types_lup = fn_types_lup, 
                  doc_in_class_1L_lgl = doc_in_class_1L_lgl, 
                  object_type_lup = object_type_lup)
                writeLines(gen_mthd_pair_ls$generic_1L_chr %>% 
                  stringr::str_replace(paste0(",\nwhere =  ", 
                    "globalenv\\(\\)"), ""))
                ready4fun::close_open_sinks()
                write_file_ls$new_file_lgl <- T
            }
        }
        write_file_ls$meth_file <- write_file_ls$gnr_file
    }
    else {
        if (!file.exists(write_file_ls$gnr_file)) {
            write_file_ls$meth_file <- paste0(output_dir_1L_chr, 
                ifelse(fn_type_1L_chr %in% c("gen_std_s3_mthd", 
                  "gen_std_s4_mthd"), "/mthd_", "/gs_"), fn_name_1L_chr %>% 
                  stringr::str_remove("<-"), ".R")
            if (!file.exists(write_file_ls$meth_file)) 
                file.create(write_file_ls$meth_file)
        }
        else {
            write_file_ls$meth_file <- write_file_ls$gnr_file
        }
    }
    write_file_ls
}
#' Write script to make method
#' @description write_script_to_make_mthd() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write script to make method. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param write_file_ls Write file (a list)
#' @param gen_mthd_pair_ls Generate method pair (a list)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_desc_1L_chr Function description (a character vector of length one), Default: 'NA'
#' @param fn_outp_type_1L_chr Function output type (a character vector of length one), Default: 'NA'
#' @param imports_chr Imports (a character vector)
#' @param write_1L_lgl Write (a logical vector of length one), Default: T
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one), Default: F
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @param import_from_chr Import from (a character vector), Default: 'NA'
#' @return NULL
#' @rdname write_script_to_make_mthd
#' @export 
#' @importFrom ready4fun get_rds_from_pkg_dmt make_lines_for_fn_dmt close_open_sinks
#' @importFrom ready4 make_prompt
#' @importFrom stringr str_replace str_replace_all
#' @keywords internal
write_script_to_make_mthd <- function (write_file_ls, gen_mthd_pair_ls, class_nm_1L_chr, fn_name_1L_chr, 
    fn_type_1L_chr, fn_desc_1L_chr = NA_character_, fn_outp_type_1L_chr = NA_character_, 
    imports_chr, write_1L_lgl = T, append_1L_lgl = T, doc_in_class_1L_lgl = F, 
    fn_types_lup = NULL, object_type_lup = NULL, consent_1L_chr = NULL, 
    import_from_chr = NA_character_) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = "ready4-dev/ready4")
    eval(parse(text = gen_mthd_pair_ls$method_chr))
    if (write_1L_lgl) {
        if (is.null(consent_1L_chr)) {
            consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                write_file_ls$meth_file, " ?"), options_chr = c("Y", 
                "N"), force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr == "Y") {
            sink(write_file_ls$meth_file, append = ifelse(identical(write_file_ls$gen_file, 
                write_file_ls$meth_file), T, ifelse(fn_type_1L_chr %in% 
                c("gen_std_s3_mthd", "gen_std_s4_mthd"), T, write_file_ls$new_file_lgl)))
            ready4fun::make_lines_for_fn_dmt(fn_name_1L_chr = fn_name_1L_chr, 
                fn_type_1L_chr = fn_type_1L_chr, fn = eval(parse(text = gen_mthd_pair_ls$meth_fn_chr)), 
                fn_desc_1L_chr = fn_desc_1L_chr, fn_out_type_1L_chr = fn_outp_type_1L_chr, 
                fn_types_lup = fn_types_lup, class_name_1L_chr = class_nm_1L_chr, 
                import_chr = imports_chr, import_from_chr = import_from_chr, 
                doc_in_class_1L_lgl = doc_in_class_1L_lgl, object_type_lup = object_type_lup)
            writeLines(gen_mthd_pair_ls$method_chr %>% stringr::str_replace(paste0(",\nwhere =  ", 
                "globalenv\\(\\)"), "") %>% stringr::str_replace_all(",..GlobalEnv\"", 
                ""))
            ready4fun::close_open_sinks()
        }
    }
}
#' Write scripts to make generic and method
#' @description write_scripts_to_make_gnrc_and_mthd() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write scripts to make generic and method. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param args_chr Arguments (a character vector), Default: c("x")
#' @param signature_1L_chr Signature (a character vector of length one), Default: 'NA'
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'NA'
#' @param where_chr Where (a character vector), Default: 'NA'
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param fn Function (a function)
#' @param fn_type_chr Function type (a character vector)
#' @param fn_desc_chr Function description (a character vector), Default: rep(NA_character_, 2)
#' @param fn_title_1L_chr Function title (a character vector of length one), Default: 'NA'
#' @param fn_outp_type_1L_chr Function output type (a character vector of length one), Default: 'NA'
#' @param imports_chr Imports (a character vector)
#' @param write_file_ls Write file (a list)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one), Default: F
#' @param gnrc_exists_1L_lgl Generic exists (a logical vector of length one)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param s3_1L_lgl S3 (a logical vector of length one)
#' @param write_1L_lgl Write (a logical vector of length one)
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param import_from_chr Import from (a character vector), Default: 'NA'
#' @return NULL
#' @rdname write_scripts_to_make_gnrc_and_mthd
#' @export 
#' @importFrom ready4fun get_rds_from_pkg_dmt
#' @keywords internal
write_scripts_to_make_gnrc_and_mthd <- function (fn_name_1L_chr, args_chr = c("x"), signature_1L_chr = NA_character_, 
    pkg_nm_1L_chr = NA_character_, where_chr = NA_character_, 
    class_nm_1L_chr, fn, fn_type_chr, fn_desc_chr = rep(NA_character_, 
        2), fn_title_1L_chr = NA_character_, fn_outp_type_1L_chr = NA_character_, 
    imports_chr, write_file_ls, output_dir_1L_chr, append_1L_lgl = T, 
    doc_in_class_1L_lgl = F, gnrc_exists_1L_lgl, overwrite_1L_lgl = F, 
    s3_1L_lgl, write_1L_lgl, fn_types_lup = NULL, object_type_lup = NULL, 
    import_from_chr = NA_character_) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = "ready4-dev/ready4")
    gen_mthd_pair_ls <- make_gnrc_mthd_pair_ls(name_1L_chr = fn_name_1L_chr, 
        args_chr = args_chr, signature_1L_chr = signature_1L_chr, 
        pkg_nm_1L_chr = pkg_nm_1L_chr, where_1L_chr = where_chr, 
        class_nm_1L_chr = class_nm_1L_chr, fn = fn)
    write_file_ls <- write_script_to_make_gnrc(write_file_ls = write_file_ls, 
        gnrc_exists_1L_lgl = gnrc_exists_1L_lgl, gen_mthd_pair_ls = gen_mthd_pair_ls, 
        fn_name_1L_chr = fn_name_1L_chr, fn_type_1L_chr = fn_type_chr[1], 
        fn_desc_1L_chr = fn_desc_chr[1], fn_outp_type_1L_chr = NA_character_, 
        fn_title_1L_chr = fn_title_1L_chr, class_nm_1L_chr = class_nm_1L_chr, 
        output_dir_1L_chr = output_dir_1L_chr, overwrite_1L_lgl = overwrite_1L_lgl, 
        s3_1L_lgl = s3_1L_lgl, write_1L_lgl = write_1L_lgl, doc_in_class_1L_lgl = doc_in_class_1L_lgl, 
        fn_types_lup = fn_types_lup, object_type_lup = object_type_lup)
    write_file_ls$new_file_lgl <- ifelse(!overwrite_1L_lgl, T, 
        write_file_ls$new_file_lgl)
    write_script_to_make_mthd(write_file_ls = write_file_ls, 
        gen_mthd_pair_ls = gen_mthd_pair_ls, class_nm_1L_chr = class_nm_1L_chr, 
        fn_name_1L_chr = fn_name_1L_chr, fn_type_1L_chr = fn_type_chr[2], 
        fn_desc_1L_chr = fn_desc_chr[2], fn_outp_type_1L_chr = fn_outp_type_1L_chr, 
        imports_chr = imports_chr, write_1L_lgl = write_1L_lgl, 
        append_1L_lgl = append_1L_lgl, doc_in_class_1L_lgl = doc_in_class_1L_lgl, 
        fn_types_lup = fn_types_lup, object_type_lup = object_type_lup, 
        import_from_chr = import_from_chr)
    write_file_ls
}
#' Write scripts to make classes
#' @description write_scripts_to_mk_clss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write scripts to make classes. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param pts_for_new_clss_ls Prototypes for new classes (a list)
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param class_pfx_1L_chr Class prefix (a character vector of length one)
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param pt_lup Prototype (a lookup table)
#' @param description_ls Description (a list), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @return NULL
#' @rdname write_scripts_to_mk_clss
#' @export 
#' @importFrom usethis use_data
#' @importFrom ready4fun write_pt_lup_db
#' @importFrom devtools document load_all
#' @keywords internal
write_scripts_to_mk_clss <- function (pts_for_new_clss_ls, pkg_nm_1L_chr, class_pfx_1L_chr, 
    R_dir_1L_chr = "R", pt_lup, description_ls = NULL, nss_to_ignore_chr = NA_character_, 
    req_pkgs_chr = NA_character_) 
{
    reset_pkg_files_R(pkg_nm_1L_chr, description_ls = description_ls)
    pt_lup <- make_class_pts_tb(pts_for_new_clss_ls) %>% author.ready4class_constructor(dev_pkg_ns_1L_chr = pkg_nm_1L_chr, 
        name_pfx_1L_chr = class_pfx_1L_chr, output_dir_1L_chr = R_dir_1L_chr, 
        file_exists_cdn_1L_chr = "overwrite", init_class_pt_lup = pt_lup, 
        nss_to_ignore_chr = nss_to_ignore_chr, req_pkgs_chr = req_pkgs_chr, 
        class_in_cache_cdn_1L_chr = "overwrite", object_type_lup = object_type_lup)
    usethis::use_data(pt_lup, overwrite = T)
    ready4fun::write_pt_lup_db()
    devtools::document()
    devtools::load_all()
    pt_lup
}
#' Write scripts to make ready4 S3 class
#' @description write_scripts_to_mk_r3_cls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write scripts to make ready4 s3 class. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param name_stub_1L_chr Name stub (a character vector of length one)
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'data-raw'
#' @param class_desc_1L_chr Class description (a character vector of length one), Default: ''
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one), Default: NULL
#' @param type_1L_chr Type (a character vector of length one)
#' @param pt_ns_1L_chr Prototype namespace (a character vector of length one), Default: ''
#' @param pt_chkr_pfx_1L_chr Prototype checker prefix (a character vector of length one), Default: 'is.'
#' @param vals_ls Values (a list), Default: NULL
#' @param ordered_1L_lgl Ordered (a logical vector of length one), Default: FALSE
#' @param allowed_vals_ls Allowed values (a list), Default: NULL
#' @param min_max_vals_dbl Minimum maximum values (a double vector), Default: NULL
#' @param start_end_vals_dbl Start end values (a double vector), Default: NULL
#' @param prototype_lup Prototype (a lookup table)
#' @param dev_pkg_ns_1L_chr Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param file_exists_cdn_1L_chr File exists condition (a character vector of length one), Default: 'skip'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param asserts_ls Asserts (a list), Default: NULL
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table)
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname write_scripts_to_mk_r3_cls
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm close_open_sinks
#' @importFrom ready4 make_prompt
#' @importFrom purrr pwalk
#' @importFrom devtools document load_all
#' @keywords internal
write_scripts_to_mk_r3_cls <- function (name_stub_1L_chr, name_pfx_1L_chr, output_dir_1L_chr = "data-raw", 
    class_desc_1L_chr = "", parent_cls_nm_1L_chr = NULL, type_1L_chr, 
    pt_ns_1L_chr = "", pt_chkr_pfx_1L_chr = "is.", vals_ls = NULL, 
    ordered_1L_lgl = FALSE, allowed_vals_ls = NULL, min_max_vals_dbl = NULL, 
    start_end_vals_dbl = NULL, prototype_lup, dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(), 
    nss_to_ignore_chr = NA_character_, file_exists_cdn_1L_chr = "skip", 
    abbreviations_lup, asserts_ls = NULL, fn_types_lup = NULL, 
    object_type_lup, consent_1L_chr = NULL) 
{
    if (!dir.exists(output_dir_1L_chr)) 
        dir.create(output_dir_1L_chr)
    class_nm_1L_chr <- paste0(name_pfx_1L_chr, name_stub_1L_chr)
    class_file_chr <- get_class_fl_nms(class_names_chr = class_nm_1L_chr, 
        s3_1L_lgl = T, output_dir_1L_chr = output_dir_1L_chr)
    if (file_exists_cdn_1L_chr == "overwrite") {
        if (file.exists(class_file_chr)) 
            file.remove(class_file_chr)
    }
    if (file_exists_cdn_1L_chr %in% c("append", "overwrite")) {
        s3_components_ls <- make_pt_ls_for_new_r3_cls(class_name_1L_chr = class_nm_1L_chr, 
            type_1L_chr = type_1L_chr, pt_ns_1L_chr = pt_ns_1L_chr, 
            pt_chkr_pfx_1L_chr = pt_chkr_pfx_1L_chr, vals_ls = vals_ls, 
            ordered_1L_lgl = ordered_1L_lgl, parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            prototype_lup = prototype_lup, min_max_vals_dbl = min_max_vals_dbl, 
            start_end_vals_dbl = start_end_vals_dbl, dev_pkg_ns_1L_chr = dev_pkg_ns_1L_chr, 
            nss_to_ignore_chr = nss_to_ignore_chr, asserts_ls = asserts_ls)
        if (is.null(consent_1L_chr)) {
            consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                class_file_chr, " ?"), options_chr = c("Y", "N"), 
                force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr == "Y") {
            sink(class_file_chr, append = ifelse(file_exists_cdn_1L_chr == 
                "append", TRUE, FALSE))
            writeLines(s3_components_ls$include_tags_chr)
            if (type_1L_chr == "tibble") {
                writeLines(make_alg_to_set_old_clss(class_nm_1L_chr))
            }
            purrr::pwalk(list(s3_components_ls$fn_name_ls, s3_components_ls$fn_body_1L_chr_ls, 
                c("s3_valid_instance", "s3_unvalidated_instance", 
                  "s3_prototype", "s3_validator", "s3_checker")), 
                ~make_lines_for_writing_dmtd_fn(fn_name_1L_chr = ..1, 
                  fn_body_1L_chr = ..2, fn_type_1L_chr = ..3, 
                  class_nm_1L_chr = class_nm_1L_chr, class_desc_1L_chr = class_desc_1L_chr, 
                  abbreviations_lup = abbreviations_lup, fn_types_lup = fn_types_lup, 
                  object_type_lup = object_type_lup))
            ready4fun::close_open_sinks()
        }
        devtools::document()
        devtools::load_all()
    }
}
#' Write scripts to make ready4 S4 class
#' @description write_scripts_to_mk_r4_cls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write scripts to make ready4 s4 class. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param name_stub_1L_chr Name stub (a character vector of length one)
#' @param name_pfx_1L_chr Name prefix (a character vector of length one)
#' @param slots_chr Slots (a character vector)
#' @param type_chr Type (a character vector)
#' @param prototype_lup Prototype (a lookup table)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param accessors_1L_lgl Accessors (a logical vector of length one), Default: F
#' @param allowed_vals_ls Allowed values (a list), Default: NULL
#' @param asserts_ls Asserts (a list), Default: NULL
#' @param class_desc_1L_chr Class description (a character vector of length one), Default: ''
#' @param class_in_cache_cdn_1L_chr Class in cache condition (a character vector of length one), Default: 'stop'
#' @param clss_to_inc_chr Classes to include (a character vector), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @param meaningful_nms_ls Meaningful names (a list), Default: NULL
#' @param object_type_lup Object type (a lookup table)
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: 'data-raw'
#' @param outp_sub_dir_1L_chr Output sub directory (a character vector of length one), Default: NULL
#' @param names_must_match_ls Names must match (a list), Default: NULL
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector), Default: 'NA'
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one), Default: NULL
#' @param req_pkgs_chr Require packages (a character vector), Default: 'NA'
#' @param slots_of_dif_lnts_chr Slots of different lengths (a character vector), Default: NULL
#' @param vals_ls Values (a list), Default: NULL
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param helper_1L_lgl Helper (a logical vector of length one), Default: F
#' @param print_set_cls_1L_lgl Print set class (a logical vector of length one), Default: TRUE
#' @param print_gtrs_strs_1L_lgl Print getters setters (a logical vector of length one), Default: TRUE
#' @param print_validator_1L_lgl Print validator (a logical vector of length one), Default: TRUE
#' @param print_meaningful_nms_ls_1L_lgl Print meaningful names list (a logical vector of length one), Default: TRUE
#' @return NULL
#' @rdname write_scripts_to_mk_r4_cls
#' @export 
#' @importFrom ready4 make_prompt
#' @importFrom ready4fun make_lines_for_fn_dmt close_open_sinks
#' @importFrom stringr str_replace str_replace_all
#' @importFrom devtools document load_all
#' @keywords internal
write_scripts_to_mk_r4_cls <- function (name_stub_1L_chr, name_pfx_1L_chr, slots_chr, type_chr, 
    prototype_lup, abbreviations_lup = NULL, accessors_1L_lgl = F, 
    allowed_vals_ls = NULL, asserts_ls = NULL, class_desc_1L_chr = "", 
    class_in_cache_cdn_1L_chr = "stop", clss_to_inc_chr = NULL, 
    consent_1L_chr = NULL, meaningful_nms_ls = NULL, object_type_lup, 
    output_dir_1L_chr = "data-raw", outp_sub_dir_1L_chr = NULL, 
    names_must_match_ls = NULL, nss_to_ignore_chr = NA_character_, 
    parent_cls_nm_1L_chr = NULL, req_pkgs_chr = NA_character_, 
    slots_of_dif_lnts_chr = NULL, vals_ls = NULL, fn_types_lup = NULL, 
    helper_1L_lgl = F, print_set_cls_1L_lgl = TRUE, print_gtrs_strs_1L_lgl = TRUE, 
    print_validator_1L_lgl = TRUE, print_meaningful_nms_ls_1L_lgl = TRUE) 
{
    if (!is.null(outp_sub_dir_1L_chr)) {
        output_dir_1L_chr <- paste0(output_dir_1L_chr, "/", outp_sub_dir_1L_chr)
        if (!dir.exists(output_dir_1L_chr)) 
            dir.create(output_dir_1L_chr)
    }
    pt_ls <- make_pt_ls(slots_chr = slots_chr, type_chr = type_chr, 
        vals_ls = vals_ls, prototype_lup = prototype_lup)
    class_nm_1L_chr <- paste0(name_pfx_1L_chr, name_stub_1L_chr)
    output_file_class_1L_chr <- get_class_fl_nms(class_names_chr = class_nm_1L_chr, 
        s3_1L_lgl = F, output_dir_1L_chr = output_dir_1L_chr)
    parent_ns_ls <- get_parent_cls_ns(prototype_lup = prototype_lup, 
        parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr = nss_to_ignore_chr[1])
    write_to_mk_r4_cls(class_nm_1L_chr = class_nm_1L_chr, slots_chr = slots_chr, 
        type_chr = type_chr, pt_ls = pt_ls, parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
        print_set_cls_1L_lgl = print_set_cls_1L_lgl, class_desc_1L_chr = class_desc_1L_chr, 
        output_file_class_1L_chr = output_file_class_1L_chr, 
        clss_to_inc_chr = clss_to_inc_chr, prototype_lup = prototype_lup, 
        helper_1L_lgl = F, parent_ns_ls = parent_ns_ls, abbreviations_lup = abbreviations_lup, 
        consent_1L_chr = consent_1L_chr, object_type_lup = object_type_lup)
    helper_function <- make_helper_fn(class_nm_1L_chr = class_nm_1L_chr, 
        parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, slots_chr = slots_chr, 
        pt_ls = pt_ls, prototype_lup = prototype_lup, parent_ns_ls = parent_ns_ls)
    eval(parse(text = helper_function))
    if (helper_1L_lgl) {
        if (is.null(consent_1L_chr)) {
            consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                output_file_class_1L_chr, " ?"), options_chr = c("Y", 
                "N"), force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr == "Y") {
            sink(output_file_class_1L_chr, append = TRUE)
            ready4fun::make_lines_for_fn_dmt(fn_name_1L_chr = class_nm_1L_chr, 
                fn_type_1L_chr = "set_class", fn_types_lup = fn_types_lup, 
                fn = eval(parse(text = class_nm_1L_chr)), class_name_1L_chr = class_nm_1L_chr, 
                object_type_lup = object_type_lup)
            writeLines(helper_function)
            ready4fun::close_open_sinks()
        }
    }
    if (accessors_1L_lgl) {
        accessors <- make_alg_to_write_gtr_str_mthds(class_nm_1L_chr = class_nm_1L_chr, 
            parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, print_gtrs_strs_1L_lgl = print_gtrs_strs_1L_lgl, 
            output_dir_1L_chr = output_dir_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr, 
            req_pkgs_chr = req_pkgs_chr, parent_ns_ls = parent_ns_ls)
        eval(parse(text = accessors %>% replace_NA_in_fn()))
    }
    valid_txt <- make_alg_to_set_validity_of_r4_cls(class_nm_1L_chr = class_nm_1L_chr, 
        parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, slots_of_dif_lnts_chr = slots_of_dif_lnts_chr, 
        allowed_vals_ls = allowed_vals_ls, names_must_match_ls = names_must_match_ls, 
        asserts_ls = asserts_ls)
    if (print_validator_1L_lgl) {
        if (is.null(consent_1L_chr)) {
            consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                output_file_class_1L_chr, " ?"), options_chr = c("Y", 
                "N"), force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr == "Y") {
            sink(output_file_class_1L_chr, append = TRUE)
            writeLines(paste0("\n", valid_txt %>% stringr::str_replace(paste0(",\nwhere =  ", 
                "globalenv\\(\\)"), "") %>% stringr::str_replace(",\".GlobalEnv\"", 
                "")))
            ready4fun::close_open_sinks()
        }
    }
    eval(parse(text = valid_txt))
    if (!is.null(meaningful_nms_ls)) {
        meaningful_txt <- make_show_mthd_fn(class_nm_1L_chr = class_nm_1L_chr, 
            meaningful_nms_ls = meaningful_nms_ls)
        eval(parse(text = meaningful_txt))
        if (print_meaningful_nms_ls_1L_lgl) {
            if (is.null(consent_1L_chr)) {
                consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                  output_file_class_1L_chr, " ?"), options_chr = c("Y", 
                  "N"), force_from_opts_1L_chr = T)
            }
            if (consent_1L_chr == "Y") {
                sink(output_file_class_1L_chr, append = TRUE)
                writeLines(paste0("\n", meaningful_txt %>% stringr::str_replace(paste0(",\nwhere =  ", 
                  "globalenv\\(\\)"), "") %>% stringr::str_replace_all("\\\\n\\\",", 
                  "\\\\n\\\",\n") %>% stringr::str_replace("\\nsep", 
                  "sep")))
                ready4fun::close_open_sinks()
            }
        }
    }
    devtools::document()
    devtools::load_all()
}
#' Write self service classes
#' @description write_self_srvc_clss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write self service classes. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @return Package setup (a list)
#' @rdname write_self_srvc_clss
#' @export 
#' @importFrom dplyr bind_rows filter
#' @keywords internal
write_self_srvc_clss <- function (pkg_setup_ls) 
{
    second_step_classes_tb <- pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x[-1, 
        ]
    classes_to_make_tb <- pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x[1, 
        ]
    write_scripts_to_mk_r3_cls(name_stub_1L_chr = classes_to_make_tb$name_stub_chr, 
        name_pfx_1L_chr = paste0(pkg_desc_ls$Package, "_"), output_dir_1L_chr = "R", 
        class_desc_1L_chr = classes_to_make_tb$class_desc_chr, 
        type_1L_chr = classes_to_make_tb$pt_ls[[1]][[1]], pt_chkr_pfx_1L_chr = classes_to_make_tb$pt_chkr_pfx_ls[[1]][[1]], 
        pt_ns_1L_chr = ifelse(classes_to_make_tb$pt_ns_ls[[1]][[1]] %in% 
            c("base"), "", classes_to_make_tb$pt_ns_ls[[1]][[1]]), 
        vals_ls = classes_to_make_tb$vals_ls[[1]], allowed_vals_ls = classes_to_make_tb$allowed_vals_ls[[1]], 
        min_max_vals_dbl = classes_to_make_tb$min_max_vals_ls[[1]][[1]], 
        start_end_vals_dbl = classes_to_make_tb$start_end_vals_ls[[1]][[1]], 
        prototype_lup = pkg_setup_ls$subsequent_ls$prototype_lup, 
        file_exists_cdn_1L_chr = "overwrite", abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
        asserts_ls = classes_to_make_tb$asserts_ls[[1]], fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
        object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
    pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x <- classes_to_make_tb %>% 
        ready4class_constructor() %>% dplyr::bind_rows(second_step_classes_tb)
    authorClasses.ready4class_constructor(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x %>% 
        dplyr::filter(name_stub_chr == "pt_lup"), name_pfx_1L_chr = paste0(pkg_desc_ls$Package, 
        "_"), output_dir_1L_chr = "R", prototype_lup = pkg_setup_ls$subsequent_ls$prototype_lup, 
        file_exists_cdn_1L_chr = "overwrite", abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
        fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
        object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
    if (!"ready4class_pt_lup" %in% class(pkg_setup_ls$subsequent_ls$prototype_lup)) 
        pkg_setup_ls$subsequent_ls$prototype_lup <- pkg_setup_ls$subsequent_ls$prototype_lup %>% 
            ready4class_pt_lup()
    return(pkg_setup_ls)
}
#' Write slot getter setter methods
#' @description write_slot_gtr_str_mthds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write slot getter setter methods. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param slot_nm_1L_chr Slot name (a character vector of length one)
#' @param set_only_1L_lgl Set only (a logical vector of length one)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param print_gtrs_strs_1L_lgl Print getters setters (a logical vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param nss_to_ignore_chr Namespaces to ignore (a character vector)
#' @param req_pkgs_chr Require packages (a character vector)
#' @param fn_types_lup Function types (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @return NULL
#' @rdname write_slot_gtr_str_mthds
#' @export 
#' @keywords internal
write_slot_gtr_str_mthds <- function (slot_nm_1L_chr, set_only_1L_lgl, parent_cls_nm_1L_chr, 
    class_nm_1L_chr, print_gtrs_strs_1L_lgl, output_dir_1L_chr, 
    nss_to_ignore_chr, req_pkgs_chr, fn_types_lup, object_type_lup) 
{
    curr_gnrcs_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(req_pkgs_chr = req_pkgs_chr, 
        generic_1L_chr = slot_nm_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr)
    pkgs_to_imp_ls <- make_ls_of_pkgs_to_imp(curr_gnrcs_ls = curr_gnrcs_ls, 
        fn_name_1L_chr = slot_nm_1L_chr, nss_to_ignore_chr = nss_to_ignore_chr)
    write_gtr_str_mthds_for_r4(slot_nm_1L_chr = slot_nm_1L_chr, 
        set_only_1L_lgl = set_only_1L_lgl, pkgs_to_imp_ls = pkgs_to_imp_ls, 
        class_nm_1L_chr = class_nm_1L_chr, print_gtrs_strs_1L_lgl = print_gtrs_strs_1L_lgl, 
        output_dir_1L_chr = output_dir_1L_chr, fn_types_lup = fn_types_lup, 
        object_type_lup = object_type_lup)
}
#' Write standard method
#' @description write_std_mthd() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write standard method. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fn Function (a function)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param fn_desc_chr Function description (a character vector)
#' @param fn_title_1L_chr Function title (a character vector of length one)
#' @param fn_outp_type_1L_chr Function output type (a character vector of length one)
#' @param fn_types_lup Function types (a lookup table)
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @param signature_1L_chr Signature (a character vector of length one), Default: 'NA'
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param first_1L_lgl First (a logical vector of length one), Default: T
#' @param import_from_chr Import from (a character vector), Default: 'NA'
#' @return NULL
#' @rdname write_std_mthd
#' @export 
#' @importFrom testit assert
#' @importFrom utils installed.packages
#' @importFrom purrr discard
#' @keywords internal
write_std_mthd <- function (fn, fn_name_1L_chr, class_nm_1L_chr, fn_desc_chr, fn_title_1L_chr, 
    fn_outp_type_1L_chr, fn_types_lup, pkg_nm_1L_chr, output_dir_1L_chr, 
    signature_1L_chr = NA_character_, append_1L_lgl = T, first_1L_lgl = T, 
    import_from_chr = NA_character_) 
{
    s3_1L_lgl = !isS4(eval(parse(text = paste0(class_nm_1L_chr, 
        "()"))))
    testit::assert("x" %in% formalArgs(fn))
    fn_type_chr <- paste0(c("gen_", "meth_"), "std_", ifelse(s3_1L_lgl, 
        "s3", "s4"), "_mthd")
    write_file_ls <- list(new_file_lgl = F, gnr_file = paste0(output_dir_1L_chr, 
        "/gnrc_", fn_name_1L_chr, ".R"), meth_file = paste0(output_dir_1L_chr, 
        "/meth_", fn_name_1L_chr, ".R"))
    gnrc_exists_1L_lgl <- ifelse(!all(is.na(import_from_chr)), 
        fn_name_1L_chr %in% names(import_from_chr), F)
    if (!gnrc_exists_1L_lgl) {
        curr_gnrcs_ls <- make_ls_of_tfd_nms_of_curr_gnrcs(req_pkgs_chr = NA_character_, 
            generic_1L_chr = fn_name_1L_chr, nss_to_ignore_chr = ifelse(pkg_nm_1L_chr %in% 
                rownames(utils::installed.packages()), pkg_nm_1L_chr, 
                NA_character_))
        pkgs_to_imp_ls <- make_ls_of_pkgs_to_imp(curr_gnrcs_ls = curr_gnrcs_ls, 
            fn_name_1L_chr = fn_name_1L_chr, nss_to_ignore_chr = ifelse(pkg_nm_1L_chr %in% 
                rownames(utils::installed.packages()), pkg_nm_1L_chr, 
                NA_character_))
        gnrc_exists_1L_lgl <- pkgs_to_imp_ls$gnrc_gtr_exists_1L_lgl
        imports_chr <- pkgs_to_imp_ls$gtr_imps_chr[pkgs_to_imp_ls$gtr_imps_chr != 
            pkg_nm_1L_chr]
        if (identical(imports_chr, character(0))) 
            imports_chr <- NA_character_
    }
    else {
        imports_chr <- NA_character_
    }
    write_file_ls <- write_scripts_to_make_gnrc_and_mthd(fn_name_1L_chr = fn_name_1L_chr, 
        args_chr = c("x", ifelse(length(formalArgs(fn)) > 1, 
            "...", NA_character_)) %>% purrr::discard(is.na), 
        signature_1L_chr = signature_1L_chr, pkg_nm_1L_chr = NA_character_, 
        where_chr = "globalenv()", class_nm_1L_chr = class_nm_1L_chr, 
        fn = fn, fn_type_chr = fn_type_chr, fn_desc_chr = fn_desc_chr, 
        fn_title_1L_chr = fn_title_1L_chr, fn_types_lup = fn_types_lup, 
        fn_outp_type_1L_chr = fn_outp_type_1L_chr, imports_chr = imports_chr, 
        write_file_ls = write_file_ls, output_dir_1L_chr = output_dir_1L_chr, 
        append_1L_lgl = append_1L_lgl, doc_in_class_1L_lgl = F, 
        gnrc_exists_1L_lgl = gnrc_exists_1L_lgl, overwrite_1L_lgl = !append_1L_lgl, 
        s3_1L_lgl = s3_1L_lgl, write_1L_lgl = T, import_from_chr = import_from_chr)
    write_file_ls
}
#' Write to delete files with pattern
#' @description write_to_delete_fls_with_ptrn() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete files with pattern. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param dir_1L_chr Directory (a character vector of length one)
#' @param pattern_1L_chr Pattern (a character vector of length one)
#' @return NULL
#' @rdname write_to_delete_fls_with_ptrn
#' @export 
#' @keywords internal
write_to_delete_fls_with_ptrn <- function (dir_1L_chr, pattern_1L_chr) 
{
    if (!is.na(pattern_1L_chr)) {
        files_chr <- list.files(dir_1L_chr, pattern = pattern_1L_chr)
        if (!identical(files_chr, character(0))) 
            paste0(dir_1L_chr, "/", files_chr) %>% file.remove()
    }
}
#' Write to delete generic function files
#' @description write_to_delete_gnrc_fn_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete generic function files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param x An object
#' @param output_dir_1L_chr Output directory (a character vector of length one)
#' @return NULL
#' @rdname write_to_delete_gnrc_fn_fls
#' @export 
#' @importFrom dplyr pull
#' @importFrom purrr compact flatten flatten_chr reduce walk
#' @keywords internal
write_to_delete_gnrc_fn_fls <- function (x, output_dir_1L_chr) 
{
    delete_chr <- x %>% dplyr::pull(slots_ls) %>% purrr::compact() %>% 
        purrr::flatten() %>% purrr::flatten_chr()
    if (!identical(delete_chr, character(0))) 
        paste0(output_dir_1L_chr, "/gnrc_", purrr::reduce(delete_chr, 
            ~append(.x, .y[!.y %in% .x])), ".R") %>% purrr::walk(~if (file.exists(.x)) 
            file.remove(.x))
}
#' Write to make ready4 S4 class
#' @description write_to_mk_r4_cls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to make ready4 s4 class. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param class_nm_1L_chr Class name (a character vector of length one)
#' @param slots_chr Slots (a character vector)
#' @param type_chr Type (a character vector)
#' @param pt_ls Prototype (a list)
#' @param parent_cls_nm_1L_chr Parent class name (a character vector of length one)
#' @param print_set_cls_1L_lgl Print set class (a logical vector of length one)
#' @param class_desc_1L_chr Class description (a character vector of length one)
#' @param output_file_class_1L_chr Output file class (a character vector of length one)
#' @param clss_to_inc_chr Classes to include (a character vector)
#' @param prototype_lup Prototype (a lookup table)
#' @param helper_1L_lgl Helper (a logical vector of length one), Default: F
#' @param parent_ns_ls Parent namespace (a list)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname write_to_mk_r4_cls
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom stringr str_c str_replace_all str_replace
#' @importFrom ready4fun update_ns make_arg_desc close_open_sinks
#' @importFrom ready4 make_prompt
#' @keywords internal
write_to_mk_r4_cls <- function (class_nm_1L_chr, slots_chr, type_chr, pt_ls, parent_cls_nm_1L_chr, 
    print_set_cls_1L_lgl, class_desc_1L_chr, output_file_class_1L_chr, 
    clss_to_inc_chr, prototype_lup, helper_1L_lgl = F, parent_ns_ls, 
    abbreviations_lup = NULL, object_type_lup = NULL, consent_1L_chr = NULL) 
{
    slot_str <- purrr::map2_chr(slots_chr, type_chr, ~paste0(.x, 
        " = \"", .y, "\"")) %>% stringr::str_c(sep = "", collapse = ",") %>% 
        paste0("c(", ., ")")
    named_slots_chr <- eval(parse(text = slot_str))
    old_class_tb_extension <- make_alg_to_set_old_clss(type_chr = type_chr, 
        prototype_lup = prototype_lup)
    if (!identical(old_class_tb_extension, character(0))) {
        eval(parse(text = old_class_tb_extension))
    }
    else {
        old_class_tb_extension <- ""
    }
    prototype <- eval(parse(text = pt_ls))
    if (is.null(parent_cls_nm_1L_chr)) {
        st_class_fn <- paste0("methods::setClass(", make_alg_to_gen_ref_to_cls(class_nm_1L_chr), 
            ",\nslots = ", slot_str, ",\nprototype =  ", pt_ls, 
            ",\nwhere =  ", "globalenv()", ")")
    }
    else {
        parent_slots_chr <- get_parent_cls_slot_nms(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            parent_ns_ls = parent_ns_ls)
        parent_pt_chr <- get_parent_cls_pts(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
            parent_ns_ls = parent_ns_ls, slot_names_chr = parent_slots_chr)
        parent_pt_chr <- `names<-`(parent_pt_chr, parent_slots_chr)
        named_slots_chr <- c(named_slots_chr, parent_pt_chr)
        named_slots_chr <- named_slots_chr[!duplicated(names(named_slots_chr))]
        st_class_fn <- paste0("methods::setClass(", make_alg_to_gen_ref_to_cls(class_nm_1L_chr, 
            pkg_nm_1L_chr = transform_parent_ns_ls(parent_ns_ls) %>% 
                ready4fun::update_ns()), ",\ncontains = \"", 
            parent_cls_nm_1L_chr, "\",\nslots = ", purrr::map2_chr(names(named_slots_chr), 
                named_slots_chr, ~paste0(.x, " = \"", .y, "\"")) %>% 
                stringr::str_c(sep = "", collapse = ",") %>% 
                paste0("c(", ., ")"), ",\nprototype =  ", pt_ls, 
            ",\nwhere =  ", "globalenv()", ")")
    }
    slots_tags <- paste0("#' @slot ", names(named_slots_chr), 
        " ", names(named_slots_chr) %>% ready4fun::make_arg_desc(abbreviations_lup = abbreviations_lup, 
            object_type_lup = object_type_lup), "\n", collapse = "")
    clss_to_inc_chr <- get_nms_of_clss_to_inc(parent_cls_nm_1L_chr = parent_cls_nm_1L_chr, 
        parent_ns_ls = parent_ns_ls, base_set_of_clss_to_inc_chr = clss_to_inc_chr)
    include_tags_chr <- make_dmt_inc_tag(clss_to_inc_chr, s3_1L_lgl = F)
    if (print_set_cls_1L_lgl) {
        if (is.null(consent_1L_chr)) {
            consent_1L_chr <- ready4::make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                output_file_class_1L_chr, " ?"), options_chr = c("Y", 
                "N"), force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr == "Y") {
            sink(output_file_class_1L_chr)
            writeLines(paste0(paste0("#' ", class_nm_1L_chr, 
                "\n"), paste0("#' ", "\n"), paste0("#' ", class_desc_1L_chr, 
                "\n"), paste0("#' ", "\n"), include_tags_chr, 
                old_class_tb_extension %>% stringr::str_replace_all(paste0(",where =  ", 
                  "globalenv\\(\\)"), ""), ifelse(old_class_tb_extension == 
                  "", "", "\n"), slots_tags, ifelse(!ifelse(is.null(parent_ns_ls$transformed_1L_chr), 
                  F, ifelse(is.na(parent_ns_ls$transformed_1L_chr), 
                    F, parent_ns_ls$transformed_1L_chr != "")), 
                  "", paste0("#' @import ", parent_ns_ls$transformed_1L_chr, 
                    "\n")), paste0("#' @name ", class_nm_1L_chr, 
                  "-class\n"), paste0("#' @rdname ", class_nm_1L_chr, 
                  "-class\n"), paste0("#' @export ", class_nm_1L_chr, 
                  "\n"), ifelse(T, paste0("#' @exportClass ", 
                  class_nm_1L_chr, "\n"), ""), ifelse(helper_1L_lgl, 
                  "", paste0(class_nm_1L_chr, " <- ")), st_class_fn %>% 
                  stringr::str_replace(paste0(",\nwhere =  ", 
                    "globalenv\\(\\)"), "") %>% transform_alg_to_ref_cls_nm(pkg_nm_1L_chr = ifelse(is.null(parent_cls_nm_1L_chr), 
                  ".GlobalEnv", transform_parent_ns_ls(parent_ns_ls) %>% 
                    ready4fun::update_ns())), "\n"))
            ready4fun::close_open_sinks()
        }
    }
    eval(parse(text = st_class_fn))
}
