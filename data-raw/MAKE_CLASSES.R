## This script creates the initial S3 class exported with this package.
##
## 1. Pre-requisites
## The following script to make the initial class was run once the following items were already in the package's data-raw/mthds/ folder:
## fn_write_scripts_to_mk_r3_clss.R
## fn_write_scripts_to_mk_r4_clss.R
## import_methods.R
## import_pipe.R
## mthd_make_classes.R
## pckg_ready4class.R
##
## 2. Document and load all package functions.
# devtools::document()
# devtools::load_all()
#ready4fun::read_fns("data-raw/mthds/")
##
## 3. Specify details about this package and the prefix we will use on all classes we create in this package.
name_prefix <- "ready4_"
dev_pckg_namespace <- "ready4class"
## 2. Create a table with metadata about frequently used class prototypes
prototype_lup <- tibble::tibble(type = c("character", "list", "logical", "numeric", "POSIXt", "sf","tbl_df"), ## CHANGED FROM POSIXt
                                value = c("NA_character_", "list(list())","NA","NA_real_",".POSIXct(NA_character_)", "st_sf(sf::st_sfc())","tibble::tibble()"),
                                type_namespace = c("base", "base", "base", "base", "base", "sf", "tibble"),
                                function_to_call = c("", "list", "", "", ".POSIXct", "st_sf", "tibble"),
                                default_value = c("NA_character_", "list()", "NA", "NA_real_", "NA_character_", "sf::st_sfc()", ""),
                                old_class = FALSE)
## 3. Create a table with the metadata for the ready4_class_make_tb class that we wish to create.
classes_to_make_tb <- tibble::tribble(
  ~ make_s3, ~ name_stub, ~ prototype, ~ prototype_checker_prefix, ~ prototype_namespace, ~ values, ~ allowed_values, ~ min_max_values, ~ start_end_values, ~ class_desc, ~ parent_class, ~ class_slots, ~ meaningful_names, ~include_classes,
  TRUE, "class_make_tb", list("tibble"), list("is_"), list("tibble"), list(make_s3 = "logical(0)",
                                                                           name_stub = "character(0)",
                                                                           prototype = "list()",
                                                                           prototype_checker_prefix = "list()",
                                                                           prototype_namespace = "list()",
                                                                           values = "list()",
                                                                           allowed_values = "list()",
                                                                           min_max_values = "list()",
                                                                           start_end_values = "list()",
                                                                           class_desc = "character(0)",
                                                                           parent_class = "character(0)",
                                                                           class_slots = "list()",
                                                                           meaningful_names = "list()",
                                                                           include_classes = "list()"), NULL, NULL, NULL, "MAKE CLASS TABLE Readyforwhatsnext S3 class of metadata required to make new classes.", NA_character_, NULL, NULL, NULL)
## 4. Convert the metadata table into a new S3 class called ready4_class_make_tb
write_scripts_to_mk_r3_clss(name_stub = classes_to_make_tb$name_stub,
              name_prefix = name_prefix,
              output_folder = "R",
              class_desc = classes_to_make_tb$class_desc,
              type = classes_to_make_tb$prototype[[1]],
              type_checker_prefix = classes_to_make_tb$prototype_checker_prefix[[1]],
              type_namespace = ifelse(classes_to_make_tb$prototype_namespace[[1]] %in% c("base"),
                                      "",
                                      classes_to_make_tb$prototype_namespace[[1]]),
              values = classes_to_make_tb$values[[1]],
              allowed_values = classes_to_make_tb$allowed_values[[1]],
              min_max_values = classes_to_make_tb$min_max_values[[1]],
              start_end_values = classes_to_make_tb$start_end_values[[1]],
              prototype_lup = prototype_lup,
              file_exists_logic = "overwrite")
## 5. Convert the classes_to_make_tb tibble we created  into an instance of the ready4_class_make_tb class also created in that step.
classes_to_make_tb <- classes_to_make_tb %>%
  ready4_class_make_tb()
## 6. Expand the ready4_class_make_tb instance to include information to make a new class of the class prototype lookup.
classes_to_make_tb <- classes_to_make_tb %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3, ~ name_stub, ~ prototype, ~ prototype_checker_prefix, ~ prototype_namespace, ~ values, ~ allowed_values, ~ min_max_values, ~ start_end_values, ~ class_desc, ~ parent_class, ~ class_slots, ~ meaningful_names, ~include_classes,
    TRUE, "class_pt_lup", list("tibble"), list("is_"), list("tibble"), list(type = "character(0)",
                                                                            value = "character(0)",
                                                                            type_namespace = "character(0)",
                                                                            function_to_call = "character(0)",
                                                                            default_value = "character(0)",
                                                                            old_class = "logical(0)"), NULL, NULL, NULL, "PROTOTYPE LOOKUP Readyforwhatsnext S3 class of metadata of prototype classes.", NA_character_, NULL, NULL, NULL))
## 7. Call the make_classes method that we have defined in our R directory to create the scripts that will create scripts to make the classes in this table and save these scripts to the package R directory.
##    Note: In addition to creating a new class (ready4_class_pt_lup) the call to the below method recreates the script to make the ready4_class_make_tb class. That duplication is of no importance in this step,
##    but having all the classes we have made summarised in one table (classes_to_make_tb) is necessary for a subsequent step (see DATASET.R in the data-raw directory.)
make_classes(classes_to_make_tb %>% dplyr::filter(name_stub == "class_pt_lup"),
             name_prefix = name_prefix,
             output_dir = "R",
             prototype_lup = prototype_lup,
             file_exists_logic = "overwrite")


