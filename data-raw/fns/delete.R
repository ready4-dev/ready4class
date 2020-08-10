delete_getters_setters <- function(x,
                                   output_dir){ ## NEEDS TO BE TESTED AND COMPARED TO DELETE_FILES FUNCITON
  delete_vec <- x %>%
    dplyr::pull(class_slots) %>%
    purrr::compact() %>%
    purrr::flatten() %>%
    purrr::flatten_chr()
  if(!identical(delete_vec,character(0)))
    paste0(output_dir,"/gnrc_",
           purrr::reduce(delete_vec ,
                         ~ append(.x,.y[!.y %in% .x])),
           ".R") %>%
    purrr::walk(~ if(file.exists(.x))
      file.remove(.x))
}

delete_files <- function(dir_chr,
                         pattern_chr){
  if(!is.na(pattern_chr)){
    files_chr_vec <- list.files(dir_chr, pattern = pattern_chr)
    if(!identical(files_chr_vec, character(0)))
      paste0(dir_chr,"/",files_chr_vec) %>% file.remove()
  }
}
