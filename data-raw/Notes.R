# manifest_r3$subsequent_ls$prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup") # Add to pkg_set_up logic (inc validation)
# manifest_r3 <- ready4fun::validate_pkg_setup(manifest_r3)
# manifest_r3 <- renew(manifest_r3,
#                                         type_1L_chr = "words",
#                                         are_words_chr = c("accessors"))
# manifest_r3 <- write_new_abbrs(manifest_r3,
#                                 long_name_chr = c("initial","ready4class R package","require","unvalidated"),
#                                                    custom_plural_ls = NULL,
#                                                    no_plural_chr = c("ready4class package","unvalidated"))
# One off tidy-up of abbreviations lup and prototype_lup
# abbreviations_lup <- get_rds_from_dv("abbreviations_lup",
#                                     dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9")
# abbreviations_lup <- abbreviations_lup %>%
#   dplyr::filter(short_name_chr %>% purrr::map_lgl(~!(startsWith(.x,"ready4_")|(.x %in% c("unvd", "..._ls", "..._r4",
#                                                                                          "ready4class_class_pt_lup")))))
# write_env_objs_to_dv(list(abbreviations_lup = abbreviations_lup),
#                      descriptions_chr = "Abbreviations lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
# prototype_lup <- get_rds_from_dv("prototype_lup")
# prototype_lup <- prototype_lup %>%
#   dplyr::filter(type_chr %>% purrr::map_lgl(~!startsWith(.x,"ready4_")))
# class(prototype_lup) <- class(prototype_lup)[3:5]
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
# Prior to running this script, the gh-pages orphan branch should be set up using instructions at:
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
# 15. Manual step: Push changes
## NOTE TO SELF: Need to implement variant of local git step outlined here: https://pkgdown.r-lib.org/reference/deploy_site_github.html
