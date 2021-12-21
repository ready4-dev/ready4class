a <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
b <- a %>%
  ingest(fls_to_ingest_chr = "abbreviations_lup",
         metadata_1L_lgl = F) %>%
  dplyr::filter(!startsWith(short_name_chr,
                                    "ready4class")) %>%
  dplyr::filter(short_name_chr != "Ready4classTestClass")
a <- share(a,
           obj_to_share_xx = b,
           fl_nm_1L_chr = "abbreviations_lup")


# x_ready4fun_manifest$subsequent_ls$prototype_lup <- ready4::get_rds_from_dv("prototype_lup") # Add to pkg_set_up logic (inc validation)
# x_ready4fun_manifest <- ready4fun::validate_pkg_setup(x_ready4fun_manifest)
# x_ready4fun_manifest <- renew(x_ready4fun_manifest,
#                                         type_1L_chr = "words",
#                                         are_words_chr = c("accessors"))
# x_ready4fun_manifest <- write_new_abbrs(x_ready4fun_manifest,
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
# prototype_lup <- prototype_lup %>%
#   dplyr::arrange(pt_ns_chr)
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
# Prior to running this script, the gh-pages orphan branch should be set up using instructions at:
# https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
# 15. Manual step: Push changes
## NOTE TO SELF: Need to implement variant of local git step outlined here: https://pkgdown.r-lib.org/reference/deploy_site_github.html
# pkg_clss_chr <- x_ready4fun_manifest$subsequent_ls$prototype_lup %>% dplyr::filter(pt_ns_chr == "ready4use") %>% dplyr::pull(type_chr)
# prototype_lup <- x_ready4fun_manifest$subsequent_ls$prototype_lup %>%
#   dplyr::filter(!type_chr %in% pkg_clss_chr[1:8])
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                      publish_dv_1L_lgl = T)
