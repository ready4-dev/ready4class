% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grp_generics.R,
%   R/mthd_write_classes_and_make_lup.R
\name{write_classes_and_make_lup}
\alias{write_classes_and_make_lup}
\alias{write_classes_and_make_lup.ready4_constructor_tbl}
\alias{write_classes_and_make_lup,ready4_constructor_tbl-method}
\title{Write classes and make lookup table}
\usage{
write_classes_and_make_lup(x, ...)

write_classes_and_make_lup.ready4_constructor_tbl(
  x,
  dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
  name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"),
  output_dir_1L_chr = "R",
  delete_cdn_ptrn_chr = NA_character_,
  file_exists_cdn_1L_chr = "overwrite",
  init_class_pt_lup = NULL,
  nss_to_ignore_chr = NA_character_,
  req_pkgs_chr = NA_character_,
  class_in_cache_cdn_1L_chr = "stop",
  abbreviations_lup = NULL,
  object_type_lup = NULL
)

\S4method{write_classes_and_make_lup}{ready4_constructor_tbl}(
  x,
  dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
  name_pfx_1L_chr = paste0(ready4fun::get_dev_pkg_nm(), "_"),
  output_dir_1L_chr = "R",
  delete_cdn_ptrn_chr = NA_character_,
  file_exists_cdn_1L_chr = "overwrite",
  init_class_pt_lup = NULL,
  nss_to_ignore_chr = NA_character_,
  req_pkgs_chr = NA_character_,
  class_in_cache_cdn_1L_chr = "stop",
  abbreviations_lup = NULL,
  object_type_lup = NULL
)
}
\arguments{
\item{x}{An instance of ready4 S3 class CLASS CONSTRUCTOR TABLE}

\item{...}{Additional arguments}

\item{dev_pkg_ns_1L_chr}{Development package namespace (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()}

\item{name_pfx_1L_chr}{Name prefix (a character vector of length one), Default: paste0(ready4fun::get_dev_pkg_nm(), "_")}

\item{output_dir_1L_chr}{Output directory (a character vector of length one), Default: 'R'}

\item{delete_cdn_ptrn_chr}{Delete condition pattern (a character vector), Default: 'NA'}

\item{file_exists_cdn_1L_chr}{File exists condition (a character vector of length one), Default: 'overwrite'}

\item{init_class_pt_lup}{Init class prototype (a lookup table), Default: NULL}

\item{nss_to_ignore_chr}{Namespaces to ignore (a character vector), Default: 'NA'}

\item{req_pkgs_chr}{Req packages (a character vector), Default: 'NA'}

\item{class_in_cache_cdn_1L_chr}{Class in cache condition (a character vector of length one), Default: 'stop'}

\item{abbreviations_lup}{Abbreviations (a lookup table), Default: NULL}

\item{object_type_lup}{Object type (a lookup table), Default: NULL}
}
\value{
Instance (a ready4 S3 class CLASS PROTOTYPE LOOKUP TABLE)
}
\description{
write_classes_and_make_lup() is a Write Classes and Make Lookup Table generic that makes new classes and creates or updates a class prototype lookup table.

write_classes_and_make_lup.ready4_constructor_tbl() is a Write Classes and Make Lookup Table method that makes new classes and creates or updates a class prototype lookup table. This method is implemented for the ready4 S3 class CLASS CONSTRUCTOR TABLE. The function returns Instance (a ready4 S3 class CLASS PROTOTYPE LOOKUP TABLE).
}
