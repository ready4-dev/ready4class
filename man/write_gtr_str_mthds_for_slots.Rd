% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_gtr_str_mthds_for_slots}
\alias{write_gtr_str_mthds_for_slots}
\title{Write getter setter methods for slots}
\usage{
write_gtr_str_mthds_for_slots(
  slot_names_chr,
  set_only_chr,
  parent_cls_nm_1L_chr,
  class_nm_1L_chr,
  print_gtrs_strs_1L_lgl,
  output_dir_1L_chr,
  nss_to_ignore_chr,
  req_pkgs_chr,
  fn_types_lup,
  object_type_lup
)
}
\arguments{
\item{slot_names_chr}{Slot names (a character vector)}

\item{set_only_chr}{Set only (a character vector)}

\item{parent_cls_nm_1L_chr}{Parent class name (a character vector of length one)}

\item{class_nm_1L_chr}{Class name (a character vector of length one)}

\item{print_gtrs_strs_1L_lgl}{Print getters setters (a logical vector of length one)}

\item{output_dir_1L_chr}{Output directory (a character vector of length one)}

\item{nss_to_ignore_chr}{Namespaces to ignore (a character vector)}

\item{req_pkgs_chr}{Require packages (a character vector)}

\item{fn_types_lup}{Function types (a lookup table)}

\item{object_type_lup}{Object type (a lookup table)}
}
\value{
No return value, called for side effects.
}
\description{
write_gtr_str_mthds_for_slots() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write getter setter methods for slots. The function is called for its side effects and does not return a value.
}
\keyword{internal}
