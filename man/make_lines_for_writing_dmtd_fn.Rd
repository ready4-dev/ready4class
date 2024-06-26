% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_lines_for_writing_dmtd_fn}
\alias{make_lines_for_writing_dmtd_fn}
\title{Make lines for writing documented function}
\usage{
make_lines_for_writing_dmtd_fn(
  fn_name_1L_chr,
  fn_body_1L_chr,
  fn_type_1L_chr,
  class_nm_1L_chr,
  class_desc_1L_chr,
  abbreviations_lup,
  fn_types_lup,
  object_type_lup
)
}
\arguments{
\item{fn_name_1L_chr}{Function name (a character vector of length one)}

\item{fn_body_1L_chr}{Function body (a character vector of length one)}

\item{fn_type_1L_chr}{Function type (a character vector of length one)}

\item{class_nm_1L_chr}{Class name (a character vector of length one)}

\item{class_desc_1L_chr}{Class description (a character vector of length one)}

\item{abbreviations_lup}{Abbreviations (a lookup table)}

\item{fn_types_lup}{Function types (a lookup table)}

\item{object_type_lup}{Object type (a lookup table)}
}
\value{
No return value, called for side effects.
}
\description{
make_lines_for_writing_dmtd_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make lines for writing documented function. The function is called for its side effects and does not return a value.
}
\keyword{internal}
