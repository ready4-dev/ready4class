% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_script_to_make_mthd}
\alias{write_script_to_make_mthd}
\title{Write script to make method}
\usage{
write_script_to_make_mthd(
  write_file_ls,
  gen_mthd_pair_ls,
  class_nm_1L_chr,
  fn_name_1L_chr,
  fn_type_1L_chr,
  fn_desc_1L_chr = NA_character_,
  fn_outp_type_1L_chr = NA_character_,
  imports_chr,
  write_1L_lgl = T,
  append_1L_lgl = T,
  doc_in_class_1L_lgl = F,
  fn_types_lup = NULL,
  object_type_lup = NULL,
  consent_1L_chr = NULL,
  import_from_chr = NA_character_,
  s3_1L_lgl = F
)
}
\arguments{
\item{write_file_ls}{Write file (a list)}

\item{gen_mthd_pair_ls}{Generate method pair (a list)}

\item{class_nm_1L_chr}{Class name (a character vector of length one)}

\item{fn_name_1L_chr}{Function name (a character vector of length one)}

\item{fn_type_1L_chr}{Function type (a character vector of length one)}

\item{fn_desc_1L_chr}{Function description (a character vector of length one), Default: 'NA'}

\item{fn_outp_type_1L_chr}{Function output type (a character vector of length one), Default: 'NA'}

\item{imports_chr}{Imports (a character vector)}

\item{write_1L_lgl}{Write (a logical vector of length one), Default: T}

\item{append_1L_lgl}{Append (a logical vector of length one), Default: T}

\item{doc_in_class_1L_lgl}{Document in class (a logical vector of length one), Default: F}

\item{fn_types_lup}{Function types (a lookup table), Default: NULL}

\item{object_type_lup}{Object type (a lookup table), Default: NULL}

\item{consent_1L_chr}{Consent (a character vector of length one), Default: NULL}

\item{import_from_chr}{Import from (a character vector), Default: 'NA'}

\item{s3_1L_lgl}{S3 (a logical vector of length one), Default: F}
}
\value{
No return value, called for side effects.
}
\description{
write_script_to_make_mthd() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write script to make method. The function is called for its side effects and does not return a value.
}
\keyword{internal}
