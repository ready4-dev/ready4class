% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_r4_mthds}
\alias{write_r4_mthds}
\title{Write ready4 module methods}
\usage{
write_r4_mthds(
  fns_dir_1L_chr = "data-raw/s4_fns",
  fn_types_lup = NULL,
  import_from_chr = character(0),
  output_dir_1L_chr = "R",
  pkg_nm_1L_chr = character(0)
)
}
\arguments{
\item{fns_dir_1L_chr}{Functions directory (a character vector of length one), Default: 'data-raw/s4_fns'}

\item{fn_types_lup}{Function types (a lookup table), Default: NULL}

\item{import_from_chr}{Import from (a character vector), Default: character(0)}

\item{output_dir_1L_chr}{Output directory (a character vector of length one), Default: 'R'}

\item{pkg_nm_1L_chr}{Package name (a character vector of length one), Default: character(0)}
}
\value{
a S4 methods (a list)
}
\description{
write_r4_mthds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write ready4 module methods. The function returns a S4 methods (a list).
}
\keyword{internal}
