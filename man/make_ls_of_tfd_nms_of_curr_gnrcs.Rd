% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_ls_of_tfd_nms_of_curr_gnrcs}
\alias{make_ls_of_tfd_nms_of_curr_gnrcs}
\title{Make list of transformed names of current generics}
\usage{
make_ls_of_tfd_nms_of_curr_gnrcs(
  req_pkgs_chr,
  generic_1L_chr,
  nss_to_ignore_chr
)
}
\arguments{
\item{req_pkgs_chr}{Require packages (a character vector)}

\item{generic_1L_chr}{Generic (a character vector of length one)}

\item{nss_to_ignore_chr}{Namespaces to ignore (a character vector)}
}
\value{
Current generics (a list)
}
\description{
make_ls_of_tfd_nms_of_curr_gnrcs() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list of transformed names of current generics. The function returns Current generics (a list).
}
\keyword{internal}
