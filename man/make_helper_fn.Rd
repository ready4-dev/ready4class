% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_helper_fn}
\alias{make_helper_fn}
\title{Make helper function}
\usage{
make_helper_fn(
  class_nm_1L_chr,
  parent_cls_nm_1L_chr,
  slots_chr,
  pt_ls,
  prototype_lup,
  parent_ns_ls,
  vals_ls = NULL
)
}
\arguments{
\item{class_nm_1L_chr}{Class name (a character vector of length one)}

\item{parent_cls_nm_1L_chr}{Parent class name (a character vector of length one)}

\item{slots_chr}{Slots (a character vector)}

\item{pt_ls}{Prototype (a list)}

\item{prototype_lup}{Prototype (a lookup table)}

\item{parent_ns_ls}{Parent namespace (a list)}

\item{vals_ls}{Values (a list), Default: NULL}
}
\value{
Helper function (a character vector of length one)
}
\description{
make_helper_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make helper function. The function returns Helper function (a character vector of length one).
}
\keyword{internal}
