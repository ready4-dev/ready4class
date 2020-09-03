% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_nms_of_clss_to_inc}
\alias{get_nms_of_clss_to_inc}
\title{Get names of classes to inc}
\usage{
get_nms_of_clss_to_inc(
  parent_chr,
  parent_ns_ls,
  prespecified_includes_chr = NULL
)
}
\arguments{
\item{parent_chr}{Parent (a character vector)}

\item{parent_ns_ls}{Parent namespace (a list)}

\item{prespecified_includes_chr}{Prespecified includes (a character vector), Default: NULL}
}
\description{
get_nms_of_clss_to_inc() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get names of clss to inc. Function argument parent_chr specifies the where to look for the required object.The function is called for its side effects and does not return a value.
}