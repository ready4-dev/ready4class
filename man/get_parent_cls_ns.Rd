% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_parent_cls_ns}
\alias{get_parent_cls_ns}
\title{Get parent class namespace}
\usage{
get_parent_cls_ns(prototype_lup, parent_cls_nm_1L_chr, dev_pkg_ns_1L_chr)
}
\arguments{
\item{prototype_lup}{Prototype (a lookup table)}

\item{parent_cls_nm_1L_chr}{Parent class name (a character vector of length one)}

\item{dev_pkg_ns_1L_chr}{Development package namespace (a character vector of length one)}
}
\value{
Parent class (a namespace)
}
\description{
get_parent_cls_ns() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get parent class namespace. Function argument prototype_lup specifies the where to look for the required object. The function is called for its side effects and does not return a value.
}
\keyword{internal}
