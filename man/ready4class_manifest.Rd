% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/C3_ready4class_manifest.R
\name{ready4class_manifest}
\alias{ready4class_manifest}
\alias{make_pt_ready4class_manifest}
\alias{is_ready4class_manifest}
\title{Class creation manifest}
\usage{
ready4class_manifest(x = make_pt_ready4class_manifest())

make_pt_ready4class_manifest(
  x_ready4fun_manifest = ready4fun::ready4fun_manifest(),
  constructor_r3 = ready4class_constructor()
)

is_ready4class_manifest(x)
}
\arguments{
\item{x}{An object of any type}

\item{x_ready4fun_manifest}{PARAM_DESCRIPTION, Default: ready4fun::ready4fun_manifest()}

\item{constructor_r3}{Constructor (a ready4 submodule), Default: ready4class_constructor()}
}
\value{
A validated instance of the Class creation manifest

A prototype for Class creation manifest

A logical value, TRUE if a valid instance of the Class creation manifest
}
\description{
Create a new valid instance of the Class creation manifest
}
\details{
Class creation manifest
}
