\name{tcltk-deprecated}
%------ NOTE:  ../R/tcltk-deprecated.R   must be synchronized with this!
\alias{tcltk-deprecated}
\alias{$.tclvar}
\alias{$<-.tclvar}
\alias{tkcmd}
\alias{tkfile.tail}
\alias{tkfile.dir}
\alias{tkopen}
\alias{tkclose}
\alias{tkputs}        
\alias{tkread}        

\title{Deprecated Functions in Package tcltk}
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
tkcmd(...)

tkfile.tail(...)
tkfile.dir(...)
tkopen(...)
tkclose(...)
tkputs(...)
tkread(...)
}
\arguments{
  \item{\dots}{Handled via \code{.Tcl.args}}
}
\details{
  As from \R 2.0.0, \code{tkcmd} has been renamed to \code{tcl}.  The
  file commands used to be \code{tkopen}, \code{tkclose}, etc., but
  since \code{open} and \code{close} do not belong to the Tk toolkit,
  the preferred names are now \code{tclopen}, etc.

  The \code{$} methods for class \code{"tclvar"} have been deprecated in
  favour of \code{\link{tclVar}} and \code{\link{tclvalue}}.
}
\seealso{
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{misc}
