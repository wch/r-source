\name{tcltk-defunct}
%------ NOTE:  ../R/tcltk-defunct.R   must be synchronized with this!
\alias{tcltk-defunct}
\alias{$.tclvar}
\alias{$<-.tclvar}
\alias{tkcmd}
\alias{tkfile.tail}
\alias{tkfile.dir}
\alias{tkopen}
\alias{tkclose}
\alias{tkputs}        
\alias{tkread}        

\title{Defunct Functions in Package tcltk}
\description{
  The functions or variables listed here are no longer part of \R as
  they are not needed (any more).
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
  \code{\link{Defunct}}
}
\keyword{internal}
