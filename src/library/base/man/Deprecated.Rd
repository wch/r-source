\name{Deprecated}
%------ NOTE:  ../R/Deprecated.R   must be synchronized with this!
\title{Deprecated Functions}
\alias{Deprecated}
\alias{.Deprecated}
\alias{printNoClass}
\alias{tkfilefind}
%------ PLEASE: put \alias{.} here for EACH !
\description{
  These functions are provided for compatibility with older versions of
  \R only, and may be defunct as soon as of the next release.
}
\usage{
.Deprecated(new)
printNoClass(x, digits = NULL, quote = TRUE, na.print = NULL,
             print.gap = NULL, right = FALSE, \dots)
}
\details{
  \code{.Deprecated("<new name>")} is called from deprecated
  functions. The original help page for these functions is often
  available at \code{help("oldName-deprecated")} (note the quotes).

  \code{printNoClass} was in package \code{methods} and calls directy the
  internal function \code{print.default}.  It should no longer be needed.

  \code{tkfilefind} is a demo in package \code{tcltk} displaying a
  widget for selecting files but the same functionality is available in
  a better form in the \code{tkgetOpenFile} and \code{tkgetSaveFile}
  functions. The demo is reported not even to work with recent versions
  of Tcl and Tk libraries.
}

\seealso{
  \code{\link{Defunct}},
}
\keyword{documentation}
\keyword{utilities}
