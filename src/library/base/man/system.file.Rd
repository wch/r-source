\name{system.file}
\title{Find names of \R System Files}
\usage{
system.file(file = "", pkg = .packages(), lib = .lib.loc) {
}
\alias{system.file}
\arguments{
\item{file}{a character, specifying subdirectory and file(s).  The
default, \code{""} returns the root of the base library.}
\item{pkg}{a character vector with package names.}
\item{lib}{a character vector with path names of \R package libraries,
 see \code{\link{.lib.loc} for the default.}
}
\description{
\code{system.file} returns a character vector of positive \code{length},
containing the file names that matched \code{file}, or the empty string,
\code{""} if none matched.
A typical call is \code{fs <- system.file("help/lm")}, followed by
\code{if(fs == "") \{ warning(".. not found") \} else \{ \dots\dots \}}.
}
\examples{
system.file("")#- The 'base' library root
system.file("INDEX")
system.file("demo/*")#> "" : nothing
system.file("help/AnIndex", pkg=c("stepfun","mva"))
system.file("data/w*")
}
\keyword{file}
\keyword{utilities}
