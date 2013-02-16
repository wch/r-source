\name{vignetteEngine}
\alias{vignetteEngine}
\title{
Set or get a vignette processing engine.
}
\description{
Vignettes are normally processed by \code{\link{Sweave}}, but package writers
may choose to use a different engine (e.g. \code{knitr} or \code{noweb}).  This
function is used by those packages to register their engines, and internally
by \R{} to retrieve them.
}
\usage{
vignetteEngine(name, weave, tangle)
}
\arguments{
  \item{name}{
The name of the engine.
}
  \item{weave}{
A function to convert vignette source files to LaTeX output.
}
  \item{tangle}{
A function to convert vignette source files to R code.
}
}
\details{
The \code{weave} and \code{tangle} functions must be defined with
argument lists compatible with \code{\link{Sweave}} and 
\code{\link{Stangle}} respectively. If the filename being processed
has one of the Sweave extensions (i.e. matching the regular expression
\code{"[.][RrSs](nw|tex)$"}), the \code{weave} function should produce 
a \file{.tex} file in the same directory.  If it has the extension
\file{.Rmd}, weaving should produce an \file{.html} file.  The
\code{tangle} function should always produce a \file{.R} file.

If \code{weave} is missing, \code{vignetteEngine} will return the currently
registered engine.  If it is \code{NULL}, the engine will be deleted.
}
\value{
If the engine is being deleted, \code{NULL}.  Otherwise a list containing components
\item{name}{The name of the engine}
\item{weave}{The weave function}
\item{tangle}{The tangle function}
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Sweave}} and the \sQuote{Writing R Extensions} manual.
}
\examples{
str(vignetteEngine("Sweave"))
}
\keyword{utilities}
\keyword{documentation}

