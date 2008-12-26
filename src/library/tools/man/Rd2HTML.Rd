% File src/library/tools/man/Rd2HTML.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{Rd2HTML}
\Rdversion{1.1}
\alias{Rd2HTML}
\alias{checkRd}
\title{ Work with an Rd object }
\description{
  These experimental functions take the output of the
  \code{\link{parse_Rd}} function and check it or produce an HTML help
  page from it.  Their interfaces (and existence!) are subject to
  change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type)
checkRd(Rd, defines=.Platform$OS.type, unknownOkay = FALSE, listOkay = FALSE)
}
\arguments{
  \item{Rd}{ the \code{Rd} object }
  \item{out}{ a filename or connection object to which to write the output }
  \item{package}{ the package to list in the output }
  \item{defines}{ strings to use in \code{"#ifdef"} tests }
  \item{unknownOkay}{ unrecognized macros are treated as errors if \code{TRUE}}
  \item{listOkay}{ unnecessary non-empty braces (e.g. around text, not
    as an argument) are treated as errors if \code{TRUE}}
}
\details{
  The \code{Rd2HTML} converter is still in development, and currently
  has the following known deficiencies:
  \itemize{
    \item \code{"\\link"} macros are not generated properly.
    \item \code{"\\enc"} and \code{"\\encoding"} are not supported.
    \item Error messages are not as informative as they could be.  Often
    using \code{parse_Rd(..., verbose=TRUE)} will give an idea of what is wrong.
    \item Formatting is not always optimal.
  }
}
\value{
  \code{Rd2HTML} is executed mainly for the side effect of writing the
  HTML page.  Its value is the name of that page.
  
  \code{checkRd} returns \code{TRUE} if the file has no errors; it
  generates an R error otherwise.
}
\author{ Duncan Murdoch }
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\seealso{ \code{\link{parse_Rd}} }
\examples{
  \dontrun{
    # This example requires access to the R source code
    Rsrc <- file.path(R.home(), "src")
    infile <- file.path(Rsrc, "library","tools","man", "Rd2HTML.Rd")
    outfile <- paste(tempfile(), ".html", sep="")
    browseURL(Rd2HTML(infile, outfile, package="tools"))
    checkRd(infile) # A stricter test than Rd2HTML uses
  }
}
\keyword{ documentation }
