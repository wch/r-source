\name{Rd2HTML}
\Rdversion{1.1}
\alias{Rd2HTML}
\title{ Write HTML page from an Rd object }
\description{
This experimental function takes the output of the \code{\link{parse_Rd}} function
and produces an HTML help page from it.  Its interface (and existence!) are subject
to change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type)
}
\arguments{
  \item{Rd}{ the \code{Rd} object }
  \item{out}{ a filename or connection object to which to write the output }
  \item{package}{ the package to list in the output }
  \item{defines}{ strings to use in \code{"#ifdef"} tests }
}
\details{
  This parser is still in development, and currently has the following known
  deficiencies:
  \itemize{
  \item \code{"\\link"} macros are not generated properly.
  \item \code{"\\enc"} and \code{"\\encoding"} are not supported.
  \item Error messages are not as informative as they could be.  Often
  using \code{parse_Rd(..., verbose=TRUE)} will give an idea of what is wrong.
  \item Formatting is not always optimal.
  }
}
\value{
  This function is executed mainly for the side effect of writing the HTML page.  Its
  value is the name of that page.
}
\author{ Duncan Murdoch }
\references{ \url{http://developer.r-project.org/ParseRd.pdf} }
\seealso{ \code{\link{parse_Rd}} }
\examples{
  \dontrun{
    # This example requires access to the R source code
    Rsrc <- file.path(R.home(), "src")
    infile <- file.path(Rsrc, "library","tools","man", "Rd2HTML.Rd")
    outfile <- paste(tempfile(), ".html", sep="")
    browseURL(Rd2HTML(infile, outfile, package="tools"))
  }
}
\keyword{ documentation }
