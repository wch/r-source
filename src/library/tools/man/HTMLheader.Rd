\name{HTMLheader}
\alias{HTMLheader}
\title{
Generate a standard HTML header for R help.
}
\description{
This function generates the standard HTML header used on R help pages.
}
\usage{
HTMLheader(title = "R", logo = TRUE, up = NULL, top = file.path(Rhome, "doc/html/index.html"), 
           Rhome = "", headerTitle = paste("R:", title), outputEncoding = "UTF-8")
}
\arguments{
  \item{title}{
The title to display and use in the HTML headers.  Should have had any
HTML escaping already done.
}
  \item{logo}{
Whether to display the \R{} logo after the title.
}
  \item{up}{
Which page (if any) to link to on the \dQuote{up} button.
}
  \item{top}{
Which page (if any) to link to on the \dQuote{top} button.
}
  \item{Rhome}{
A relative path to the R home directory.  See the Details.
}
  \item{headerTitle}{
The title used in the headers.
}
  \item{outputEncoding}{
The declared encoding for the whole page.
}
}
\details{
The \code{up} and \code{top} links should be relative to the current page.
The \code{Rhome} path default works with dynamic help; for static help, a
relative path (e.g. \file{../..}) to it should be used.
}
\value{
A character vector containing the lines of an HTML header which can be used
to start a page in the R help system.
}
\examples{
cat(HTMLheader("This is a sample header"), sep="\n")
}
\keyword{ utilities }
\keyword{ documentation }
