% File src/library/tools/man/HTMLheader.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2014 R Core Team
% Distributed under GPL 2 or later

\name{HTMLheader}
\alias{HTMLheader}
\title{Generate a Standard HTML Header for R Help}
\description{
This function generates the standard HTML header used on R help pages.
}
\usage{
HTMLheader(title = "R", logo = TRUE, up = NULL,
           top = file.path(Rhome, "doc/html/index.html"),
           Rhome = "",
           css = file.path(Rhome, "doc/html/R.css"),
           headerTitle = paste("R:", title),
           outputEncoding = "UTF-8")
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
A \bold{relative} path to the R home directory.  See the \sQuote{Details}.
}
  \item{css}{
The relative URL for the Cascading Style Sheet.
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
relative path (e.g., \file{../..}) to it should be used.
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
