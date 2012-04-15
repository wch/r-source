% File src/library/utils/man/mirrorAdmin.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{mirrorAdmin}
\alias{mirrorAdmin}
\alias{mirror2html}
\alias{checkCRAN}
\title{Managing Repository Mirrors}
\description{
  Functions helping to maintain CRAN, some of them may also be useful
  for administrators of other repository networks.
}
\usage{
mirror2html(mirrors = NULL, file = "mirrors.html",
  head = "mirrors-head.html", foot = "mirrors-foot.html")
checkCRAN(method)
}
\arguments{
  \item{mirrors}{A data frame, by default the CRAN list of mirrors is used.}
  \item{file}{A \link{connection} or a character string.}
  \item{head}{Name of optional header file.}
  \item{foot}{Name of optional footer file.}
  \item{method}{Download method, see \code{download.file}.}
}
\details{
  \code{mirror2html} creates the HTML file for the CRAN list of mirrors
  and invisibly returns the HTML text.

  \code{checkCRAN} performs a sanity checks on all CRAN mirrors.
}
\keyword{misc}

