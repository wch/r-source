% File src/library/utils/man/RShowDoc.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{RShowDoc}
\alias{RShowDoc}
\title{Show R Manuals and Other Documentation}
\description{
  Utility function to find and display \R documentation.
}
\usage{
RShowDoc(what, type = c("pdf", "html", "txt"), package)
}
\arguments{
  \item{what}{a character string: see \sQuote{Details}.}
  \item{type}{an optional character string giving the preferred format.
      Can be abbreviated.}
  \item{package}{an optional character string specifying the name of a
    package within which to look for documentation.}
}
\details{
  \code{what} can specify one of several different sources of documentation,
  including the \R manuals (\code{R-admin}, \code{R-data}, \code{R-exts},
  \code{R-intro}, \code{R-ints}, \code{R-lang}), \code{NEWS},
  \code{COPYING} (the GPL licence), any of the licenses in
  \file{share/licenses}, \code{FAQ} (also available as
  \code{R-FAQ}), and the files in \file{\var{\link{R_HOME}}/doc}.

  Only on Windows, the \R for Windows FAQ is specified by \code{rw-FAQ}.

  If \code{package} is supplied, documentation is looked for in the
  \file{doc} and top-level directories of an installed package of that name.

  If \code{what} is missing a brief usage message is printed.

  The documentation types are tried in turn starting with the first
  specified in \code{type} (or \code{"pdf"} if none is specified).
}
\seealso{
  For displaying regular help files, \code{\link{help}} (or
  \code{\link{?}}) and \code{\link{help.start}}.

  For \code{type = "txt"}, \code{\link{file.show}} is used.
  \code{\link{vignette}}s are nicely viewed via \code{RShowDoc(*, package= . )}.
}
\value{
  A invisible character string given the path to the file found.
}
\examples{\donttest{
RShowDoc("R-lang")
RShowDoc("FAQ", type = "html")
RShowDoc("frame", package = "grid")
RShowDoc("changes.txt", package = "grid")
RShowDoc("NEWS", package = "MASS")
}}
\keyword{documentation}


