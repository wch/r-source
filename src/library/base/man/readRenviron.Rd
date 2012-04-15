% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010 R Core Team
% Distributed under GPL 2 or later

\name{readRenviron}
\alias{readRenviron}
\title{
  Set Environment Variables from a File
}
\description{
  Read as file such as \file{.Renviron} or \file{Renviron.site} in the
  format described in the help for \link{Startup}, and set environment
  variables as defined in the file.
}
\usage{
readRenviron(path)
}
\arguments{
  \item{path}{A length-one character vector giving the path to the
    file.  Tilde-expansion is performed where supported.}
}
\value{
  Scalar logical indicating if the file was read successfully.  Returned
  invisibly.  If the file cannot be opened for reading, a warning is given.
}
\seealso{
  \code{\link{Startup}} for the file format.
}
\examples{\dontrun{
## re-read a startup file (or read it in a vanilla session)
readRenviron("~/.Renviron")
}}
\keyword{file}
