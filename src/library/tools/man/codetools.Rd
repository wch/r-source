% File src/library/tools/man/codetools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{codetools}
\alias{find_calls}
\alias{find_calls_in_package_code}
\title{Find Calls in R Code}
\description{
  Find calls in parsed R code, or in the results of parsing the R code
  in given collections of files in unpacked package sources.
}
\usage{
find_calls(x, predicate = NULL, recursive = FALSE)
find_calls_in_package_code(dir, predicate = NULL, recursive = FALSE,
                           which = "code")
}
\arguments{
  \item{x}{an object of type \code{"\link{expression}"} as obtained from
    \code{link{parse}}.}
  \item{predicate}{a unary function for selecting the calls of interest.
    Only the calls \code{e} with \code{any(predicate(e))} are returned.
    By default, all calls are selected.}
  \item{recursive}{a logical indicating whether to return only top-level
    calls (default) or recursively all calls satisfying the predicate.} 
  \item{dir}{a character string specifying the path to a package's root
    source directory.}
  \item{which}{a character vector specifying the files to be looked
    through, with elements matching
    \code{"code"}, \code{"data"}, \code{"demo"}, \code{"tests"},
    \code{"vignettes"},
    \code{"NAMESPACE"}, \code{"CITATION"}, or
    \code{"docs"}.
    The first five correspond to the R code files in the \file{R},
    \file{data}, \file{demo}, \file{tests} (and common unit testing
    framework subdirectories), and \file{inst/doc}, the next two to
    files \file{NAMESPACE} and \file{inst/CITATION}.  The last uses the
    R code in examples of the Rd files in the \file{man} subdirectory.
  }
}
\value{
  For \code{find_calls}, a list of the calls satisfying the predicate.

  For \code{find_calls_in_package_code}, a named list of such lists,
  with elements corresponding to the files looked through.
}
\examples{
## Where is sapply() still used in the 'tools' code?
## If we cannot easily access the unpacked sources:
exprs <- as.list(asNamespace("tools"))
predicate <- function(e) {
    is.call(e) && deparse1(e[[1L]]) == "sapply"
}
find_calls(exprs, predicate, recursive = TRUE)

## Where is sapply() still used in the demos for 'base'?
## If we cannot easily access the unpacked sources, we can take
## advantage of the fact that the installed package has the copied
## 'demo' subdirectory at top level too:
calls <- find_calls_in_package_code(system.file(package = "base"),
                                    predicate, recursive = TRUE,
                                    which = "demo")
## This is a named list of calls found in each demo file:
names(calls)
## Not all of these have calls to sapply():
table(lengths(calls))
## The ones that do:
Filter(length, calls)
}
