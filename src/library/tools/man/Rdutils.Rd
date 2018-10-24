% File src/library/tools/man/Rdutils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{Rdutils}
\alias{Rd_db}
\title{Rd Utilities}
\description{Utilities for computing on the information in Rd objects.}
\usage{
Rd_db(package, dir, lib.loc = NULL, stages = "build")
}
\arguments{
  \item{package}{a character string naming an installed package.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \file{man}
    with \R documentation sources (in Rd format).  Only used if
    \code{package} is not given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
  \item{stages}{if \code{dir} is specified and the database is being
    built from source, which stages of \verb{\Sexpr} processing should be
    processed?}
}
\details{
  \code{Rd_db} builds a simple database of all Rd objects in a package,
  as a list of the results of running \code{\link{parse_Rd}} on the Rd
  source files in the package and processing platform conditionals
  and some \verb{\Sexpr} macros.
}
\seealso{
  \code{\link{parse_Rd}}
}
\examples{\donttest{
## Build the Rd db for the (installed) base package.
db <- Rd_db("base")

## Keyword metadata per Rd object.
keywords <- lapply(db, tools:::.Rd_get_metadata, "keyword")
## Tabulate the keyword entries.
kw_table <- sort(table(unlist(keywords)))
## The 5 most frequent ones:
rev(kw_table)[1 : 5]
## The "most informative" ones:
kw_table[kw_table == 1]

## Concept metadata per Rd file.
concepts <- lapply(db, tools:::.Rd_get_metadata, "concept")
## How many files already have \concept metadata?
sum(sapply(concepts, length) > 0)
## How many concept entries altogether?
length(unlist(concepts))
}}
\keyword{utilities}
\keyword{documentation}
