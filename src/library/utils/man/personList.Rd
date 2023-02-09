% File src/library/utils/man/personList.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023 R Core Team
% Distributed under GPL 2 or later

\name{personList}
\title{Collections of Persons (Older Interface)}
\alias{personList}
\alias{as.personList}
\alias{as.personList.person}
\alias{as.personList.default}
\description{
  Old interface providing functionality for information about
  collections of persons.  Since R 2.14.0 \code{\link{person}} objects
  can be combined with the corresponding \code{c} method which
  supersedes the \code{personList} function.
}
\usage{
personList(...)
as.personList(x)
}
\arguments{
  \item{...}{person objects (inheriting from class
    \code{"\link{person}"})}
  \item{x}{an object the elements of which are coercible via
    \code{\link{as.person}}}
}
\value{
  a person object (inheriting from class \code{"\link{person}"})
}
\seealso{
  \code{\link{person}} for the new functionality for representing and
  manipulating information about persons.
}
\keyword{misc}
