\name{person}
\title{Person Names and Contact Information}
\alias{person}
\alias{as.person}
\alias{as.person.default}
\alias{personList}
\alias{as.personList}
\alias{as.personList.person}
\alias{as.personList.default}
\alias{as.character.person}
\alias{as.character.personList}
\alias{bibtex.person}
\alias{bibtex.personList}
\description{
  A class and utility methods for holding information about persons
  like name and email address.
}
\usage{
person(first = "", last = "", middle = "", email = "")
personList(...)
as.person(x)
as.personList(x)

\S3method{as.character}{person}(x, ...)
\S3method{as.character}{personList}(x, ...)
}
\arguments{
  \item{first}{character string, first name}
  \item{middle}{character string, middle name(s)}
  \item{last}{character string, last name}
  \item{email}{character string, email address}
  \item{\dots}{for \code{personList} an arbitrary number of \code{person}
    objects}
  \item{x}{a character string or an object of class \code{person} or
    \code{personList}}
}
\keyword{misc}
\examples{
## create a person object directly
p1 <- person("Karl", "Pearson", email = "pearson@stats.heaven")
p1

## convert a string
p2 <- as.person("Ronald Aylmer Fisher")
p2

## create one object holding both
p <- personList(p1, p2)
ps <- as.character(p)
ps
as.personList(ps)
}
