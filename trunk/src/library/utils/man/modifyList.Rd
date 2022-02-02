% File src/library/utils/man/modifyList.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{modifyList}
\alias{modifyList}
\title{Recursively Modify Elements of a List}
\description{
  Modifies a possibly nested list recursively by changing a subset of
  elements at each level to match a second list.
}
\usage{
modifyList(x, val, keep.null = FALSE)
}
\arguments{
  \item{x}{A named \code{\link{list}}, possibly empty.}
  \item{val}{A named list with components to replace corresponding
    components in \code{x}.}
  \item{keep.null}{ If \code{TRUE}, \code{NULL} elements in \code{val}
    become \code{NULL} elements in \code{x}.  Otherwise, the
    corresponding element, if present, is deleted from \code{x}. }
}
\value{
  A modified version of \code{x}, with the modifications determined as
  follows (here, list elements are identified by their names).  Elements
  in \code{val} which are missing from \code{x} are added to \code{x}.
  For elements that are common to both but are not both lists
  themselves, the component in \code{x} is replaced (or possibly
  deleted, depending on the value of \code{keep.null}) by the one in
  \code{val}.  For common elements that are in both lists, \code{x[[name]]}
  is replaced by \code{modifyList(x[[name]], val[[name]])}.
}
\examples{
foo <- list(a = 1, b = list(c = "a", d = FALSE))
bar <- modifyList(foo, list(e = 2, b = list(d = TRUE)))
str(foo)
str(bar)
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{utilities}
