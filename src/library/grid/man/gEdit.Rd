\name{gEdit}
\alias{gEdit}
\alias{gEditList}
\alias{applyEdit}
\alias{applyEdits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create and Apply Edit Objects }
\description{
  The functions \code{gEdit} and \code{gEditList} create
  objects representing an edit operation (essentially a list
  of arguments to \code{editGrob}).

  The functions \code{applyEdit} and \code{applyEdits} apply
  one or more edit operations to a graphical object.

  These functions are most useful for developers creating new
  graphical functions and objects.
}
\usage{
gEdit(...)
gEditList(...)
applyEdit(x, edit)
applyEdits(x, edits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ one or more arguments to the \code{editGrob} function
    (for \code{gEdit}) or one or more \code{"gEdit"} objects
    (for \code{gEditList}).}
  \item{x}{ a grob (grid graphical object).}
  \item{edit}{ a \code{"gEdit"} object.}
  \item{edits}{ either a \code{"gEdit"} object or a \code{"gEditList"}
    object.}
}
\value{
  \code{gEdit} returns an object of class \code{"gEdit"}.

  \code{gEditList} returns an object of class \code{"gEditList"}.

  \code{applyEdit} and \code{applyEditList} return the modified grob.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grob}}
  \code{\link{editGrob}}
}
\examples{
grid.rect(gp=gpar(col="red"))
# same thing, but more verbose
grid.draw(applyEdit(rectGrob(), gEdit(gp=gpar(col="red"))))
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
