\name{editDetails}
\alias{editDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Customising grid Editing }
\description{
  This generic hook function is called whenever a grid grob is edited
  via \code{grid.edit} or \code{editGrob}.
  This provides an opportunity for customising the editing of a
  new class derived from grob (or gTree).
}
\usage{
editDetails(x, specs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A grid grob. }
  \item{specs}{ A list of named elements.  The names indicate the
    grob slots to modify and the values are the new values for the slots. }
}
\details{
  This function is called by \code{grid.edit} and \code{editGrob}.
  A method should be written for classes derived from grob or gTree
  if a change in a slot has an effect on other slots in the grob or
  children of a gTree (e.g., see \code{grid:::editDetails.xaxis}).

  Note that the slot already has the new value.
}
\value{
  The function MUST return the modified grob.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.edit}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS
