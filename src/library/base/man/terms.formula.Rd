\name{terms.formula}
\alias{terms.formula}
\title{A function to construct a terms object from a formula. }
\description{
  This function takes a formula and some optional arguments and
  constructs a terms object. The terms object can then be used to
  construct a \code{\link{model.matrix}}.
}
\usage{
terms.formula(x, specials=NULL, abb=NULL, data=NULL, neg.out=TRUE,
              keep.order=FALSE, ...)
}
\arguments{
  \item{x}{A formula.}
  \item{specials}{What functions in the formula should be marked as
    special in the terms object.}
  \item{abb}{Unused in R.}
  \item{data}{A data frame from which the meaning of the special symbol
    \code{.} can be inferred. It is unused if there is no \code{.} in
    the formula.}
  \item{neg.out}{\code{TRUE} if terms with a minus, \code{-} should be
    removed. If FALSE these are kept in and indicate a negative order
    (for fractionate?).}
  \item{keep.order}{A logical value indicating whether the terms should
    keep their positions. If FALSE the terms are reordered so that main
    effects come first.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  Not all of the options work in the same way that they do in S and not
  all are implemented.
}
\value{
  A \code{\link{terms}} object is returned.
}
\seealso{ \code{\link{terms.object}},  \code{\link{terms.default}} }
\keyword{ models }
