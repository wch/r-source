% file lqs/man//predict.lqs.Rd
% copyright (C) 1999 B. D. Ripley
%
\name{predict.lqs}
\alias{predict.lqs}
\title{
  Predict from an lqs Fit
}
\description{
  Predict from an resistant regression fitted by \code{lqs}.
}
\usage{
\method{predict}{lqs}(object, newdata, na.action = na.pass, \dots)
}
\arguments{
  \item{object}{
    object inheriting from class \code{"lqs"}
  }
  \item{newdata}{
    matrix or data frame of cases to be predicted or, if object
    has a formula, a data frame with columns of the same names as the
    variables used. A vector will be interpreted
    as a row vector.  If \code{newdata} is missing, an attempt will be
    made to retrieve the data used to fit the \code{lqs} object.
  }
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{\dots}{arguments to be passed from or to other methods.}
}
\value{
  A vector of predictions.
}
\details{
  This function is a method for the generic function
  \code{predict()} for class \code{lqs}.
  It can be invoked by calling \code{predict(x)} for an
  object \code{x} of the appropriate class, or directly by
  calling \code{predict.lqs(x)} regardless of the
  class of the object.

  Missing values in \code{newdata} are handled by returning \code{NA} if the
  linear fit cannot be evaluated. If \code{newdata} is omitted and
  the \code{na.action} of the fit omitted cases, these will be omitted on the
  prediction.
}
\author{B.D. Ripley}
\seealso{
\code{\link{lqs}}
}
\examples{
data(stackloss)
set.seed(123)
fm <- lqs(stack.loss ~ ., data = stackloss, method = "S", nsamp = "exact")
predict(fm, stackloss)
}
\keyword{models}
