\name{model.extract}
\title{Extract Components from a Model Frame}
\usage{
model.extract(frame, component)
model.offset(frame)
model.response(frame)
model.weights(x)
}
\alias{model.extract}
\alias{model.offset}
\alias{model.response}
\alias{model.weights}
\arguments{
 \item{frame, x}{A model frame}
 \item{component}{The name of a components to extract, such as
   \code{"weights"}, \code{"subset"}}
}
\description{Returns the response, offset, subset, weights or other
  special components of a model frame passed as optional arguments to
  \code{\link{model.frame}}.
}
\details{
  \code{model.offset} and
  \code{model.response} are equivalent to \code{model.frame(, "offset")}
  and \code{model.frame(, "response")} respectively.

  \code{model.weights} is slightly different from
  \code{model.frame(, "weights")} in not naming the vector it returns.
}
\value{The specified component of the model frame, usually a vector.}

\seealso{\code{\link{model.frame}}, \code{\link{offset}}}

\examples{
data(esoph)
a <- model.frame(cbind(ncases,ncontrols) ~ agegp+tobgp+alcgp, data=esoph)
model.extract(a, "response")
all(model.extract(a, "response") == model.response(a))

a <- model.frame(ncases/(ncases+ncontrols) ~ agegp+tobgp+alcgp,
                 data = esoph, weights = ncases+ncontrols)
model.response(a)
model.extract(a, "weights")

a <- model.frame(cbind(ncases,ncontrols) ~ agegp,
                 something = tobgp, data = esoph)
names(a)
all(model.extract(a, "something") == esoph$tobgp) # TRUE
}
\keyword{manip}
\keyword{programming}
\keyword{models}
