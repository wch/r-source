\name{update.lm}
\title{Update a [G]LM Model}
\usage{
 update.lm( lm.obj, formula, data, weights, subset, na.action)
update.glm(glm.obj, formula, data, weights, subset, na.action,
           offset, family, x)
}
\alias{update.lm}
\alias{update.glm}
\arguments{
 \item{[g]lm.obj}{result of \code{\link{lm}} or \code{\link{glm}}.

   %-- would be nice if this could be written OUTSIDE an \item  :
   The following arguments are all \emph{optional}, and have the
   identical meaning as the arguments of \code{\link{lm}} or \code{\link{glm}},
   respectively:}
 \item{formula}{a \code{\link{formula}} for updating the former model.}
 \item{data}{the \code{\link{data.frame}}.}
 \item{weights}{observation weights.}
 \item{subset}{choose a subset of cases.}
 \item{na.action}{a function. what do to with \code{\link{NA}}s, see
   \code{\link{na.action}}.}
 \item{offset}{see \code{\link{glm}}.}
 \item{family}{\code{\link{family}} for \code{glm}.}
 \item{x}{logical, should the \code{x} matrix be returned?}
}
\description{
.....
}
\value{
  An object of the same \code{\link{class}} as the original g\code{lm.obj}.
}
\references{The ``white book''.}
\seealso{
  \code{\link{update.formula}},
  \code{\link{formula}},\code{\link{lm}},\code{\link{glm}}.
}
\examples{

}
\keyword{models}
