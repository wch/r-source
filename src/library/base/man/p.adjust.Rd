\name{p.adjust}
\alias{p.adjust}
\alias{p.adjust.methods}
\title{Adjust p values for multiple comparisons}
\description{
  Given a set of p values, returns p values adjusted using one of
  several methods.
}
\usage{
p.adjust(p, method=p.adjust.methods, n=length(p))
p.adjust.methods # c("holm", "hochberg", "bonferroni","none")
}
\arguments{
 \item{p}{vector of p values}
 \item{method}{correction method}
 \item{n}{number of comparisons}
}
\details{
  The adjustment methods include the Bonferroni correction in
  which the p values are multiplied by the number of comparisons. Two
  less conservative corrections by Holm, respectively Hochberg,
  are also included. A pass-through option \code{"none"} is also
  included. The set of methods are contained in the
  \code{p.adjust.methods} vector for the benefit of methods that need to
  have the method as an option and pass it on to \code{p.adjust}.
}
\value{
  A vector of corrected p values.
}
\references{ S Paul Wright: Adjusted P-values for simultaneous
  inference,  Biometrics \bold{48}, 1005--1013
 }
\note{The Hochberg method is only proved to work if the p values are
  independent, although simulations have indicated that it works in
  correlated cases as well. Hence the Holm method is the default.}
\seealso{\code{pairwise.*} functions in the \code{ctest} package, such
  as \code{\link[ctest]{pairwise.t.test}}.
}
\examples{
  p <- runif(50)
  p.adjust(p)
}
\keyword{ htest }
