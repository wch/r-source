\name{pairwise.t.test}
\alias{pairwise.t.test}
\title{ Pairwise t tests}
\description{
 Calculate pairwise comparisons between group levels with corrections
 for multiple testing 
}
\usage{
pairwise.t.test(x, g, p.adjust.method = p.adjust.methods,
                pool.sd = TRUE, \dots)
}
\arguments{
 \item{x}{ Response vector }
 \item{g}{ Grouping vector or factor }
 \item{p.adjust.method}{ Method for adjusting p values (see \code{\link{p.adjust}}) }
 \item{pool.sd}{ Switch to allow/disallow the use of a pooled SD }
 \item{\dots}{ Additional arguments to pass to \code{t.test} }
}
\value{
  Object of class \code{"pairwise.htest"}
}
\seealso{ \code{\link{t.test}}, \code{\link{p.adjust}}}

\examples{
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.t.test(Ozone, Month)
pairwise.t.test(Ozone, Month, p.adj = "bonf")
pairwise.t.test(Ozone, Month, pool.sd = FALSE)
detach()
}
\keyword{htest}
