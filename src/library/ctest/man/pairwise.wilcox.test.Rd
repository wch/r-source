\name{pairwise.wilcox.test}
\alias{pairwise.wilcox.test}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Pairwise Wilcoxon rank sum tests}
\description{
 Calculate pairwise comparisons between group levels with corrections
 for multiple testing 
}
\usage{
pairwise.wilcox.test(x, g, p.adjust.method=p.adjust.methods, \dots)
}
\arguments{
 \item{x}{ Response vector }
 \item{g}{ Grouping vector or factor }
 \item{p.adjust.method}{ Method for adjusting p values (see \code{\link{p.adjust}}) }
 \item{\dots}{Additional arguments to pass to \code{t.test} }
}
\value{
  Object of class \code{"pairwise.htest"}
}
\seealso{ \code{\link{wilcox.test}}, \code{\link[base]{p.adjust}}}

\examples{
data(airquality)
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.wilcox.test(Ozone, Month)
pairwise.wilcox.test(Ozone, Month, p.adj = "bonf")
detach()
}
\keyword{ htest }
