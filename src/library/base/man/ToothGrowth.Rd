\name{ToothGrowth}
\title{The Effect of Vitamin C on Tooth Growth in Guinea Pigs}
\usage{data(ToothGrowth)}
\alias{ToothGrowth}
\format{A data frame with 60 observations on 3 variables.

  \tabular{rlll}{
    [,1]  \tab len   \tab numeric  \tab Tooth length\cr
    [,2]  \tab supp  \tab factor   \tab Supplement type (VC or OJ).\cr
    [,3]  \tab dose  \tab numeric  \tab Dose in milligrams.
  }
}
\source{
  C. I. Bliss (1952) \emph{The Statistics of Bioassay.}  Academic
  Press.
}
\description{
  The response is the length of odontoblasts (teeth) in each of 10
  guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2
  mg) with each of two delivery methods (orange juice or ascorbic
  acid).
}
\references{
  McNeil, D. R. (1977) \emph{Interactive Data Analysis.}
  New York: Wiley.
}
\examples{
data(ToothGrowth)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: given is supplement type")
}
\keyword{datasets}
