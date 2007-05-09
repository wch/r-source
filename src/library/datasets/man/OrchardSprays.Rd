\name{OrchardSprays}
\docType{data}
\alias{OrchardSprays}
\title{Potency of Orchard Sprays}
\description{
  An experiment was conducted to assess the potency of various
  constituents of orchard sprays in repelling honeybees, using a
  Latin square design.
}
\usage{OrchardSprays}
\format{
  A data frame with 64 observations on 4 variables.
  \tabular{rlll}{
    [,1]  \tab rowpos    \tab numeric \tab Row of the design\cr
    [,2]  \tab colpos    \tab numeric \tab Column of the design\cr
    [,3]  \tab treatment \tab factor  \tab Treatment level\cr
    [,4]  \tab decrease  \tab numeric \tab Response
  }
}
\details{
  Individual cells of dry comb were filled with measured amounts of lime
  sulphur emulsion in sucrose solution.  Seven different concentrations
  of lime sulphur ranging from a concentration of 1/100 to 1/1,562,500
  in successive factors of 1/5 were used as well as a solution
  containing no lime sulphur. 
  
  The responses for the different solutions were obtained by releasing
  100 bees into the chamber for two hours, and then measuring the
  decrease in volume of the solutions in the various cells.
  
  An \eqn{8 \times 8}{8 x 8} Latin square design was used and the
  treatments were coded as follows:
  \tabular{rl}{
    A \tab highest level of lime sulphur\cr
    B \tab next highest level of lime sulphur\cr
    . \tab \cr
    . \tab \cr
    . \tab \cr
    G \tab lowest level of lime sulphur\cr
    H \tab no lime sulphur
  }
}
\source{
  Finney, D. J. (1947)
  \emph{Probit Analysis}.
  Cambridge.
}
\references{
  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  New York: Wiley.
}
\examples{
require(graphics)
pairs(OrchardSprays, main = "OrchardSprays data")
}
\keyword{datasets}
