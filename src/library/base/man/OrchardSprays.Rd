\name{OrchardSprays}
\title{Potency of Orchard Sprays}
\usage{data(OrchardSprays)}
\alias{OrchardSprays}
\format{A data frame with 64 observations on 4 variables.
    \tabular{rlll}{
    	[,1]  \tab rowpos    \tab numeric \tab Row of the design\cr
    	[,2]  \tab colpos    \tab numeric \tab Column of the design\cr
    	[,3]  \tab treatment \tab factor  \tab Treatment level\cr
    	[,4]  \tab decrease  \tab numeric \tab Response\cr
    }
}
\source{
    Finney, D. J. (1947).  Probit Analysis.  Cambridge.
}
\description{
    An experiment was conducted to assess the potency of various
    constituents of orchard sprays in repelling honeybees.  Individual
    cells of dry comb were filled with measured amounts of lime sulphur
    emulsion in sucrose solution.  Seven different concentrations of
    lime sulphur ranging from a concentration of 1/100 to 1/1,562,500 in
    succesive factors of 1/5 were used as well as a solution containing
    no lime sulphur. 
  
    The responses for the different solutions were obtained by releasing
    100 bees into the chamber for two hours, and then measuring the
    decrease in volume of the solutions in the various cells.
  
    An 8x8 Latin Square design was used and the treatments were coded as
    follows:
    \tabular{rl}{
    	A \tab highest level of lime sulphur\cr
    	B \tab next highest level of lime sulphur\cr
    	. \tab \cr
    	. \tab \cr
    	. \tab \cr
    	G \tab lowest level of lime sulphur\cr
    	H \tab no lime sulphur\cr
    }
}
\references{
    McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.
}
\keyword{datasets}
