\name{warpbreaks}
\title{The Number of Breaks in Yarn during Weaving}
\usage{data(warpbreaks)}
\alias{warpbreaks}
\format{A data frame with 54 observations on 3 variables.
  \tabular{rlll}{
    [,1]  \tab breaks   \tab numeric  \tab The number of breaks\cr
    [,2]  \tab wool     \tab factor   \tab The type of wool (A or B)\cr
    [,3]  \tab tension  \tab factor   \tab The level of tension (L, M, H)
  }
}
\source{L. H. C. Tippet (1950).  Technological Applications of Statistics,
Wiley, page 106.

}
\description{This data set gives the number of warp breaks per loom, where a
loom corresponds to a fixed length of yarn.

}
\references{Tukey, J. W. (1977).  Exploratory Data Analysis.  Addison-Wesley.
McNeil, D. (1977).  Interactive Data Analysis.  Wiley.
}
\examples{
data(warpbreaks)
summary(warpbreaks)
opar <- par(mfrow = c(1,2), oma = c(0, 0, 1.1, 0))
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "A", main = "Wool A")
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "B", main = "Wool B")
mtext("warpbreaks data", side = 3, outer = TRUE)
par(opar)
summary(fm1 <- lm(breaks ~ wool*tension, data = warpbreaks))
anova(fm1)
}
\keyword{datasets}
