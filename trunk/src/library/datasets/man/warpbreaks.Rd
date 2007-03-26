\name{warpbreaks}
\docType{data}
\alias{warpbreaks}
\title{The Number of Breaks in Yarn during Weaving}
\description{
  This data set gives the number of warp breaks per loom, where a loom
  corresponds to a fixed length of yarn.
}
\usage{warpbreaks}
\format{
  A data frame with 54 observations on 3 variables.
  \tabular{rlll}{
    \code{[,1]} \tab \code{breaks}  \tab numeric \tab The number of breaks\cr
    \code{[,2]} \tab \code{wool}    \tab factor  \tab The type of wool (A or B)\cr
    \code{[,3]} \tab \code{tension} \tab factor  \tab The level of tension (L, M, H)
  }
  There are measurements on 9 looms for each of the six types of warp
  (\code{AL}, \code{AM}, \code{AH}, \code{BL}, \code{BM}, \code{BH}).
}
\source{
  Tippett, L. H. C. (1950)
  \emph{Technological Applications of Statistics}.
  Wiley.  Page 106.

}
\references{
  Tukey, J. W. (1977)
  \emph{Exploratory Data Analysis}.
  Addison-Wesley.

  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\seealso{
  \code{\link{xtabs}} for ways to display these data as a table.
}
\examples{
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
