\name{trees}
\title{Girth, Height and Volume for Black Cherry Trees}
\usage{data(trees)}
\alias{trees}
\format{A data frame with 31 observations on 3 variables.
  \tabular{rlll}{
  [,1]  \tab Girth   \tab numeric  \tab Tree diameter at 4 ft 6 in\cr
  [,2]  \tab Height  \tab numeric  \tab Height in ft\cr
  [,3]  \tab Volume  \tab numeric  \tab Volume of timber in cubic ft\cr
}
\source{
  Ryan, T. A., Joiner, B. L. and Ryan, B. F. (1976)
  \emph{The Minitab Student Handbook.} Duxbury Press.
}
\description{
  This data set provides measurements of the girth, height and volume
  of timber in 31 felled black cherry trees.  Note that girth is the
  diameter of the tree at 4 ft 6 in above the ground.
}
\references{
  Atkinson, A. C. (1985)  \emph{Plots, Transformations and Regression.}
  Oxford University Press.
}
\examples{
data(trees)
pairs(trees, panel = panel.smooth, main = "trees data")
plot(Volume ~ Girth, data = trees, log = "xy")
coplot(log(Volume) ~ log(Girth) | Height, data = trees,
       panel = panel.smooth)
summary(fm1 <- lm(log(Volume) ~ log(Girth), data = trees))
}
\keyword{datasets}
