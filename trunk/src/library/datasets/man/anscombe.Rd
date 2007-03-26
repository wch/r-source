\name{anscombe}
\docType{data}
\alias{anscombe}
\title{Anscombe's Quartet of ``Identical'' Simple Linear Regressions}
\description{
  Four \eqn{x}-\eqn{y} datasets which have the same traditional
  statistical properties (mean, variance, correlation, regression line,
  etc.), yet are quite different.
}
\usage{anscombe}
\format{
  A data frame with 11 observations on 8 variables.
  \tabular{rl}{
    x1 == x2 == x3 \tab the integers 4:14, specially arranged \cr
    x4             \tab values 8 and 19 \cr
    y1, y2, y3, y4 \tab numbers in (3, 12.5) with mean 7.5 and sdev 2.03}
}
\source{
  Tufte, Edward R. (1989)
  \emph{The Visual Display of Quantitative Information}, 13--14.
  Graphics Press.
}
\references{
  Anscombe, Francis J. (1973)  Graphs in statistical analysis.
  \emph{American Statistician}, \bold{27}, 17--21.
}
\examples{
require(stats)
summary(anscombe)

##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
for(i in 1:4) {
  ff[2:3] <- lapply(paste(c("y","x"), i, sep=""), as.name)
  ## or   ff[[2]] <- as.name(paste("y", i, sep=""))
  ##      ff[[3]] <- as.name(paste("x", i, sep=""))
  assign(paste("lm.",i,sep=""), lmi <- lm(ff, data= anscombe))
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(objects(pat="lm\\\\.[1-4]$"), function(n) coef(get(n)))
lapply(objects(pat="lm\\\\.[1-4]$"), function(n) summary(get(n))$coef)

## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow=c(2,2), mar=.1+c(4,4,1,1), oma= c(0,0,2,0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste(c("y","x"), i, sep=""), as.name)
  plot(ff, data =anscombe, col="red", pch=21, bg = "orange", cex = 1.2,
       xlim=c(3,19), ylim=c(3,13))
  abline(get(paste("lm.",i,sep="")), col="blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex=1.5)
par(op)
}
\keyword{datasets}
