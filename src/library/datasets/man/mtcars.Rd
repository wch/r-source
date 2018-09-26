% File src/library/datasets/man/mtcars.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{mtcars}
\docType{data}
\alias{mtcars}
\title{Motor Trend Car Road Tests}
\description{
  The data was extracted from the 1974 \emph{Motor Trend} US magazine,
  and comprises fuel consumption and 10 aspects of
  automobile design and performance for 32 automobiles (1973--74
  models).
}
\usage{mtcars}
\format{
  A data frame with 32 observations on 11 (numeric) variables.
  \tabular{rll}{
    [, 1] \tab mpg  \tab Miles/(US) gallon \cr
    [, 2] \tab cyl  \tab Number of cylinders \cr
    [, 3] \tab disp \tab Displacement (cu.in.) \cr
    [, 4] \tab hp   \tab Gross horsepower \cr
    [, 5] \tab drat \tab Rear axle ratio \cr
    [, 6] \tab wt   \tab Weight (1000 lbs) \cr
    [, 7] \tab qsec \tab 1/4 mile time \cr
    [, 8] \tab vs   \tab Engine (0 = V-shaped, 1 = straight) \cr
    [, 9] \tab am   \tab Transmission (0 = automatic, 1 = manual) \cr
    [,10] \tab gear \tab Number of forward gears \cr
    [,11] \tab carb \tab Number of carburetors
  }
}
\source{
  Henderson and Velleman (1981),
  Building multiple regression models interactively.
  \emph{Biometrics}, \bold{37}, 391--411.
}
\note{
  Henderson and Velleman (1981) comment in a footnote to Table 1:
  \sQuote{Hocking [original transcriber]'s noncrucial coding of the
    Mazda's rotary engine as a straight six-cylinder engine and the
    Porsche's flat engine as a V engine, as well as the inclusion of the
    diesel Mercedes 240D, have been retained to enable direct comparisons
    to be made with previous analyses.}
}
\examples{
require(graphics)
pairs(mtcars, main = "mtcars data", gap = 1/4)
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
## possibly more meaningful, e.g., for summary() or bivariate plots:
mtcars2 <- within(mtcars, {
   vs <- factor(vs, labels = c("V", "S"))
   am <- factor(am, labels = c("automatic", "manual"))
   cyl  <- ordered(cyl)
   gear <- ordered(gear)
   carb <- ordered(carb)
})
summary(mtcars2)
}
\keyword{datasets}
