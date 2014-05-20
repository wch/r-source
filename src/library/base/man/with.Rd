% File src/library/base/man/with.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{with}
\alias{with}
\alias{with.default}
\alias{within}
\alias{within.list}
\alias{within.data.frame}
\title{Evaluate an Expression in a Data Environment}
\description{
  Evaluate an \R expression in an environment constructed from data,
  possibly modifying the original data.
}
\usage{
with(data, expr, \dots)
within(data, expr, \dots)
}
\arguments{
  \item{data}{data to use for constructing an environment. For the
    default \code{with} method this may be an environment, a list, a
    data frame, or an integer as in \code{sys.call}. For \code{within},
    it can be a list or a data frame.}
  \item{expr}{expression to evaluate.}
  \item{\dots}{arguments to be passed to future methods.}
}
\details{
  \code{with} is a generic function that evaluates \code{expr} in a
  local environment constructed from \code{data}.  The environment has
  the caller's environment as its parent.  This is useful for
  simplifying calls to modeling functions.  (Note: if \code{data} is
  already an environment then this is used with its existing parent.)

  Note that assignments within \code{expr} take place in the constructed
  environment and not in the user's workspace.

  \code{within} is similar, except that it examines the environment
  after the evaluation of \code{expr} and makes the corresponding
  modifications to \code{data} (this may fail in the data frame case if
  objects are created which cannot be stored in a data frame), and
  returns it.  \code{within} can be used as an alternative to
  \code{transform}.
}
\value{
  For \code{with}, the value of the evaluated \code{expr}.  For
  \code{within}, the modified object.
}
\seealso{
  \code{\link{evalq}}, \code{\link{attach}}, \code{\link{assign}},
  \code{\link{transform}}.
}
\examples{
require(stats); require(graphics)
#examples from glm:
\donttest{
library(MASS)
with(anorexia, {
    anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                    family = gaussian)
    summary(anorex.1)
})
}



aq <- within(airquality, {     # Notice that multiple vars can be changed
    lOzone <- log(Ozone)
    Month <- factor(month.abb[Month])
    cTemp <- round((Temp - 32) * 5/9, 1) # From Fahrenheit to Celsius
    rm(Day, Temp)
})
head(aq)

with(data.frame(u = c(5,10,15,20,30,40,60,80,100),
                lot1 = c(118,58,42,35,27,25,21,19,18),
                lot2 = c(69,35,26,21,18,16,13,12,12)),
    list(summary(glm(lot1 ~ log(u), family = Gamma)),
         summary(glm(lot2 ~ log(u), family = Gamma))))

# example from boxplot:
with(ToothGrowth, {
    boxplot(len ~ dose, boxwex = 0.25, at = 1:3 - 0.2,
            subset = (supp == "VC"), col = "yellow",
            main = "Guinea Pigs' Tooth Growth",
            xlab = "Vitamin C dose mg",
            ylab = "tooth length", ylim = c(0, 35))
    boxplot(len ~ dose, add = TRUE, boxwex = 0.25, at = 1:3 + 0.2,
            subset = supp == "OJ", col = "orange")
    legend(2, 9, c("Ascorbic acid", "Orange juice"),
           fill = c("yellow", "orange"))
})

# alternate form that avoids subset argument:
with(subset(ToothGrowth, supp == "VC"),
     boxplot(len ~ dose, boxwex = 0.25, at = 1:3 - 0.2,
             col = "yellow", main = "Guinea Pigs' Tooth Growth",
             xlab = "Vitamin C dose mg",
             ylab = "tooth length", ylim = c(0, 35)))
with(subset(ToothGrowth,  supp == "OJ"),
     boxplot(len ~ dose, add = TRUE, boxwex = 0.25, at = 1:3 + 0.2,
             col = "orange"))
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
}
\keyword{data}
\keyword{programming}
