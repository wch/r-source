\name{with}
\alias{with}
\alias{with.default}
\title{Evaluate an Expression in a Data Environment}
\description{
  Evaluate an \R expression in an environment constructed from data.
}
\usage{
with(data, expr, \dots)
%\method{with}{default}(data, expr, \dots)
}
\arguments{
  \item{data}{data to use for constructing an environment. For the
    default method this may be an environment, a list, a data frame,
    or an integer as in \code{sys.call}.}
  \item{expr}{expression to evaluate.}
  \item{\dots}{arguments to be passed to future methods.}
}
\details{
  \code{with} is a generic function that evaluates \code{expr} in a
  local environment constructed from \code{data}.  The environment has
  the caller's environment as its parent.  This is useful for
  simplifying calls to modeling functions.

  Note that assignments within \code{expr} take place in the constructed
  environment and not in the user's workspace.
}
\seealso{
  \code{\link{evalq}}, \code{\link{attach}}.
}
\examples{
require(stats); require(graphics)
#examples from glm:
\dontrun{
library(MASS)
with(anorexia, {
    anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                    family = gaussian)
    summary(anorex.1)
})
}

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
            ylab = "tooth length", ylim = c(0,35))
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
             ylab = "tooth length", ylim = c(0,35)))
with(subset(ToothGrowth,  supp == "OJ"),
     boxplot(len ~ dose, add = TRUE, boxwex = 0.25, at = 1:3 + 0.2,
             col = "orange"))
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
}
\keyword{data}
\keyword{programming}
