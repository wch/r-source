% File src/library/stats/man/smooth.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{smooth}
\alias{smooth}
\title{Tukey's (Running Median) Smoothing}
\description{
  Tukey's smoothers, \emph{3RS3R}, \emph{3RSS}, \emph{3R}, etc.
}
\usage{
smooth(x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"),
       twiceit = FALSE, endrule = "Tukey", do.ends = FALSE)
}
\arguments{
  \item{x}{a vector or time series}
  \item{kind}{a character string indicating the kind of smoother required;
    defaults to \code{"3RS3R"}.}
  \item{twiceit}{logical, indicating if the result should be \sQuote{twiced}.
    Twicing a smoother \eqn{S(y)} means \eqn{S(y) + S(y - S(y))}, i.e.,
    adding smoothed residuals to the smoothed values.  This decreases
    bias (increasing variance).}
  \item{endrule}{a character string indicating the rule for smoothing at the
    boundary.  Either \code{"Tukey"} (default) or \code{"copy"}.}
  \item{do.ends}{logical, indicating if the 3-splitting of ties should
    also happen at the boundaries (ends).  This is only used for
    \code{kind = "S"}.}
}
\details{
  \emph{\code{3}} is Tukey's short notation for running \code{\link{median}}s
  of length \bold{3},
  \cr
  \emph{\code{3R}} stands for \bold{R}epeated \emph{\code{3}} until
  convergence, and
  \cr
  \emph{\code{S}} for \bold{S}plitting of horizontal stretches of length 2 or 3.

  Hence, \emph{\code{3RS3R}} is a concatenation of \code{3R}, \code{S}
  and \code{3R}, \emph{\code{3RSS}} similarly,
  whereas \emph{\code{3RSR}} means first \code{3R}
  and then \code{(S and 3)} \bold{R}epeated until convergence -- which
  can be bad.
}
\value{
  An object of class \code{"tukeysmooth"} (which has \code{print} and
  \code{summary} methods) and is a vector or time series containing the
  smoothed values with additional attributes.
}
\note{
  S and S-PLUS use a different (somewhat better) Tukey smoother in
  \code{smooth(*)}.
  Note that there are other smoothing methods which provide
  rather better results.  These were designed for hand calculations
  and may be used mainly for didactical purposes.

  Since \R version 1.2, \code{smooth} \emph{does} really implement
  Tukey's end-point rule correctly (see argument \code{endrule}).

  \code{kind = "3RSR"} has been the default till \R-1.1,
  but it can have very bad properties, see the examples.

  Note that repeated application of \code{smooth(*)} \emph{does}
  smooth more, for the \code{"3RS*"} kinds.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.
}
\seealso{
  \code{\link{runmed}} for running medians;
  \code{\link{lowess}} and \code{\link{loess}};
  \code{\link{supsmu}} and
  \code{\link{smooth.spline}}.
}
\examples{
require(graphics)

## see also   demo(smooth) !

x1 <- c(4, 1, 3, 6, 6, 4, 1, 6, 2, 4, 2) # very artificial
(x3R <- smooth(x1, "3R")) # 2 iterations of "3"
smooth(x3R, kind = "S")

sm.3RS <- function(x, ...)
   smooth(smooth(x, "3R", ...), "S", ...)

y <- c(1, 1, 19:1)
plot(y, main = "misbehaviour of \"3RSR\"", col.main = 3)
lines(sm.3RS(y))
lines(smooth(y))
lines(smooth(y, "3RSR"), col = 3, lwd = 2)  # the horror

x <- c(8:10, 10, 0, 0, 9, 9)
plot(x, main = "breakdown of  3R  and  S  and hence  3RSS")
matlines(cbind(smooth(x, "3R"), smooth(x, "S"), smooth(x, "3RSS"), smooth(x)))

presidents[is.na(presidents)] <- 0 # silly
summary(sm3 <- smooth(presidents, "3R"))
summary(sm2 <- smooth(presidents,"3RSS"))
summary(sm  <- smooth(presidents))

all.equal(c(sm2), c(smooth(smooth(sm3, "S"), "S")))  # 3RSS  === 3R S S
all.equal(c(sm),  c(smooth(smooth(sm3, "S"), "3R"))) # 3RS3R === 3R S 3R

plot(presidents, main = "smooth(presidents0, *) :  3R and default 3RS3R")
lines(sm3, col = 3, lwd = 1.5)
lines(sm, col = 2, lwd = 1.25)
}
\keyword{robust}
\keyword{smooth}
