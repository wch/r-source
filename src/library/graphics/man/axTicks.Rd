% File src/library/graphics/man/axTicks.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{axTicks}
\alias{axTicks}
\title{Compute Axis Tickmark Locations}
\description{
  Compute pretty tickmark locations, the same way as \R does internally.
  This is only non-trivial when \bold{log} coordinates are active.
  By default, gives the \code{at} values which
  \code{\link{axis}(side)} would use.
}
\usage{
axTicks(side, axp = NULL, usr = NULL, log = NULL, nintLog = NULL)
}
\arguments{
  \item{side}{integer in 1:4, as for \code{\link{axis}}.}
  \item{axp}{numeric vector of length three, defaulting to
    \code{\link{par}("xaxp")} or \code{\link{par}("yaxp")}
    depending on the \code{side} argument (\code{par("xaxp")}
    if \code{side} is 1 or 3, \code{par("yaxp")} if side is 2 or 4).}
  \item{usr}{numeric vector of length two giving user coordinate
    limits, defaulting to the relevant portion of
    \code{\link{par}("usr")} (\code{par("usr")[1:2]} or
    \code{par("usr")[3:4]} for \code{side} in (1,3) or (2,4)
    respectively).}
  \item{log}{logical indicating if log coordinates are active; defaults
    to \code{\link{par}("xlog")} or \code{\link{par}("ylog")}
    depending on \code{side}.}
  \item{nintLog}{(only used when \code{log} is true): approximate (lower
    bound for the) number of tick intervals; defaults to
    \code{\link{par}("lab")[j]} where \code{j} is 1 or 2 depending on
    \code{side}.  Set this to \code{Inf} if you want the same behavior
    as in earlier \R versions (than 2.14.x).}
}
\details{
  The \code{axp}, \code{usr}, and \code{log} arguments must be consistent
  as their default values (the \code{par(..)} results) are.  If you
  specify all three (as non-NULL), the graphics environment is not used
  at all.  Note that the meaning of \code{axp} differs significantly
  when \code{log} is \code{TRUE}; see the documentation on
  \code{\link{par}(xaxp = .)}.

  \code{axTicks()} may be seen as an \R implementation of the C function
  \code{CreateAtVector()} in \file{..../src/main/plot.c}
  which is called by \code{\link{axis}(side, *)} when no argument
  \code{at} is specified or directly by \code{\link{axisTicks}()} (in package
  \pkg{grDevices}).
  \cr
  The delicate case, \code{log = TRUE}, now makes use of
  \code{\link{axisTicks}} unless \code{nintLog = Inf} which exists for back
  compatibility.
}
\value{
  numeric vector of coordinate values at which axis tickmarks can be
  drawn.  By default, when only the first argument is specified,
  these values should be identical to those that
  \code{\link{axis}(side)} would use or has used.  Note that the values
  are decreasing when \code{usr} is (\dQuote{reverse axis} case).
}
\seealso{\code{\link{axis}}, \code{\link{par}}.  \code{\link{pretty}}
  uses the same algorithm (but independently of the graphics
  environment) and has more options.  However it is not available for
  \code{log = TRUE.}

  \code{\link[grDevices]{axisTicks}()} (package \pkg{grDevices}).
}
\examples{
 plot(1:7, 10*21:27)
 axTicks(1)
 axTicks(2)
 stopifnot(identical(axTicks(1), axTicks(3)),
           identical(axTicks(2), axTicks(4)))

## Show how axTicks() and axis() correspond :
op <- par(mfrow = c(3, 1))
for(x in 9999 * c(1, 2, 8)) {
    plot(x, 9, log = "x")
    cat(formatC(par("xaxp"), width = 5),";", T <- axTicks(1),"\n")
    rug(T, col =  adjustcolor("red", 0.5), lwd = 4)
}
par(op)

x <- 9.9*10^(-3:10)
plot(x, 1:14, log = "x")
axTicks(1) # now length 5, in R <= 2.13.x gave the following
axTicks(1, nintLog = Inf) # rather too many

## An example using axTicks() without reference to an existing plot
## (copying R's internal procedures for setting axis ranges etc.),
## You do need to supply _all_ of axp, usr, log, nintLog
## standard logarithmic y axis labels
ylims <- c(0.2, 88)
get_axp <- function(x) 10^c(ceiling(x[1]), floor(x[2]))
## mimic par("yaxs") == "i"
usr.i <- log10(ylims)
(aT.i <- axTicks(side = 2, usr = usr.i,
                 axp = c(get_axp(usr.i), n = 3), log = TRUE, nintLog = 5))
## mimic (default) par("yaxs") == "r"
usr.r <- extendrange(r = log10(ylims), f = 0.04)
(aT.r <- axTicks(side = 2, usr = usr.r,
                 axp = c(get_axp(usr.r), 3), log = TRUE, nintLog = 5))

## Prove that we got it right :
plot(0:1, ylims, log = "y", yaxs = "i")
stopifnot(all.equal(aT.i, axTicks(side = 2)))

plot(0:1, ylims, log = "y", yaxs = "r")
stopifnot(all.equal(aT.r, axTicks(side = 2)))
}
\keyword{dplot}

