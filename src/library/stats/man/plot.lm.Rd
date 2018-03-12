% File src/library/stats/man/plot.lm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{plot.lm}
\alias{plot.lm}
% \alias{plot.mlm} %which is .NotYetImplemented()
\title{Plot Diagnostics for an lm Object}
\usage{
\method{plot}{lm}(x, which = c(1:3, 5), %% was which = 1:4
     caption = list("Residuals vs Fitted", "Normal Q-Q",
       "Scale-Location", "Cook's distance",
       "Residuals vs Leverage",
       expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))),
     panel = if(add.smooth) panel.smooth else points,
     sub.caption = NULL, main = "",
     ask = prod(par("mfcol")) < length(which) && dev.interactive(),
     \dots,
     id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
     qqline = TRUE, cook.levels = c(0.5, 1.0),
     add.smooth = getOption("add.smooth"), label.pos = c(4,2),
     cex.caption = 1, cex.oma.main = 1.25)
}
\arguments{
  \item{x}{\code{lm} object, typically result of \code{\link{lm}} or
    \code{\link{glm}}.}
  \item{which}{if a subset of the plots is required, specify a subset of
    the numbers \code{1:6}, see \code{caption} below (and the
    \sQuote{Details}) for the different kinds.}
  \item{caption}{captions to appear above the plots;
    \code{\link{character}} vector or \code{\link{list}} of valid
    graphics annotations, see \code{\link{as.graphicsAnnot}}, of length
    6, the j-th entry corresponding to \code{which[j]}.  Can be set to
    \code{""} or \code{NA} to suppress all captions.
  }
  \item{panel}{panel function.  The useful alternative to
    \code{\link{points}}, \code{\link{panel.smooth}} can be chosen
    by \code{add.smooth = TRUE}.}
  \item{sub.caption}{common title---above the figures if there are more
    than one; used as \code{sub} (s.\code{\link{title}}) otherwise.  If
    \code{NULL}, as by default, a possible abbreviated version of
    \code{deparse(x$call)} is used.}
  \item{main}{title to each plot---in addition to \code{caption}.}
  \item{ask}{logical; if \code{TRUE}, the user is \emph{ask}ed before
    each plot, see \code{\link{par}(ask=.)}.}
  \item{\dots}{other parameters to be passed through to plotting
    functions.}
  \item{id.n}{number of points to be labelled in each plot, starting
    with the most extreme.}
  \item{labels.id}{vector of labels, from which the labels for extreme
    points will be chosen.  \code{NULL} uses observation numbers.}
  \item{cex.id}{magnification of point labels.}
  \item{qqline}{logical indicating if a \code{\link{qqline}()} should be
    added to the normal Q-Q plot.}
  \item{cook.levels}{levels of Cook's distance at which to draw contours.}
  \item{add.smooth}{logical indicating if a smoother should be added to
    most plots; see also \code{panel} above.}
  \item{label.pos}{positioning of labels, for the left half and right
    half of the graph respectively, for plots 1-3.}
  \item{cex.caption}{controls the size of \code{caption}.}
  \item{cex.oma.main}{controls the size of the \code{sub.caption} only if
    that is \emph{above} the figures when there is more than one.}
}
\description{
  Six plots (selectable by \code{which}) are currently available: a plot
  of residuals against fitted values, a Scale-Location plot of
  \eqn{\sqrt{| residuals |}}{sqrt(| residuals |)}
  against fitted values, a Normal Q-Q plot, a
  plot of Cook's distances versus row labels, a plot of residuals
  against leverages, and a plot of Cook's distances against
  leverage/(1-leverage).  By default, the first three and \code{5} are
  provided.
}
\details{
  \code{sub.caption}---by default the function call---is shown as
  a subtitle (under the x-axis title) on each plot when plots are on
  separate pages, or as a subtitle in the outer margin (if any) when
  there are multiple plots per page.

  The \sQuote{Scale-Location} plot, also called \sQuote{Spread-Location} or
  \sQuote{S-L} plot, takes the square root of the absolute residuals in
  order to diminish skewness (\eqn{\sqrt{| E |}}{sqrt(|E|)} is much less skewed
  than \eqn{| E |} for Gaussian zero-mean \eqn{E}).

  The \sQuote{S-L}, the Q-Q, and the Residual-Leverage plot, use
  \emph{standardized} residuals which have identical variance (under the
  hypothesis).  They are given as
  \eqn{R_i / (s \times \sqrt{1 - h_{ii}})}{R[i] / (s * sqrt(1 - h.ii))}
  where \eqn{h_{ii}}{h.ii} are the diagonal entries of the hat matrix,
  \code{\link{influence}()$hat} (see also \code{\link{hat}}), and
  where the Residual-Leverage plot uses standardized Pearson residuals
  (\code{\link{residuals.glm}(type = "pearson")}) for \eqn{R[i]}.

  The Residual-Leverage plot shows contours of equal Cook's distance,
  for values of \code{cook.levels} (by default 0.5 and 1) and omits
  cases with leverage one with a warning.  If the leverages are constant
  (as is typically the case in a balanced \code{\link{aov}} situation)
  the plot uses factor level combinations instead of the leverages for
  the x-axis.  (The factor levels are ordered by mean fitted value.)

  In the Cook's distance vs leverage/(1-leverage) plot, contours of
  standardized residuals (\code{\link{rstandard}(.)}) that are equal in
  magnitude are lines through the origin.  The contour lines are
  labelled with the magnitudes.
}
\references{
  Belsley, D. A., Kuh, E. and Welsch, R. E. (1980).
  \emph{Regression Diagnostics}.
  New York: Wiley.

  Cook, R. D. and Weisberg, S. (1982).
  \emph{Residuals and Influence in Regression}.
  London: Chapman and Hall.

  Firth, D. (1991) Generalized Linear Models.
  In Hinkley, D. V. and Reid, N. and Snell, E. J., eds:
  Pp.\sspace{}55-82 in Statistical Theory and Modelling.
  In Honour of Sir David Cox, FRS.
  London: Chapman and Hall.

  Hinkley, D. V. (1975).
  On power transformations to symmetry.
  \emph{Biometrika}, \bold{62}, 101--111.
  \doi{10.2307/2334491}.

  McCullagh, P. and Nelder, J. A. (1989).
  \emph{Generalized Linear Models}.
  London: Chapman and Hall.
}
\author{
  John Maindonald and Martin Maechler.
}
\seealso{\code{\link{termplot}}, \code{\link{lm.influence}},
  \code{\link{cooks.distance}}, \code{\link{hatvalues}}.
}
\examples{
require(graphics)

## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
plot(lm.SR)

## 4 plots on 1 page;
## allow room for printing model formula in outer margin:
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm.SR)
plot(lm.SR, id.n = NULL)                 # no id's
plot(lm.SR, id.n = 5, labels.id = NULL)  # 5 id numbers

## Was default in R <= 2.1.x:
## Cook's distances instead of Residual-Leverage plot
plot(lm.SR, which = 1:4)

## Fit a smooth curve, where applicable:
plot(lm.SR, panel = panel.smooth)
## Gives a smoother curve
plot(lm.SR, panel = function(x, y) panel.smooth(x, y, span = 1))

par(mfrow = c(2,1))  # same oma as above
plot(lm.SR, which = 1:2, sub.caption = "Saving Rates, n=50, p=5")

\dontshow{
## An example with *long* formula that needs abbreviation:
for(i in 1:5) assign(paste("long.var.name", i, sep = "."), runif(10))
plot(lm(long.var.name.1 ~
        long.var.name.2 + long.var.name.3 + long.var.name.4 + long.var.name.5))
}
}
\keyword{hplot}
\keyword{regression}
