\name{plot.lm}
\alias{plot.lm}
\title{Plot Diagnostics for an "lm" Object}
\usage{
plot.lm (x, which = 1:4,
         caption = c("Residuals vs Fitted", "Normal Q-Q plot",
                     "Scale-Location plot", "Cook's distance plot"),
         panel = points,
         sub.caption = deparse(x$call), main = "",
         ask = interactive() && one.fig && .Device != "postscript",
         ...,
         id.n = 3, labels.id = names(residuals(x)), cex.id = 0.25)
}
\arguments{
    \item{x}{\code{lm} object, typically result of \code{\link{lm}} or
	\code{\link{glm}}.}
    \item{which}{If a subset of the plots is required, specify a subset
	of the numbers \code{1:4}.}
    \item{caption}{Captions to appear above the plots}
    \item{panel}{Panel function.  A useful alternative to \code{\link{points}}
	is \code{\link{panel.smooth}}.}
    \item{sub.caption}{common title -- above figures if there are
	multiple; used as \code{sub} (s.\code{\link{title}}) otherwise.}
    \item{main}{title to each plot -- in addition to the above \code{caption}.}
    \item{ask}{logical; if \code{TRUE}, the user is \emph{ask}ed before
	each plot, see \code{\link{par}(ask=.)}.}
    \item{\dots}{other parameters to be passed through to plotting functions.}
    \item{id.n}{number of points to be labelled in each plot, starting with
	the most extreme.}
    \item{labels.id}{vector of labels, from which the labels for extreme
	points will be chosen.  \code{NULL} uses observation numbers.}
    \item{cex.id}{magnification of point labels.}
}
\description{
    Four plots (choosable by \code{which}) are currently provided: a
    plot of residuals against fitted values, a Scale-Location plot of
    \eqn{\sqrt{| residuals |}} against fitted values, a Normal Q-Q plot,
    and a plot of Cook's distances versus row labels.
}
\details{\code{sub.caption}---by default the function call---is shown as
    a subtitle (under the x-axis title) on each plot when plots are on
    separate pages, or as a subtitle in the outer margin (if any) when
    there are multiple plots per page.

    The ``Scale-Location'' plot, also called ``Spread-Location'' or
    ``S-L'' plot, takes the square root of the absolute residuals in
    order to diminish skewness (\eqn{\sqrt{| E |}} is much less skewed
    than \eqn{| E |} for Gaussian zero-mean \eqn{E}).

    This `S-L' and the Q-Q plot use \emph{standardized} residuals which
    have identical variance (under the hypothesis).  They are given as
    \eqn{R_i / (s \times \sqrt{1 - h_{ii}})}{R[i] / (s*sqrt(1 - h.ii))}
    where \eqn{h_{ii}}{h.ii} are the diagonal entries of the hat matrix,
% bug in Rdconv: "$" and \link inside \code fails; '\$' doesn't help...
    \code{\link{lm.influence}()}\code{$hat}, see also \code{\link{hat}}.
}
\references{
    Belsley, D. A., E. Kuh and R. E. Welsch (1980).
    \emph{Regression Diagnostics}.  New York: Wiley.

    Cook, R. D. and S. Weisberg (1982).
    \emph{Residuals and Influence in Regression}.
    London: Chapman and Hall.

    Hinkley, D. V. (1975). On power transformations to
    symmetry. \emph{Biometrika} \bold{62}: 101-111.

    McCullagh, P. and Nelder, J. A. (1989).
    \emph{Generalized Linear Models}.
    London: Chapman and Hall.
}
\author{John Maindonald and Martin Maechler.
}
\seealso{
    \code{\link{lm.influence}}, \code{\link{cooks.distance}}
}

\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
data(LifeCycleSavings)
plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings))

## 4 plots on 1 page; allow room for printing model formula in outer margin:
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm.SR)
plot(lm.SR, id.n = NULL)               # no id's
plot(lm.SR, id.n = 5, labels.id = NULL)# 5 id numbers

## Fit a smmooth curve, where applicable:
plot(lm.SR, panel = panel.smooth)
## Gives a smoother curve
plot(lm.SR, panel = function(x,y) panel.smooth(x, y, span = 1))
## Warnings:  panel = panel.smoth, span = 1

par(mfrow=c(2,1))# same oma as above
plot(lm.SR, which = 1:2, sub.caption = "Saving Rates, n=50, p=5")
}
\keyword{hplot}
\keyword{regression}
