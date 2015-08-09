% File src/library/graphics/man/curve.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{curve}
\title{Draw Function Plots}
\alias{curve}
\alias{plot.function}

\description{
  Draws a curve corresponding to a function over the interval
  \code{[from, to]}. \code{curve} can plot also an expression in the variable
  \code{xname}, default \samp{x}.
}

\usage{
curve(expr, from = NULL, to = NULL, n = 101, add = FALSE,
      type = "l", xname = "x", xlab = xname, ylab = NULL,
      log = NULL, xlim = NULL, \dots)

\method{plot}{function}(x, y = 0, to = 1, from = y, xlim = NULL, ylab = NULL, \dots)
}

\arguments{
  \item{expr}{The name of a function, or a \link{call} or an
    \link{expression} written as a function of \code{x} which will
    evaluate to an object of the same length as \code{x}.}
  \item{x}{a \sQuote{vectorizing} numeric \R function.}
  \item{y}{alias for \code{from} for compatibility with \code{plot}}
  \item{from, to}{the range over which the function will be plotted.}
  \item{n}{integer; the number of x values at which to evaluate.}
  \item{add}{logical; if \code{TRUE} add to an already existing plot; if
    \code{NA} start a new plot taking the defaults for the limits and
    log-scaling of the x-axis from the previous plot.  Taken as
    \code{FALSE} (with a warning if a different value is supplied) if no
    graphics device is open.}
  \item{xlim}{\code{NULL} or a numeric vector of length 2;
    if non-\code{NULL} it provides the defaults for \code{c(from, to)}
    and, unless \code{add = TRUE}, selects the x-limits of the plot -- see
    \code{\link{plot.window}}.}
  \item{type}{plot type: see \code{\link{plot.default}}.}
  \item{xname}{character string giving the name to be used for the x axis.}
  \item{xlab, ylab, log, \dots}{labels and \link{graphical parameters}
    can also be specified as arguments.  See \sQuote{Details} for the
    interpretation of the default for \code{log}.

    For the \code{"function"} method of \code{plot}, \code{\dots} can
    include any of the other arguments of \code{curve}, except
    \code{expr}.
  }
}

\details{
  The function or expression \code{expr} (for \code{curve}) or function
  \code{x} (for \code{plot}) is evaluated at \code{n} points equally
  spaced over the range \code{[from, to]}.  The points determined in
  this way are then plotted.

  If either \code{from} or \code{to} is \code{NULL}, it defaults to the
  corresponding element of \code{xlim} if that is not \code{NULL}.

  What happens when neither \code{from}/\code{to} nor \code{xlim}
  specifies both x-limits is a complex story.  For
  \code{plot(<function>)} and for \code{curve(add = FALSE)} the defaults
  are \eqn{(0, 1)}.  For \code{curve(add = NA)} and \code{curve(add =
  TRUE)} the defaults are taken from the x-limits used for the previous
  plot.  (This differs from versions of \R prior to 2.14.0.)

  The value of \code{log} is used both to specify the plot axes (unless
  \code{add = TRUE}) and how \sQuote{equally spaced} is interpreted: if
  the x component indicates log-scaling, the points at which the
  expression or function is plotted are equally spaced on log scale.

  The default value of \code{log} is taken from the current plot when
  \code{add = TRUE}, whereas if \code{add = NA} the x component is taken
  from the existing plot (if any) and the y component defaults to
  linear.  For \code{add = FALSE} the default is \code{""}

  This used to be a quick hack which now seems to serve a useful purpose,
  but can give bad results for functions which are not smooth.

  For expensive-to-compute \code{expr}essions, you should use smarter tools.

  The way \code{curve} handles \code{expr} has caused confusion.  It
  first looks to see if \code{expr} is a \link{name} (also known as a
  symbol), in which case it is taken to be the name of a function, and
  \code{expr} is replaced by a call to \code{expr} with a single
  argument with name given by \code{xname}.  Otherwise it checks that
  \code{expr} is either a \link{call} or an \link{expression}, and that
  it contains a reference to the variable given by \code{xname} (using
  \code{\link{all.vars}}): anything else is an error.  Then \code{expr}
  is evaluated in an environment which supplies a vector of name given
  by \code{xname} of length \code{n}, and should evaluate to an object
  of length \code{n}.  Note that this means that \code{curve(x, ...)} is
  taken as a request to plot a function named \code{x} (and it is used
  as such in the \code{function} method for \code{plot}).

  The \code{plot} method can be called directly as \code{plot.function}.
}

\value{
  A list with components \code{x} and \code{y} of the points that were
  drawn is returned invisibly.
}

\section{Warning}{
  For historical reasons, \code{add} is allowed as an argument to the
  \code{"function"} method of \code{plot}, but its behaviour may surprise
  you.  It is recommended to use \code{add} only with \code{curve}.
}

\seealso{
  \code{\link{splinefun}} for spline interpolation, \code{\link{lines}}.
}
\examples{
plot(qnorm) # default range c(0, 1) is appropriate here,
            # but end values are -/+Inf and so are omitted.
plot(qlogis, main = "The Inverse Logit : qlogis()")
abline(h = 0, v = 0:2/2, lty = 3, col = "gray")

curve(sin, -2*pi, 2*pi, xname = "t")
curve(tan, xname = "t", add = NA,
      main = "curve(tan)  --> same x-scale as previous plot")

op <- par(mfrow = c(2, 2))
curve(x^3 - 3*x, -2, 2)
curve(x^2 - 2, add = TRUE, col = "violet")

## simple and advanced versions, quite similar:
plot(cos, -pi,  3*pi)
curve(cos, xlim = c(-pi, 3*pi), n = 1001, col = "blue", add = TRUE)

chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n = 2001)
plot (chippy, -8, -5)

for(ll in c("", "x", "y", "xy"))
   curve(log(1+x), 1, 100, log = ll, sub = paste0("log = '", ll, "'"))
par(op)
}
\keyword{hplot}
