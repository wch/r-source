% File src/library/graphics/man/points.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{points}
\alias{points}
\alias{points.default}
\alias{pch}
\title{Add Points to a Plot}
\description{
  \code{points} is a generic function to draw a sequence of points at
  the specified coordinates.  The specified character(s) are plotted,
  centered at the coordinates.
}
\usage{
points(x, \dots)

\method{points}{default}(x, y = NULL, type = "p", \dots)
}
\arguments{
  \item{x, y}{coordinate vectors of points to plot.}
  \item{type}{character indicating the type of plotting; actually any of
    the \code{type}s as in \code{\link{plot.default}}.}
  \item{\dots}{Further graphical parameters may also be supplied as
    arguments.  See \sQuote{Details}.}
}
\details{
  The coordinates can be passed in a plotting structure
  (a list with \code{x} and \code{y} components), a two-column matrix, a
  time series, \dots.  See \code{\link{xy.coords}}.  If supplied
  separately, they must be of the same length.

  Graphical parameters commonly used are
  \describe{
    \item{\code{pch}}{plotting \sQuote{character}, i.e., symbol to use.
      This can either be a single character or an integer code for one
      of a set of graphics symbols.  The full set of S symbols is
      available with \code{pch=0:18}, see the examples below.
      % (currently, \R uses circles instead of octagons).

      Value \code{pch="."} (equivalently \code{pch = 46}) is handled
      specially.  It is a rectangle of side 0.01 inch (scaled by
      \code{cex}).  In addition, if \code{cex = 1} (the default), each
      side is at least one pixel (1/72 inch on the \code{\link{pdf}},
      \code{\link{postscript}} and \code{\link{xfig}} devices).

      For other text symbols, \code{cex = 1} corresponds to the default
      fontsize of the device, often specified by an argument
      \code{pointsize}.  For \code{pch} in \code{0:25} the default size
      is about 75\% of the character height (see \code{par("cin")}).
    }
    \item{\code{col}}{color code or name, see \code{\link{par}}.}
    \item{\code{bg}}{background (fill) color for the open plot
      symbols given by \code{pch=21:25}.}
    \item{\code{cex}}{character (or symbol) expansion: a numerical vector.
      This works as a multiple of \code{\link{par}("cex")}.}
    \item{\code{lwd}}{line width for drawing symbols see \code{\link{par}}.}
  }
  Others less commonly used are \code{lty} and \code{lwd} for
  types such as \code{"b"} and \code{"l"}.

  Graphical parameters \code{pch}, \code{col}, \code{bg}, \code{cex} and
  \code{lwd} can be vectors (which will be recycled as needed) giving a
  value for each point plotted.  If lines are to be plotted (e.g. for
  \code{type = "b"}/ the first element of \code{lwd} is used.

  Points whose \code{x}, \code{y}, \code{pch}, \code{col} or \code{cex}
  value is \code{NA} are omitted from the plot.
}
\section{'pch' values}{
  Values of \code{pch} are stored internally as integers.  The
  interpretation is
  \itemize{
    \item \code{NA_integer_}: no symbol.
    \item \code{0:18}: S-compatible vector symbols.
    \item \code{0:25}: \R vector symbols.
    \item \code{26:31}: unused (and ignored).
    \item \code{32:127}: ASCII characters.
    \item \code{128:255} native characters \emph{only in a
      single-byte locale and for the symbol font}.  (\code{128:159} are
    only used on Windows.)
    \item \code{-32 \dots} Unicode point (where supported).
  }
  Note that unlike S (which uses octagons), symbols \code{1}, \code{10},
  \code{13} and \code{16} use circles.  The filled shapes do not include
  a border.
  
  The following  \R plotting symbols are can be obtained with
  \code{pch=19:25}: those with \code{21:25} can be colored and
  filled with different colors: \code{col} gives the border color
  and \code{bg} the background color.
  \itemize{
    \item \code{pch=19}: solid circle,
    \item \code{pch=20}: bullet (smaller circle),
    \item \code{pch=21}: filled circle,
    \item \code{pch=22}: filled square,
    \item \code{pch=23}: filled diamond,
    \item \code{pch=24}: filled triangle point-up,
    \item \code{pch=25}: filled triangle point down.
  }
  Note that all of these both fill the shape and draw a border. Some
  care in interpretation is needed when semi-transparent colours are
  used for both fill and border (and the result might be device-specific
  and even viewer-specific for \code{\link{pdf}}).
      
  Values \code{pch=26:31} are currently unused and \code{pch=32:127}
  give the ASCII characters.  In a single-byte locale
  \code{pch=128:255} give the corresponding character (if any) in
  the locale's character set.  Where supported by the OS, negative
  values specify a Unicode point, so e.g. \code{-0x2642L}
  is a \sQuote{male sign} and \code{-0x20ACL} is the Euro.

  A character string consisting of a single character is converted to an
  integer: \code{32:127} for ASCII characters, and usually to the
  Unicode point number otherwise.  (In non-Latin-1 single-byte locales,
  \code{128:255} will be used for 8-bit characters.)

  If \code{pch} supplied is a logical, integer or character
  \code{NA} or an empty character string the point is omitted from
  the plot.

  If \code{pch} is \code{NULL} or otherwise of length 0,
  \code{par("pch")} is used.

  If the symbol font (\code{\link{par}(font = 5)}) is used, numerical
  values should be used for \code{pch}: the range is
  \code{c(32:126, 160:254)} in all locales (but \code{240} is not
  defined (used for \sQuote{apple} on Mac OS) and \code{160}, Euro, may
  not be present).
}
\note{
  A single-byte encoding may include the characters in
  \code{pch=128:255}, and if it does, a font may not include all (or
  even any) of them.

  Not all negative numbers are valid as Unicode points, and no check is
  done.  A display device is likely to use a rectangle for (or omit)
  Unicode points that do not exist or which it does not have a glyph.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{plot}}, \code{\link{lines}}, and the underlying
  workhorse function \code{\link{plot.xy}}.
}
\examples{
require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")# setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)

op <- par(bg = "light blue")
x <- seq(0,2*pi, len=51)
## something "between type='b' and type='o'":
plot(x, sin(x), type="o", pch=21, bg=par("bg"), col = "blue", cex=.6,
 main='plot(..., type="o", pch=21, bg=par("bg"))')
par(op)

##-------- Showing all the extra & some char graphics symbols ---------
pchShow <-
  function(extras = c("*",".", "o","O","0","+","-","|","\%","#"),
           cex = 3, ## good for both .Device=="postscript" and "x11"
           col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
           main = paste("plot symbols :  points (...  pch = *, cex =",
                        cex,")"))
  {
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch \%/\% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch \%\% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
         main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
      pc <- pch[[i]]
      ## 'col' symbols with a 'bg'-colored interior (where available) :
      points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
      if(cextext > 0)
          text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
  }

pchShow()
pchShow(c("o","O","0"), cex = 2.5)
pchShow(NULL, cex = 4, cextext = 0, main = NULL)

\donttest{
## ------------ test code for various pch specifications -------------
# Try this in various font families (including Hershey)
# and locales.  Use sign=-1 asserts we want Latin-1.
# Standard cases in a MBCS locale will not plot the top half.
TestChars <- function(sign=1, font=1, ...)
{
   if(font == 5) { sign <- 1; r <- c(32:126, 160:254)
   } else if (l10n_info()$MBCS) r <- 32:126 else r <- 32:255
   if (sign == -1) r <- c(32:126, 160:255)
   par(pty="s")
   plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="",
        xaxs="i", yaxs="i")
   grid(17, 17, lty=1)
   for(i in r) try(points(i\%\%16, i\%/\%16, pch=sign*i, font=font,...))
}
TestChars()
try(TestChars(sign=-1)) # needs MBCS support
TestChars(font=5) # Euro might be at 160.  Mac OS has apple at 240.
}}
\keyword{aplot}
