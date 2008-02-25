% File src/library/grDevices/man/unix/png.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{cairo}
\alias{svg}
\alias{cairo_pdf}
\alias{cairo_ps}
\title{Cairo-based SVG, PDF and PostScript Graphics Devices}
\description{
  Graphics devices for SVG and bitmap PDF and PostScript graphics files.
}
\usage{
svg(filename = "Rplot\%03d.svg",
    width = 480, height = 480, units = "px", res = 72,
    pointsize = 12, bg = "white", antialias)

cairo_pdf(filename = "Rplot\%03d.pdf",
          width = 7, height = 7, pointsize = 12,
          bg = "white", antialias)

cairo_ps(filename = "Rplot\%03d.eps",
         width = 7, height = 7, pointsize = 12,
         bg = "white", antialias)
}
\arguments{
  \item{filename}{the name of the output file.
    The page number is substituted if a C integer format is included in
    the character string, as in the default.  (The result must be less
    than \code{PATH_MAX} characters long, and may be truncated if not.
    See \code{\link{postscript}} for further details.)  Tilde expansion
    is performed where supported by the platform.}
  \item{width}{the width of the device.}
  \item{height}{the height of the device.}
  \item{units}{The units in which \code{height} and \code{width} are
    given.  Can be \code{px} (pixels), \code{in} (inches),
    \code{cm} or \code{mm}.}
  \item{res}{The resolution used to convert points and other
    device-size units to pixels.}
  \item{pointsize}{the default pointsize of plotted text.}
  \item{bg}{the initial background colour: can be overridden by setting
    par("bg").}
  \item{antialias}{the type of anti-aliasing (if any) to be used.
    See \code{\link{X11}}.}
}
\details{
  SVG (Scalar Vector Graphics) is a W3C standard for vector graphics.
  See \url{http://www.w3.org/Graphics/SVG/}.  Cairo currently produces
  SVG version 1.1.
  
  Note that unlike \code{\link{postscript}} and \code{\link{pdf}},
  \code{cairo_pdf} and \code{cairo_ps} record \emph{bitmaps} and not
  vector graphics.  This can occasionally be useful (bitmaps of very
  complex plots can be more compact, and a much wider range of
  characters can be rendered), but the more usual vector format is
  normally much preferred.  A resolution of 72dpi is used.
  
  \R can be compiled without support for any of these
  devices: this will be reported if you attempt to use them on a system
  where they are not supported.
  
  If you plot more than one page on one of these devices and do not
  include something like \code{\%d} for the sequence number in
  \code{file}, the file will contain the last page plotted.
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}


\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}, \code{\link{pdf}},
  \code{\link{postscript}}

  \code{\link{capabilities}} to see if Cairo is supported.
}

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is in pixels (\code{svg}) or inches..
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths in 1/96 inch..
    \item Circle radii have a minumum of 1/72 inch.
    \item Colours are interpreted by the viewing application.
  }
}

\keyword{device}
