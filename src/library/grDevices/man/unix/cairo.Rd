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
  Graphics devices for SVG, PDF and PostScript graphics files.
}
\usage{
svg(filename = if(onefile) "Rplots.svg" else "Rplot\%03d.svg",
    width = 7, height = 7, pointsize = 12,
    onefile = FALSE, bg = "white", antialias)

cairo_pdf(filename = if(onefile) "Rplots.pdf" else "Rplot\%03d.pdf",
          width = 7, height = 7, pointsize = 12,
          onefile = FALSE, bg = "white", antialias)

cairo_ps(filename = if(onefile) "Rplots.ps" else "Rplot\%03d.ps",
         width = 7, height = 7, pointsize = 12,
         onefile = FALSE, bg = "white", antialias)
}
\arguments{
  \item{filename}{the name of the output file.
    The page number is substituted if a C integer format is included in
    the character string, as in the default.  (The result must be less
    than \code{PATH_MAX} characters long, and may be truncated if not.
    See \code{\link{postscript}} for further details.)  Tilde expansion
    is performed where supported by the platform.}
  \item{width}{the width of the device in inches.}
  \item{height}{the height of the device in inches.}
  \item{pointsize}{the default pointsize of plotted text (in big points).}
  \item{onefile}{should all plots appear in one file or in separate files?}
  \item{bg}{the initial background colour: can be overridden by setting
    par("bg").}
  \item{antialias}{the type of anti-aliasing (if any) to be used.
    See \code{\link{X11}}.}
}
\details{
  SVG (Scalar Vector Graphics) is a W3C standard for vector graphics.
  See \url{http://www.w3.org/Graphics/SVG/}.  The output is SVG version
  1.1 for \code{onefile = FALSE} (the default), otherwise SVG 1.2.  (Very few
  SVG viewers are capable of displaying multi-page SVG files.)
  
  Note that unlike \code{\link{postscript}} and \code{\link{pdf}},
  \code{cairo_pdf} and \code{cairo_ps} sometimes record \emph{bitmaps}
  and not vector graphics: a resolution of 72dpi is used.  On the other
  hand, they can (on suitable platforms) include a much wider range of
  UTF-8 glyphs, and embed the fonts used.  They are somewhat experimental.
  
  \R can be compiled without support for any of these
  devices: this will be reported if you attempt to use them on a system
  where they are not supported.  They all require cairo version 1.2 or
  later.
  
  If you plot more than one page on one of these devices and do not
  include something like \code{\%d} for the sequence number in
  \code{file} (or set \code{onefile=TRUE}) the file will contain the
  last page plotted.

  %http://lwn.net/Articles/256590/
  The \code{cairo_ps} output is not yet encapsulated (that is coming in
  cairo 1.6).

  There is full support of transparency, but using this is one of the
  things liable to trigger bitmap output (and will always do so for
  \code{cairo_ps}).
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}


\seealso{
  \code{\link{Devices}}, \code{\link{dev.print}}, \code{\link{pdf}},
  \code{\link{postscript}}

  \code{\link{capabilities}} to see if cairo is supported.
}

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is in pixels (\code{svg}) or inches.
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths are multiples of 1/96 inch.
    \item Circle radii have a minumum of 1/72 inch.
    \item Colours are interpreted by the viewing application.
  }
}

\keyword{device}
