% File src/library/base/man/capabilities.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{capabilities}
\alias{capabilities}
\title{Report Capabilities of this Build of R}
\description{
  Report on the optional features which have been compiled into this
  build of \R.
}
\usage{
capabilities(what = NULL)
}
\arguments{
  \item{what}{character vector or \code{NULL}, specifying
    required components.  \code{NULL} implies that all are required.}
}
#ifdef unix
\section{Note to OS X users}{
  Capabilities \code{"jpeg"}, \code{"png"} and \code{"tiff"} refer to
  the X11-based versions of these devices.  If
  \code{capabilities("aqua")} is true, then these devices with
  \code{type = "quartz"} will be available, and out-of-the-box will be the
  default type.  Thus for example the \code{\link{tiff}} device will be
  available if \code{capabilities("aqua") || capabilities("tiff")} if
  the defaults are unchanged.
}
#endif
\value{
  A named logical vector.  Current components are
  \item{jpeg}{is the \code{\link{jpeg}} function operational?}
  \item{png}{is the \code{\link{png}} function operational?}
  \item{tiff}{is the \code{\link{tiff}} function operational?}
  \item{tcltk}{is the \pkg{tcltk} package operational?
#ifdef unix
    Note that to make use of Tk you will almost always need to check
    that \code{"X11"} is also available.
#endif
  }
#ifdef unix
  \item{X11}{are the \code{\link{X11}} graphics device and the
    X11-based data editor available?  This loads the X11 module if not
    already loaded, and checks that the default display can be
    contacted unless a \code{X11} device has already been used.}

  \item{aqua}{is the \code{\link{quartz}} function operational?
    Only on some OS X builds, including \acronym{CRAN} binary
    distributions of \R.

    Note that this is distinct from \code{.Platform$GUI == "AQUA"},
    which is true only when using the Mac \code{R.app} GUI console.}
#endif
#ifdef windows
  \item{X11}{always \code{FALSE} on Windows.}
  \item{aqua}{\code{FALSE} except on OS X.}
#endif
  \item{http/ftp}{does the internal method for \code{\link{url}} and
    \code{\link{download.file}} support \samp{http://} and \samp{ftp://}
    URLs?  Always \code{TRUE} as from \R 3.3.0.}
  \item{sockets}{are  \code{\link{make.socket}} and related functions
    available?  Always \code{TRUE} as from \R 3.3.0.}
  \item{libxml}{is there support for integrating \code{libxml} with
    the \R event loop?  Always \code{TRUE} as from \R 3.3.0.}
  \item{fifo}{are FIFO \link{connections} supported?}
  \item{cledit}{is command-line editing available in the current \R
    session?  This is false in non-interactive sessions.
#ifdef unix
    It will be true for the command-line interface if \code{readline}
    support has been compiled in and \option{--no-readline} was
    \emph{not} used when \R was invoked.  (If \option{--interactive}
    was used, command-line editing will not actually be available.)
#endif
  }
  \item{iconv}{is internationalization conversion via
    \code{\link{iconv}} supported?  Always true in current \R.}
  \item{NLS}{is there Natural Language Support (for message translations)?}
  \item{profmem}{is there support for memory profiling?  See
    \code{\link{tracemem}}.}
  \item{cairo}{is there support for the \code{\link{svg}},
    \code{\link{cairo_pdf}} and \code{\link{cairo_ps}} devices, and
    for \code{type = "cairo"} in the
#ifdef unix
    \code{\link{X11}},
#endif
    \code{\link{bmp}}, \code{\link{jpeg}}, \code{\link{png}},
    and \code{\link{tiff}} devices?}

  \item{ICU}{is ICU available for collation?  See the help on
    \link{Comparison} and \code{\link{icuSetCollate}}: it is never
    used for a C locale.}
    
  \item{long.double}{does this build use a \code{C} \code{long double}
    type which is longer than \code{double}?  Some platforms do not
    have such a type, and on others its use can be suppressed by the
    configure option \option{--disable-long-double}.
    
    Although not guaranteed, it is a reasonable assumption that if
    present long doubles will have at least as much range and accuracy
    as the ISO/IEC 60559 80-bit \sQuote{extended precision} format.
  }

  \item{libcurl}{is \code{libcurl} available in this build?  Used by
    function \code{\link{curlGetHeaders}} and optionally by
    \code{\link{download.file}} and \code{\link{url}}.   As from \R
    3.3.0 always true for Unix-alikes, and true for CRAN Windows builds.}
}
\seealso{\code{\link{.Platform}}}

\examples{
capabilities()

if(!capabilities("ICU"))
   warning("ICU is not available")

## See also the examples for 'connections'.
}
\keyword{utilities}
