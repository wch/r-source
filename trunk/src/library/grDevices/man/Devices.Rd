\name{Devices}
\title{List of Graphical Devices}
\alias{Devices}
\alias{device}
% \usage{
% #ifdef unix
% X11(\dots)
% #endif
% #ifdef windows
% windows(\dots)
% #endif
% %% FIXME:
% #ifdef aqua
% quartz(\dots)
% #endif
% postscript(\dots)
% pdf(\dots)
% pictex(\dots)
% #ifdef unix
% png(\dots)
% jpeg(\dots)
% GNOME(\dots)
% #endif
% #ifdef windows
% win.metafile(\dots)
% win.print(\dots)
% png(\dots)
% jpeg(\dots)
% bmp(\dots)
% #endif
% xfig(\dots)
% bitmap(\dots)
% }
\description{The following graphics devices are currently available:
  \itemize{
#ifdef windows
    \item \code{\link{windows}} The graphics driver for Windows (on screen,
    to printer and to Windows metafile).
#endif
    \item \code{\link{postscript}} Writes PostScript graphics commands to
    a file
    \item \code{\link{pdf}} Write PDF graphics commands to a file
    \item \code{\link{pictex}} Writes LaTeX/PicTeX graphics commands to a
    file
#ifdef windows
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
    \item \code{\link{bmp}} BMP bitmap device
#endif
    \item \code{\link{xfig}} Device for XFIG graphics file format
    \item \code{\link{bitmap}} bitmap pseudo-device via
    \code{GhostScript} (if available).
  }
#ifdef unix
  The following devices will be available if \R was compiled to use them:
  \itemize{
    \item \code{\link{X11}} The graphics driver for the X11 Window
    system
    \item \code{\link{png}} PNG bitmap device
    \item \code{\link{jpeg}} JPEG bitmap device
%% <FIXME>  quartz() also with corresponding --gui aqua ?
#ifdef aqua
%%     ^^^^ not yet available
    \item \code{\link{quartz}} The graphics driver for the Mac OSX
    native aqua system.
#endif
%% </FIXME>
  }
#endif
}
\details{
  If no device is open, using a high-level graphics function will cause
  a device to be opened.  Which device is given by
  \code{\link{options}("device")} which is initially set as the most
  appropriate for each platform: a screen device for most interactive use and
  \code{\link{postscript}} otherwise.  The exception is interactive
  use under Unix if no screen device is known to be available, when
  \code{postscript()} is used for most systems; \code{pdf()} for Mac OS X.
}
\seealso{
  The individual help files for further information on any of the
  devices listed here;

  \code{\link{dev.interactive}},
  \code{\link{dev.cur}}, \code{\link{dev.print}},
  \code{\link{graphics.off}}, \code{\link{image}},
  \code{\link{dev2bitmap}}.
#ifdef unix

  \code{\link{capabilities}} to see if \code{\link{X11}}, \code{\link{jpeg}}
  and \code{\link{png}} are available.
%% FIXME: aqua / quartz ?
#endif
}
\examples{\dontrun{
## open the default screen device on this platform if no device is
## open
if(dev.cur() == 1) get(getOption("device"))()
}}
\keyword{device}
