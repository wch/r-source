\name{postscript}
\title{PostScript Graphics}
\synopsis{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE, \dots)
ps.options(\dots, reset = FALSE, override.check = FALSE)
}
\usage{
postscript(file = ifelse(onefile, "Rplots.ps", "Rplot\%03d.ps"),
           onefile = TRUE,
           paper, family, bg, fg, width, height, horizontal, pointsize,
           pagecentre, print.it, command))

%  paper, horizontal, width, height, family, pointsize, bg, fg)
ps.options(paper, horizontal, width, height, family, pointsize, bg, fg,
           onefile = TRUE, print.it = FALSE, append = FALSE,
           reset = FALSE, override.check = FALSE)
.PostScript.Options
}
%% The definitive doc is the source :-)
%% 	../../../main/devices.c  &   ../../../unix/devPS.c
\description{
  \code{postscript} starts the graphics device driver for producing
  PostScript graphics.

  The auxiliary function \code{ps.options} can be used to set and view
  (if called without arguments)
  default values for the arguments to \code{postscript}.
}
\alias{postscript}
\alias{ps.options}
\alias{.PostScript.Options}
\arguments{
  \item{file}{a character string giving the name of the file.  If it is
    \code{""}, the output is piped to the command given by
    the argument \code{command}.
#ifdef unix
    If it is \code{"|cmd"}, the output is piped to the command given
    by \file{cmd}.
#endif

    For use with \code{onefile=FALSE} give a
    \code{printf} format such as \code{"Rplot\%d.fig"} (the default in that
    case).
  }
  \item{\dots}{further options for \code{postscript()} :}
  \item{paper}{the size of paper in the printer.  The choices are
    \code{"a4"}, \code{"letter"}, \code{"legal"} and
    \code{"executive"} (and these can be capitalized).
    Also, \code{"special"} can be used, when the \code{width}
    and \code{height} specify the paper size.  A further choice is
    \code{"default"}, which is the default.  If this is selected, the
    papersize is taken from the option \code{"papersize"}
    if that is set and to \code{"a4"} if it is unset or empty.}
  \item{horizontal}{the orientation of the printed image, a
    logical. Defaults to true, that is landscape orientation.}
  \item{width, height}{the width and height of the graphics region in inches.
    The default is to use the entire page less a 0.25 inch border.}
  \item{family}{the font family to be used.
    This must be one of \code{"AvantGarde"},
    \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
    \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
    \code{"Palatino"} or \code{"Times"}.}
  \item{pointsize}{the default point size to be used.}
  \item{bg}{the default background color to be used.}
  \item{fg}{the default foreground color to be used.}
  \item{onefile}{logical: if true (the default) allow multiple figures
    in one file. If false, generate a
    file number containing the page number and give EPSF header
    and no \code{DocumentMedia} comment.}
  \item{pagecentre}{logical: should the device region be centred on the page:
    defaults to true.}
  \item{print.it}{logical: should the file be printed when the device is
    closed?  (This only applies if \code{file} is a real file name.)}
  \item{command}{the command to be used for ``printing''. Defaults to
    option \code{"printcmd"}; this can also be selected as \code{"default"}.}
  \item{append}{logical; currently \bold{disregarded}; just there for
    compatibility reasons.}
}
\details{
  \code{postscript(..)} opens the file \code{file} and the PostScript
  commands needed to plot any graphics requested are stored in that file.
  This file can then be printed on a suitable device to obtain hard
  copy.

  A postscript plot can be printed via \code{postscript} in two ways.
  \enumerate{
    \item Setting \code{print.it = TRUE} causes the command given in
    argument \code{command} to be called with argument \code{"file"}
    when the device is closed.
    Note that the plot file is not deleted unless command arranges to
    delete it.

    \item \code{file=""} or \code{file="|cmd"} can be used to print
    using a pipe on systems that support \file{popen}.
  }
#ifdef windows
  Only the first of these will work on Windows, and the default
  \code{"printcmd"} is empty and will give an error if \code{print.it=TRUE}
  is used.  Suitable commands to spool a PostScript file to a printer
  can be found in \file{RedMon} suite available from
  \url{http://www.cs.wisc.edu/~ghost/rjl.html}.  The command will be run
  in a minimized window.
#endif

  The postscript produced by \R is EPS (\emph{Encapsulated PostScript})
  compatible, and can be included into other documents, e.g. into LaTeX,
  using \code{\includegraphics{<filename>}}. For use in this way you will
  probably want to set \code{horizontal=FALSE, onefile=FALSE, paper="special"}.
}
\seealso{
    \code{\link{Devices}},
    {\code{\link{check.options}} which is called from both
        \code{ps.options} and \code{postscript}}.
}
\examples{
\dontrun{
# open the file "foo.ps" for graphics output
postscript("foo.ps")
# produce the desired graph(s)
dev.off()              # turn off the postscript device
#ifdef unix
postscript("|lp -dlw")
# produce the desired graph(s)
dev.off()              # plot will appear on printer
#endif
#ifdef windows
options(printcmd='redpr -P"\\\\markov\\lw"')
postscript(file=tempfile("R.ps"), print.it=TRUE)
# produce the desired graph(s)
dev.off()              # send plot file to the printer
#endif
}
\testonly{
xx <- seq(0, 7, length=701)
yy <- sin(xx)/xx; yy[1] <- 1
plot(xx,yy)                     # produce the desired graph(s)
}

stopifnot(unlist(ps.options()) == unlist(.PostScript.Options))
ps.options(bg = "pink")
str(ps.options(reset = TRUE))

### --- error checking of arguments: ----
ps.options(width=0:12, onefile=0, bg=pi)
# override the check for 'onefile', but not the others:
str(ps.options(width=0:12, onefile=1, bg=pi, override.check = c(F,T,F)))
}
\keyword{device}
