\name{par}
\alias{par}
\alias{.Pars}
\alias{.Pars.readonly}
\title{Set or Query Graphical Parameters}
\usage{
par(\dots, no.readonly = FALSE)

<highlevel plot> (\dots, <tag> = <value>)
}
\description{
  % \code{par} is an abomination before man and god.
  \code{par} can be used to set or query graphical parameters.
  Parameters can be set by specifying them as arguments to \code{par} in
  \code{tag = value} form, or by passing them as a list of tagged values.
}
\details{
  The former values are returned in an invisible tagged list.  Such a
  list can be passed as an argument to \code{par} to restore the
  parameter values.  Use \code{par(no.readonly = TRUE)} for the full
  list in that case.

  Parameters are queried by giving vectors of character strings to
  \code{par}.  In this case, the values are returned in a tagged list.

  \code{par()} (no arguments) or \code{par(no.readonly=TRUE)} is used to
  get \emph{all} the graphical parameters (as named list).  Their names
  are currently taken from the variable \code{.Pars}.
  \code{.Pars.readonly} contains the names of the \code{par} arguments
  which are \emph{readonly}.

  \emph{\bold{R.O.}} Arguments := \bold{Read-only arguments}:
  These may only be used in queries, i.e.,  they do \emph{not} set anything.

  All but these \emph{\bold{R.O.}} and the following \bold{low-level
    arguments} can be set as well in high-level and mid-level plot
  functions, such as \code{\link{plot}}, \code{\link{points}},
  \code{\link{lines}}, \code{\link{axis}}, \code{\link{title}},
  \code{\link{text}}, \code{\link{mtext}}:
  \itemize{
    \item \code{"ask"}
    \item \code{"fig"}, \code{"fin"}
    \item \code{"mai"}, \code{"mar"}, \code{"mex"}
    \item \code{"mfrow"}, \code{"mfcol"}, \code{"mfg"}
    \item \code{"new"}
    \item \code{"oma"}, \code{"omd"}, \code{"omi"}
    \item \code{"pin"}, \code{"plt"}, \code{"ps"}, \code{"pty"}
    \item \code{"usr"}
    \item \code{"xlog"}, \code{"ylog"}
  }
}
%% --- The real documentation is the code -- in ../../../main/par.c   ;-)
%% --- AND (defined AND commented) in ../../../include/Graphics.h
\arguments{
  \item{no.readonly}{logical; if \code{TRUE} and there are no other
    arguments,  only parameters are returned which can be set by a
    subsequent \code{par(.)} call.}
  \item{adj}{The value of \code{adj} determines the way in which text
    strings are justified.  A value of \code{0} produces
    left-justified text, \code{0.5} centered text and \code{1}
    right-justified text.  Note that the \code{adj} argument of
    \code{\link{text}} and \code{\link{mtext}} also allows
    \code{adj= c(x,y)} for different adjustment in x- and y- direction.}
  \item{ann}{If set to \code{FALSE}, high-level plotting functions do
    not annotate the plots they produce with axis and overall titles.
    The default is to do annotation.}
  \item{ask}{logical. If \code{TRUE}, the user is asked for input,
    before a new figure is drawn.}
  \item{bg}{The color to be used for the background of plots.  A
    description of how colors are specified is given below.}
  \item{bty}{A character string which determined the type of box which
    is drawn about plots.  If \code{bty} is one of \code{"o"},
    \code{"l"}, \code{"7"}, \code{"c"}, or \code{"]"} the resulting box
    resembles the corresponding upper case letter.  A value of
    \code{"n"} suppresses the box.}
  \item{cex}{A numerical value giving the amount by which plotting text
    and symbols should be scaled relative to the default.}
  \item{cex.axis}{The magnification to be used for axis annotation
    relative to the current.}
  \item{cex.lab}{The magnification to be used for x and y labels
    relative to the current.}
  \item{cex.main}{The magnification to be used for main titles relative
    to the current.}
  \item{cex.sub}{The magnification to be used for sub-titles relative to
    the current.}
  \item{cin}{\emph{\bold{R.O.}}; character size \code{(width,height)} in
  inches.}
  \item{col}{A specification for the default plotting color.  A
    description of how colors are specified is given below.}
  \item{col.axis}{The color to be used for axis annotation.}
  \item{col.lab}{The color to be used for x and y labels.}
  \item{col.main}{The color to be used for plot main titles.}
  \item{col.sub}{The color to be used for plot sub-titles.}
  \item{cra}{\emph{\bold{R.O.}}; size of default character
    \code{(width,height)} in ``rasters'' (pixels).}
  \item{crt}{A numerical value specifying (in degrees) how single characters
    should be rotated.  It is unwise to expect values other than
    multiples of 90 to work.  Compare with \code{srt} which does string
    rotation.}
  \item{csi}{\emph{\bold{R.O.}}. The height of (default sized)
      characters in inches.}
  \item{cxy}{\emph{\bold{R.O.}}; size of default character
    \code{(width,height)} in user coordinate units.
    \code{par("cxy")} is \code{par("cin")/par("pin")} scaled to user
    coordinates.
    Note that \code{c(\link{strwidth}(ch),\link{strheight}(ch))} for a
    given string \code{ch} is usually much more precise.}
  \item{din}{\emph{\bold{R.O.}}. The device dimensions in inches.}
  \item{err}{(\emph{Unimplemented}; \R is silent when
    points outside the plot region are \emph{not} plotted.)
    The degree of error reporting desired.}
  \item{fg}{The color to be used for the foreground of plots.
    This is the default color is used for things like axes and boxes
    around plots.  A description of how colors are specified is given
    below.}
  \item{fig}{A numerical vector of the form \code{c(x1, x2, y1, y2)}
    which gives the (NDC) coordinates of the figure region in the
    display region of the device.}
  \item{fin}{A numerical vector of the form \code{c(x, y)} which gives
    the size of the figure region in inches.}
  \item{font}{An integer which specifies which font to use for text.
    If possible, device drivers arrange so that 1 corresponds to plain
    text, 2 to bold face, 3 to italic and 4 to bold italic.}
  \item{font.axis}{The font to be used for axis annotation.}
  \item{font.lab}{The font to be used for x and y labels.}
  \item{font.main}{The font to be used for plot main titles.}
  \item{font.sub}{The font to be used for plot sub-titles.}
  \item{gamma}{the gamma correction, see \code{\link{hsv}(.., gamma)}.}
  \item{lab}{A numerical vector of the form \code{c(x, y, len)} which
    modifies the way that axes are annotated.
    The values of \code{x} and \code{y} give the (approximate) number of
    tickmarks on the x and y axes and
    \code{len} specifies the label size.
    \emph{Currently}, \code{len} \emph{is unimplemented}.}
  \item{las}{numeric in \{0,1,2,3\}; the style of axis labels.

%-- use \describe{} or \itemize{} when it will finally work for nroff/ascii !
    0 = always parallel to the axis [\emph{default}],
    1 = always horizontal,

    2 = always perpendicular to the axis,
    3 = always vertical.

    Note that other string/character rotation (via  \code{par(srt = ..)})
    does \emph{not} affect the axis labels.
  }
  \item{lty}{The line type.
    Line types can either be specified as an integer
    (0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
    or as one of the character strings
    \code{"blank"}, \code{"solid"}, \code{"dashed"}, \code{"dotted"}
    \code{"dotdash"}, \code{"longdash"}, or \code{"twodash"}, where
    \code{"blank"} uses `invisible lines' (i.e. doesn't draw them).

    Alternatively, a string of up to 8 characters
    (from \code{c(0:9, "A":"F")}) may be given, giving
    the length (typically in points/pixels) of line segments which are
    alternatively drawn and skipped.  For example, \code{"44"} is dashed and
    \code{"13"} is dotted.}
  \item{lwd}{The line width, a \emph{positive} numerical, defaulting to
      \code{1}.}
  \item{mai}{A numerical vector of the form \code{c(bottom, left, top, right)}
    which gives the margin size specified in inches.}
  \item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)}
    which gives the lines of margin to be specified on the four sides of
    the plot.  The default is \code{c(5, 4, 4, 2) + 0.1}.}
  \item{mex}{\code{mex} is a character size expansion factor which is
    used to describe coordinates in the margins of plots.}
  \item{mfcol, mfrow}{A vector of the form \code{c(nr, nc)}.
    Subsequent figures will be drawn in an \code{nr}-by-\code{nc}
    array on the device by \emph{columns} (\code{mfcol}), or
    \emph{rows} (\code{mfrow}), respectively.

    Consider the alternatives, \code{\link{layout}(..)} and
    \code{\link{split.screen}(..)}.
  }
  \item{mfg}{A numerical vector of the form \code{c(i, j)} where
    \code{i} and \code{j} indicate which figure in an array of figures
    is to be drawn next (if setting) or is being drawn (if enquiring).
    The array must already have been  set by \code{mfcol} or
    \code{mfrow}.

    For compatibility with S, the form \code{c(i, j, nr, nc)} is also
    accepted, when \code{nc} and \code{nc} should be the current number of
    rows and number of columns. Mismatches will be ignored, with a warning.}
  \item{mgp}{The margin line (in \code{mex} units) for the axis title,
    axis labels and axis line.  The default is \code{c(3, 1, 0)}.}
  \item{mkh}{The height in inches of symbols to be drawn when the value
    of \code{pch} is an integer.
    %%--- FIXME ---
    \bold{Completely ignored currently}.}
  \item{new}{logical, defaulting to \code{FALSE}.  If set to
    \code{TRUE}, the next high-level plotting command (actually
    \code{\link{plot.new}(.)} should \emph{not clean} the frame before
    drawing ``as if it was on a \bold{new} device''.}
  \item{oma}{A vector of the form \code{c(bottom, left, top, right)} giving the
    size of the outer margins in lines of text.}
  \item{omd}{A vector of the form \code{c(x1, x2, y1, y2)} giving the
    outer margin region in NDC (= normalized device coordinates), i.e.,
    as fraction (in \eqn{[0,1]}) of the device region.}
  \item{omi}{A vector of the form \code{c(bottom, left, top, right)} giving the
    size of the outer margins in inches.}
  \item{pch}{Either an integer specifying a symbol or a single character
    to be used as the default in plotting points. }
  \item{pin}{The width and height of the current plot in inches.}
  \item{plt}{A vector of the form \code{c(x1, x2, y1, y2)} giving
    the coordinates of the plot region as fractions of the current
    figure region.}
  \item{ps}{integer; the pointsize of text and symbols.}
  \item{pty}{A character specifying the type of plot region to be used;
    \code{"s"} generates a square plotting region and \code{"m"}
    generates the maximal plotting region.}
  \item{smo}{(\emph{Unimplemented}) a value which indicates how smooth
      circles and circular arc should be.}
  \item{srt}{The string rotation in degrees.
% NOT true for R (but for S): Causes \code{srt} to be set to the same value.
  }
  \item{tck}{The length of tick marks as a fraction of the smaller of
    the width or height of the plotting region.
    If \code{tck=1}, grid lines are drawn. The default setting is
    to use \code{tcl=-0.5} (see below).}
  \item{tcl}{The length of tick marks as a fraction of the height
    of a line of text.  The default value is \code{-0.5}.}
  \item{tmag}{A number specifying the enlargement of text of the main
    title relative to the other annotating text of the plot.}
  \item{type}{character;  the default plot type desired, see
    \code{\link{plot.default}(type=\dots)}, defaulting to \code{"p"}.}
  \item{usr}{A vector of the form \code{c(x1, x2, y1, y2)} giving the
    extremes of the user coordinates of the plotting region.}
  \item{xaxp}{A vector of the form \code{c(x1, x2, n)} giving the
    coordinates of the extreme tick marks and the number of intervals
    between tick-marks.}
  \item{xaxs}{The style of axis interval
    calculation to be used for the x-axis. Possible values are
    \code{"r"}, \code{"i"}, \code{"e"}, \code{"s"}, \code{"d"}. The
    styles are generally controlled by the range of data or \code{xlim},
    if given. Style \code{"r"} (regular) first extends the data range by
    4 percent and then finds an axis with pretty labels that fits within the
    range. Style \code{"i"} (internal) just finds an axis with pretty
    labels that fits within the original data range. Style \code{"s"}
    (standard) finds an axis with pretty labels within which the
    original data range fits. Style \code{"e"} (extended) is like style
    \code{"s"}, except that it is also ensured that there is room for
    plotting symbols within the bounding box.   Style \code{"d"}
    (direct) specifies that the current axis should be used on
    subsequent plots. (\emph{Only \code{"r"} and \code{"i"} styles are
      currently implemented})}
  \item{xaxt}{A character which specifies the axis type.
    Specifying \code{"n"} causes an axis to be set up, but not plotted.}
  \item{xlog}{\emph{\bold{R.O.}}.  A logical value (see \code{log} in
    \code{\link{plot.default}}).
    If \code{TRUE} a logarithmic scale is in use. For a new device, it
    defaults to \code{FALSE}, i.e., linear scale.}
  \item{xpd}{A logical value or NA.
    If \code{FALSE}, all plotting is clipped to the plot region, if
    \code{TRUE}, all plotting is clipped to the figure region, and if
    \code{NA}, all plotting is clipped to the device region.}
  \item{yaxp}{A vector of the form \code{c(y1, y2, n)} giving
    the coordinates of the extreme tick marks and the number of
    intervals between tick-marks.}
  \item{yaxs}{The style of axis interval
    calculation to be used for the y-axis. See \code{xaxs} above.}
  \item{yaxt}{A character which specifies the axis type.
    Specifying \code{"n"} causes an axis to be set up, but not plotted.}
  \item{ylog}{\emph{\bold{R.O.}}.  A logical value (see \code{log} in
    \code{\link{plot.default}}).  See \code{xlog} above.}
}
\section{Color Specification}{Colors can be specified in several
  different ways.  The simplest way is with a character string giving
  the color name (e.g., \code{"red"}).  A list of the possible colors
  can be obtained with the function \code{\link{colors}}.
  Alternatively, colors can be specified directly in terms of there RGB
  components with a string of the form \code{"#RRGGBB"} where each of
  the pairs \code{RR}, \code{GG}, \code{BB} consist of two hexadecimal
  digits giving a value in the range \code{00} to \code{FF}.  Colors can
  also be specified by giving an index into a small table of colors.
  This provides compatibility with S.

  The functions \code{rgb}, \code{hsv}, \code{gray} and \code{rainbow}
  provide additional ways of generating colors.}
\section{Line Type Specification}{Line types can either be specified by
  giving an index into a small built in table of line types (1=solid,
  2=dashed, 3=dotted) or directly as the lengths of on/off stretches of
  line.  This is done with a string of up to eight characters which give
  the lengths in consecutive positions in the string.  For example, the
  string \code{"33"} specifies three pixels on followed by three off and
  \code{"3313"} specifies three pixels on followed by three off followed
  by one on and finally three off.}
\seealso{
  \code{\link{plot.default}} for some high-level plotting parameters;
  \code{\link{colors}}, \code{\link{gray}}, \code{\link{rainbow}},
  \code{\link{rgb}};
  \code{\link{options}} for other setup parameters;
  graphic devices \code{\link{x11}}, \code{\link{postscript}} and
  setting up device regions by \code{\link{layout}} and
  \code{\link{split.screen}}.
}
\examples{
op <- par(mfrow = c(2, 2), # 2 x 2 pictures on one plot
          pty = "s")      # square plotting region, independent of device size

## At end of plotting, reset to previous settings:
par(op)

## Alternatively,
op <- par(no.readonly = TRUE)# the whole list of settable par's.
## do lots of plotting and par(.) calls, then reset :
par(op)

par("ylog")# FALSE
plot(1:12,log="y")
par("ylog")# TRUE

( nr.prof <-
  c(prof.pilots=16,lawyers=11,farmers=10,salesmen=9,physicians=9,
    mechanics=6,policemen=6,managers=6,engineers=5,teachers=4,
    housewives=3,students=3,armed.forces=1))
par(las=3)
barplot(rbind(nr.prof)) # R 0.63.2: shows alignment problem
par(las=0)# reset to default

ex <- function() {
   old.par <- par(no.readonly = TRUE)# all par settings which could be changed.
   on.exit(par(old.par))
   ## ...
   ## ... do lots of par(..) settings and plots
   ## ...
   invisible() #-- now,  par(old.par)  will be executed
}
ex()
}
\keyword{iplot}
\keyword{dplot}
\keyword{environment}
