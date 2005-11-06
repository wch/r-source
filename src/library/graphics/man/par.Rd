\name{par}
\alias{par}
\alias{.Pars}
\concept{adj}
\concept{ann}
\concept{ask}
\concept{bg}
\concept{bty}
\concept{cex.axis}
\concept{cex.lab}
\concept{cex.main}
\concept{cex.sub}
\concept{cex}
\concept{cin}
\concept{col.axis}
\concept{col.lab}
\concept{col.main}
\concept{col.sub}
\concept{col}
\concept{cra}
\concept{crt}
\concept{csi}
\concept{cxy}
\concept{din}
\concept{err}
\concept{family}
\concept{fg}
\concept{fig}
\concept{fin}
\concept{font.axis}
\concept{font.lab}
\concept{font.main}
\concept{font.sub}
\concept{font}
\concept{gamma}
\concept{lab}
\concept{las}
\concept{lend}
\concept{lheight}
\concept{ljoin}
\concept{lmitre}
\concept{lty}
\concept{lwd}
\concept{mai}
\concept{mar}
\concept{mex}
\concept{mfcol}
\concept{mfg}
\concept{mfrow}
\concept{mgp}
\concept{mkh}
\concept{new}
\concept{oma}
\concept{omd}
\concept{omi}
\concept{pch}
\concept{pin}
\concept{plt}
\concept{ps}
\concept{pty}
\concept{smo}
\concept{srt}
\concept{tck}
\concept{tcl}
\concept{tmag}
\concept{usr}
\concept{xaxp}
\concept{xaxs}
\concept{xaxt}
\concept{xlog}
\concept{xpd}
\concept{yaxp}
\concept{yaxs}
\concept{yaxt}
\concept{ylog}

\title{Set or Query Graphical Parameters}

%% The 'real documentation' is the code	in ../../../main/par.c
%% AND (defined AND commented)		in ../../../include/Graphics.h
%% Default initialization by GInit()	in ../../../main/graphics.c

\description{
  \code{par} can be used to set or query graphical parameters.
  Parameters can be set by specifying them as arguments to \code{par} in
  \code{tag = value} form, or by passing them as a list of tagged
  values.
}
\usage{
par(\dots, no.readonly = FALSE)

\special{<highlevel plot> (\dots, <tag> = <value>)}
}

\arguments{
  \item{\dots}{arguments in \code{tag = value} form, or a list of tagged
    values.  The tags must come from the graphical parameters described
    below.}
  \item{no.readonly}{logical; if \code{TRUE} and there are no other
    arguments, only parameters are returned which can be set by a
    subsequent \code{par()} call.}
}

\details{
  Parameters are queried by giving one or more character vectors to
  \code{par}.

  \code{par()} (no arguments) or \code{par(no.readonly=TRUE)} is used to
  get \emph{all} the graphical parameters (as a named list).  Their
  names are currently taken from the unexported variable \code{.Pars}.

  \emph{\bold{R.O.}} indicates \emph{\bold{read-only arguments}}: These
  may only be used in queries and cannot be set.  (\code{"cin"},
  \code{"cra"}, \code{"csi"}, \code{"cxy"} and \code{"din"} are always
  read-only, and \code{"gamma"} is on most devices.)

  There are several parameters can only be set by a call to \code{par()}:
  \itemize{
    \item \code{"ask"},
    \item \code{"family"}, \code{"fig"}, \code{"fin"},
    \item \code{"lend"}, \code{"lheight"}, \code{"ljoin"}, \code{"lmitre"},
    \item \code{"mai"}, \code{"mar"}, \code{"mex"},
    \code{"mfcol"}, \code{"mfrow"}, \code{"mfg"},
    \item \code{"new"},
    \item \code{"oma"}, \code{"omd"}, \code{"omi"},
    \item \code{"pin"}, \code{"plt"}, \code{"ps"}, \code{"pty"},
    \item \code{"usr"},
    \item \code{"xlog"}, \code{"ylog"}
  }
  The remaining parameters can also be set as arguments (often via
  \code{\dots}) to high-level plot functions such as
  \code{\link{plot.default}}, \code{\link{plot.window}},
  \code{\link{points}}, \code{\link{lines}}, \code{\link{abline}},
  \code{\link{axis}}, \code{\link{title}}, \code{\link{text}},
  \code{\link{mtext}}, \code{\link{segments}}, \code{\link{symbols}},
  \code{\link{arrows}}, \code{\link{polygon}}, \code{\link{rect}},
  \code{\link{box}}, \code{\link{contour}}, \code{\link{filled.contour}}
  and \code{\link{image}}.  Such settings will be active during the
  execution of the function, only.  However, see the comments on
  \code{bg} and \code{cex}, which may be taken as arguments to certain
  plot functions rather than as graphical parameters.
}

\value{
  When parameters are set, their former values are returned in an
  invisible named list.  Such a list can be passed as an argument to
  \code{par} to restore the parameter values.
  Use \code{par(no.readonly = TRUE)} for the full list of parameters
  that can be restored.

  When just one parameter is queried, the value is a character string.
  When two or more parameters are queried, the result is a list of
  character strings, with the list names giving the parameters.

  Note the inconsistency: setting one parameter returns a list, but
  querying one parameter returns a vector.
}

\section{Graphical Parameters}{
  \describe{
    \item{\code{adj}}{The value of \code{adj} determines the way in
      which text strings are justified.  A value of \code{0} produces
      left-justified text, \code{0.5} centered text and \code{1}
      right-justified text.  (Any value in \eqn{[0, 1]} is allowed, and
      on most devices values outside that interval will also work.)
      Note that the \code{adj} argument of \code{\link{text}} also
      allows \code{adj = c(x, y)} for different adjustment in x- and y-
      direction.}
    \item{\code{ann}}{If set to \code{FALSE}, high-level plotting
      functions calling \code{\link{plot.default}} do not annotate the
      plots they produce with axis titles and overall titles.  The
      default is to do annotation.}
    \item{\code{ask}}{logical.  If \code{TRUE}, the user is asked for
      input, before a new figure is drawn.}
    \item{\code{bg}}{The color to be used for the background of plots.
      When called from \code{par()} it also sets \code{new=FALSE}.
      A description of how colors are specified is given below.
      Note that some graphics functions such as
      \code{\link{plot.default}} and \code{\link{points}} have an
      \emph{argument} of this name with a different meaning.}
    \item{\code{bty}}{A character string which determined the type of
      \code{\link{box}} which is drawn about plots.  If \code{bty} is one of
      \code{"o"}, \code{"l"}, \code{"7"}, \code{"c"}, \code{"u"}, or
      \code{"]"} the resulting box resembles the corresponding upper
      case letter.  A value of \code{"n"} suppresses the box.}
    \item{\code{cex}}{A numerical value giving the amount by which
      plotting text and symbols should be scaled relative to the
      default.  Note that some graphics functions such as
      \code{\link{plot.default}} have an \emph{argument} of this name
      which multiplies this graphical parameter.}
    \item{\code{cex.axis}}{The magnification to be used for axis
      annotation relative to the current setting of \code{cex}.}
    \item{\code{cex.lab}}{The magnification to be used for x and y
      labels relative to the current setting of \code{cex}.}
    \item{\code{cex.main}}{The magnification to be used for main titles
      relative to the current setting of \code{cex}.}
    \item{\code{cex.sub}}{The magnification to be used for sub-titles
      relative to the current setting of \code{cex}.}
    \item{\code{cin}}{\emph{\bold{R.O.}}; character size
      \code{(width,height)} in inches.}
    \item{\code{col}}{A specification for the default plotting color.  A
      description of how colors are specified is given below.}
    \item{\code{col.axis}}{The color to be used for axis annotation.}
    \item{\code{col.lab}}{The color to be used for x and y labels.}
    \item{\code{col.main}}{The color to be used for plot main titles.}
    \item{\code{col.sub}}{The color to be used for plot sub-titles.}
    \item{\code{cra}}{\emph{\bold{R.O.}}; size of default character
      \code{(width,height)} in \dQuote{rasters} (pixels).}
    \item{\code{crt}}{A numerical value specifying (in degrees) how
      single characters should be rotated.  It is unwise to expect
      values other than multiples of 90 to work.  Compare with
      \code{srt} which does string rotation.}
    \item{\code{csi}}{\emph{\bold{R.O.}}; height of (default sized)
      characters in inches.}
    \item{\code{cxy}}{\emph{\bold{R.O.}}; size of default character
      \code{(width,height)} in user coordinate units.
      \code{par("cxy")} is \code{par("cin")/par("pin")} scaled to user
      coordinates.
      Note that \code{c(\link{strwidth}(ch), \link{strheight}(ch))} for
      a given string \code{ch} is usually much more precise.}
    \item{\code{din}}{\emph{\bold{R.O.}}; the device dimensions,
      \code{(width,height)}, in inches.}
    \item{\code{err}}{(\emph{Unimplemented}; \R is silent when points
      outside the plot region are \emph{not} plotted.)
      The degree of error reporting desired.}
    \item{\code{family}}{The name of a font family for drawing text.
      This name is device-independent and gets mapped by each
      graphics device to a device-specific font description.
      The default value is \code{""} which means that the
      default device font will be used.
      Standard values are \code{"serif"}, \code{"sans"},
      \code{"mono"}, and \code{"symbol"} and the \link{Hershey}
      font families are also available.  Different devices
      may define others.  Some devices will ignore this setting
      completely.}
    \item{\code{fg}}{The color to be used for the foreground of plots.
      This is the default color used for things like axes and boxes
      around plots.  When called from \code{par()} this also sets
      parameter \code{col} to the same value.

      A description of how colors are specified is given below.}
    \item{\code{fig}}{A numerical vector of the form \code{c(x1, x2, y1,
	y2)} which gives the (NDC) coordinates of the figure region in
      the display region of the device. If you set this, unlike S, you
      start a new plot, so to add to an existing plot use
      \code{new=TRUE} as well.}
    \item{\code{fin}}{The figure region dimensions,
      \code{(width,height)}, in inches. If you set this, unlike S, you
      start a new plot.}
    \item{\code{font}}{An integer which specifies which font to use for
      text.  If possible, device drivers arrange so that 1 corresponds
      to plain text, 2 to bold face, 3 to italic and 4 to bold
      italic.  Also, font 5 is expected to be the symbol font, in Adobe
      symbol encoding.  On some devices font families can be selected by
      \code{family} to choose different sets of 5 fonts.}
    \item{\code{font.axis}}{The font to be used for axis annotation.}
    \item{\code{font.lab}}{The font to be used for x and y labels.}
    \item{\code{font.main}}{The font to be used for plot main titles.}
    \item{\code{font.sub}}{The font to be used for plot sub-titles.}
    \item{\code{gamma}}{the gamma correction, see argument \code{gamma}
      to \code{\link{hsv}}.  This is only accepted if the current device
      has support for changing the gamma correction: currently only
      \code{windows} and \code{quartz} do.  (\code{X11} has support for
      setting the gamma correction when opening the device, but not for
      changing it.)}
    \item{\code{lab}}{A numerical vector of the form \code{c(x, y, len)}
      which modifies the way that axes are annotated.  The values of
      \code{x} and \code{y} give the (approximate) number of tickmarks
      on the x and y axes and \code{len} specifies the label size.  The
      default is \code{c(5, 5, 7)}.
      \emph{Currently}, \code{len} \emph{is unimplemented}.}
    \item{\code{las}}{numeric in \{0,1,2,3\}; the style of axis labels.
      \describe{
	\item{0:}{always parallel to the axis [\emph{default}],}
	\item{1:}{always horizontal,}
	\item{2:}{always perpendicular to the axis,}
	\item{3:}{always vertical.}
      }
      Note that other string/character rotation (via argument \code{srt}
      to \code{par}) does \emph{not} affect the axis labels.
    }
    \item{\code{lend}}{The line end style.
      This can be specified as an integer or string:
      0 and \code{"round"} mean rounded line caps;
      1 and \code{"butt"} mean butt line caps;
      2 and \code{"square"} mean square line caps.
    }
    \item{\code{lheight}}{The line height multiplier.
      The height of a line of text (used to vertically space
      multi-line text) is found by multiplying the current
      font size both by the current character expansion and by the
      line height multiplier.  Default value is 1.}
    \item{\code{ljoin}}{The line join style.
      This can be specified as an integer or string:
      0 and \code{"round"} mean rounded line joins;
      1 and \code{"mitre"} mean mitred line joins;
      2 and \code{"bevel"} mean bevelled line joins.}  
    \item{\code{lmitre}}{The line mitre limit.
      This controls when mitred line joins are automatically
      converted into bevelled line joins.  The 
      value must be larger than 1 and the default is 10.  Not all
      devices will honour this setting.
    }
    \item{\code{lty}}{The line type.
      Line types can either be specified as an integer (0=blank,
      1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or
      as one of the character strings \code{"blank"}, \code{"solid"},
      \code{"dashed"}, \code{"dotted"}, \code{"dotdash"},
      \code{"longdash"}, or \code{"twodash"}, where \code{"blank"} uses
      \sQuote{invisible lines} (i.e., doesn't draw them).

      Alternatively, a string of up to 8 characters (from \code{c(1:9,
	"A":"F")}) may be given, giving the length of line segments
      which are alternatively drawn and skipped.  See section
      \sQuote{Line Type Specification} below.}
    \item{\code{lwd}}{The line width, a \emph{positive} number,
      defaulting to \code{1}.  The interpretation is device-specific,
      and some devices do not implement line widths less than one.
    }
    \item{\code{mai}}{A numerical vector of the form \code{c(bottom,
	left, top, right)} which gives the margin size specified in
      inches.}
    \item{\code{mar}}{A numerical vector of the form \code{c(bottom,
	left, top, right)} which gives the number of lines of margin to be
      specified on the four sides of the plot.
      The default is \code{c(5, 4, 4, 2) + 0.1}.}
    \item{\code{mex}}{
      \code{mex} is a character size expansion factor
      which is used to describe coordinates in the margins of plots.
      Note that this does not change the font size, rather specifies the
      size of font used to convert between \code{mar} and \code{mai},
      and between \code{oma} and \code{omi}.
    }
    \item{\code{mfcol, mfrow}}{A vector of the form \code{c(nr, nc)}.
      Subsequent figures will be drawn in an \code{nr}-by-\code{nc}
      array on the device by \emph{columns} (\code{mfcol}), or
      \emph{rows} (\code{mfrow}), respectively.

      In a layout with exactly two rows and columns the base value of
      \code{"cex"} is reduced by a factor of 0.83: if there are three or
      more of either rows or columns, the reduction factor is 0.66.

      Consider the alternatives, \code{\link{layout}} and
      \code{\link{split.screen}}.
    }
    \item{\code{mfg}}{A numerical vector of the form \code{c(i, j)}
      where \code{i} and \code{j} indicate which figure in an array of
      figures is to be drawn next (if setting) or is being drawn (if
      enquiring).  The array must already have been set by \code{mfcol}
      or \code{mfrow}.

      For compatibility with S, the form \code{c(i, j, nr, nc)} is also
      accepted, when \code{nr} and \code{nc} should be the current
      number of rows and number of columns.  Mismatches will be ignored,
      with a warning.}
    \item{\code{mgp}}{The margin line (in \code{mex} units) for the axis
      title, axis labels and axis line.
      The default is \code{c(3, 1, 0)}.}
    \item{\code{mkh}}{The height in inches of symbols to be drawn when
      the value of \code{pch} is an integer.
      %% <FIXME>
      \emph{Completely ignored currently}.
      %% </FIXME>
    }
    \item{\code{new}}{logical, defaulting to \code{FALSE}.  If set to
      \code{TRUE}, the next high-level plotting command (actually
      \code{\link{plot.new}}) should \emph{not clean} the frame before
      drawing \dQuote{as if it was on a \bold{\emph{new}} device}.}
    \item{\code{oma}}{A vector of the form \code{c(bottom, left, top,
	right)} giving the size of the outer margins in lines of text.}
    \item{\code{omd}}{A vector of the form \code{c(x1, x2, y1, y2)}
      giving the outer margin region in NDC (= normalized device
      coordinates), i.e., as fraction (in \eqn{[0,1]}) of the device
      region.}
    \item{\code{omi}}{A vector of the form \code{c(bottom, left, top,
	right)} giving the size of the outer margins in inches.}
    \item{\code{pch}}{Either an integer specifying a symbol or a single
      character to be used as the default in plotting points.  See
      \code{\link{points}} for possible values and their interpretation.
    }
    \item{\code{pin}}{The current plot dimensions, \code{(width,height)},
      in inches.}
    \item{\code{plt}}{A vector of the form \code{c(x1, x2, y1, y2)}
      giving the coordinates of the plot region as fractions of the
      current figure region.}
    \item{\code{ps}}{integer; the pointsize of text (but not symbols).  Unlike
      the \code{pointsize} argument of most devices, this does not change
      the relationship between \code{mar} and \code{mai} (nor \code{oma}
      and \code{omi}), but it does scale \code{cin} and \code{csi}.

      What is meant by \sQuote{pointsize} is device-specific, but most
      devices mean a multiple of 1bp, that is 1/72 of an inch.
    }
    \item{\code{pty}}{A character specifying the type of plot region to
      be used; \code{"s"} generates a square plotting region and
      \code{"m"} generates the maximal plotting region.}
    \item{\code{smo}}{(\emph{Unimplemented}) a value which indicates how
      smooth circles and circular arcs should be.}
    \item{\code{srt}}{The string rotation in degrees.  See the comment
      about \code{crt}.
      %% NOT true for R (but for S): Causes \code{srt} to be set to the
      %% same value. ?? Is `crt' meant here ??
    }
    \item{\code{tck}}{The length of tick marks as a fraction of the
      smaller of the width or height of the plotting region.
      If \code{tck >= 0.5} it is interpreted as a fraction of the
      relevant side, so if
      \code{tck = 1} grid lines are drawn.  The default setting
      (\code{tck = NA}) is to use \code{tcl = -0.5} (see below).}
    \item{\code{tcl}}{The length of tick marks as a fraction of the
      height of a line of text.  The default value is \code{-0.5};
      setting \code{tcl = NA} sets \code{tck = -0.01} which is S' default.}
    \item{\code{tmag}}{A number specifying the enlargement of text of
      the main title relative to the other annotating text of the plot.
      Defaults to \code{1.2}.}
    \item{\code{usr}}{A vector of the form \code{c(x1, x2, y1, y2)}
      giving the extremes of the user coordinates of the plotting
      region.  When a logarithmic scale is in use (i.e.,
      \code{par("xlog")} is true, see below), then the x-limits will be
      \code{10 ^ par("usr")[1:2]}.  Similarly for the y-axis.
    }
    \item{\code{xaxp}}{A vector of the form \code{c(x1, x2, n)} giving
      the coordinates of the extreme tick marks and the number of
      intervals between tick-marks when \code{par("xlog")} is false.
      Otherwise, when \emph{log} coordinates are active, the three
      values have a different meaning: For a small range, \code{n} is
      \emph{negative}, and the ticks are as in the linear case,
      otherwise, \code{n} is in \code{1:3}, specifying a case number,
      and \code{x1} and \code{x2} are the lowest and highest power of 10
      inside the user coordinates, \code{10 ^ par("usr")[1:2]}. (The
      \code{"usr"} coordinates are log10-transformed here!)

      \describe{
	\item{n=1}{will produce tick marks at \eqn{10^j} for
	  integer \eqn{j},}
	\item{n=2}{gives marks  \eqn{k 10^j} with \eqn{k \in \{1, 5\}},}
	\item{n=3}{gives marks  \eqn{k 10^j} with \eqn{k \in \{1, 2, 5\}}.}
      }
      See \code{\link{axTicks}()} for a pure \R implementation of this.

      This parameter is reset when a user coordinate system is set up,
      for example by starting a new page or by calling
      \code{\link{plot.window}} or setting \code{par("usr")}: \code{n}
      is taken from \code{par("lab")}.  It affects the default behaviour
      of subsequent calls to \code{\link{axis}} for sides 1 or 3.
    }
    \item{\code{xaxs}}{The style of axis interval calculation to be used
      for the x-axis.  Possible values are \code{"r"}, \code{"i"},
      \code{"e"}, \code{"s"}, \code{"d"}.  The styles are generally
      controlled by the range of data or \code{xlim}, if given.
      Style \code{"r"} (regular) first extends the data range by 4
      percent and then finds an axis with pretty labels that fits within
      the range.
      Style \code{"i"} (internal) just finds an axis with pretty labels
      that fits within the original data range.
      Style \code{"s"} (standard) finds an axis with pretty labels
      within which the original data range fits.
      Style \code{"e"} (extended) is like style \code{"s"}, except that
      it is also ensured that there is room for plotting symbols within
      the bounding box.
      Style \code{"d"} (direct) specifies that the current axis should
      be used on subsequent plots.
      (\emph{Only \code{"r"} and \code{"i"} styles are currently
	implemented})}
    \item{\code{xaxt}}{A character which specifies the axis type.
      Specifying \code{"n"} causes an axis to be set up, but not
      plotted.  The standard value is \code{"s"}: for compatibility with
      S values \code{"l"} and \code{"e"} are accepted but are equivalent
      to \code{"s"}.}
    \item{\code{xlog}}{A logical value (see \code{log} in
      \code{\link{plot.default}}).  If \code{TRUE}, a logarithmic scale
      is in use (e.g., after \code{plot(*, log = "x")}).
      For a new device, it defaults to \code{FALSE}, i.e., linear scale.}
    \item{\code{xpd}}{A logical value or \code{NA}.
      If \code{FALSE}, all plotting is clipped to the plot region, if
      \code{TRUE}, all plotting is clipped to the figure region, and if
      \code{NA}, all plotting is clipped to the device region.}
    \item{\code{yaxp}}{A vector of the form \code{c(y1, y2, n)} giving
      the coordinates of the extreme tick marks and the number of
      intervals between tick-marks unless for log coordinates, see
      \code{xaxp} above.}
    \item{\code{yaxs}}{The style of axis interval calculation to be used
      for the y-axis.  See \code{xaxs} above.}
    \item{\code{yaxt}}{A character which specifies the axis type.
      Specifying \code{"n"} causes an axis to be set up, but not plotted.}
    \item{\code{ylog}}{A logical value; see \code{xlog} above.}
  }
}

\section{Color Specification}{
  Colors can be specified in several different ways. The simplest way is
  with a character string giving the color name (e.g., \code{"red"}).  A
  list of the possible colors can be obtained with the function
  \code{\link{colors}}. Alternatively, colors can be specified directly in
  terms of their RGB components with a string of the form \code{"#RRGGBB"}
  where each of the pairs \code{RR}, \code{GG}, \code{BB} consist of two
  hexadecimal digits giving a value in the range \code{00} to \code{FF}.
  Colors can also be specified by giving an index into a small table of
  colors, the \code{\link{palette}}.  This provides compatibility with
  S.  Index \code{0} corresponds to the background color.

  Additionally, \code{"transparent"} or (integer) \code{NA} is
  \emph{transparent}, useful for filled areas (such as the background!),
  and just invisible for things like lines or text.

  The functions \code{\link{rgb}}, \code{\link{hsv}}, \code{\link{hcl}},
  \code{\link{gray}} and \code{\link{rainbow}}
  provide additional ways of generating colors.}

\section{Line Type Specification}{
  Line types can either be specified by giving an index into a small
  built-in table of line types (1 = solid, 2 = dashed, etc, see
  \code{lty} above) or directly as the lengths of on/off stretches of
  line.  This is done with a string of an even number (up to eight)
  of characters, namely non-zero
  (hexadecimal) digits which give the lengths in consecutive positions
  in the string.  For example, the string \code{"33"} specifies three
  units on followed by three off and \code{"3313"} specifies three units
  on followed by three off followed by one on and finally three off.
  The \sQuote{units} here are (on most devices) proportional to
  \code{lwd}, and with \code{lwd = 1} are in pixels or points.

  The five standard dash-dot line types (\code{lty = 2:6}) correspond to
  \code{c("44", "13", "1343", "73", "2262")}.

  Note that \code{NA} is not a valid value for \code{lty}.
}

\note{
  The effect of restoring all the (settable) graphics parameters as
  in the examples is hard to predict if the device has been resized.
  Several of them are attempting to set the same things in different
  ways, and those last in the alphabet will win.  In particular, the
  settings of \code{mai}, \code{mar}, \code{pin}, \code{plt} and
  \code{pty} interact, as do the outer margin settings, the figure
  layout and figure region size.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{plot.default}} for some high-level plotting parameters;
  \code{\link{colors}};
  \code{\link{options}} for other setup parameters;
  graphic devices \code{\link{x11}}, \code{\link{postscript}} and
  setting up device regions by \code{\link{layout}} and
  \code{\link{split.screen}}.
}

\examples{
op <- par(mfrow = c(2, 2), # 2 x 2 pictures on one plot
          pty = "s")       # square plotting region,
                           # independent of device size

## At end of plotting, reset to previous settings:
par(op)

## Alternatively,
op <- par(no.readonly = TRUE) # the whole list of settable par's.
## do lots of plotting and par(.) calls, then reset:
par(op)

par("ylog") # FALSE
plot(1 : 12, log = "y")
par("ylog") # TRUE

plot(1:2, xaxs = "i") # 'inner axis' w/o extra space
stopifnot(par("xaxp")[1:2] == 1:2 &&
          par("usr") [1:2] == 1:2)

( nr.prof <-
  c(prof.pilots=16,lawyers=11,farmers=10,salesmen=9,physicians=9,
    mechanics=6,policemen=6,managers=6,engineers=5,teachers=4,
    housewives=3,students=3,armed.forces=1))
par(las = 3)
barplot(rbind(nr.prof)) # R 0.63.2: shows alignment problem
par(las = 0)# reset to default

## 'fg' use:
plot(1:12, type = "b", main="'fg' : axes, ticks and box in gray",
     fg = gray(0.7), bty="7" , sub=R.version.string)

ex <- function() {
   old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
   on.exit(par(old.par))
   ## ...
   ## ... do lots of par() settings and plots
   ## ...
   invisible() #-- now,  par(old.par)  will be executed
}
ex()
}

\keyword{iplot}
\keyword{dplot}
\keyword{environment}
