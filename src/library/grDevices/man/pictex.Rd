% File src/library/grDevices/man/pictex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{pictex}
\alias{pictex}
\title{A PicTeX Graphics Driver}
\usage{
pictex(file = "Rplots.tex", width = 5, height = 4, debug = FALSE,
       bg = "white", fg = "black")
}
\arguments{
  \item{file}{the file where output will appear.}
  \item{width}{The width of the plot in inches.}
  \item{height}{the height of the plot in inches.}
  \item{debug}{should debugging information be printed.}
  \item{bg}{the background color for the plot.  Ignored.}
  \item{fg}{the foreground color for the plot.  Ignored.}
}
\description{
  This function produces simple graphics suitable for inclusion in TeX
  and LaTeX documents.  It dates from the very early days of \R and is
  for historical interest only.
}
\details{
  This driver is much more basic than the other graphics drivers
  included in \R.  It does not have any font metric information, so the
  use of \code{\link{plotmath}} is not supported.

  Line widths are ignored except when setting the spacing of line textures.
  \code{pch = "."} corresponds to a square of side 1pt.

  This device does not support colour (nor does the PicTeX package),
  and all colour settings are ignored.

  Note that text is recorded in the file as-is, so annotations involving
  TeX special characters (such as ampersand and underscore) need to be
  quoted as they would be when entering TeX.

  Multiple plots will be placed as separate environments in the output file.
}
\references{
  Knuth, D. E. (1984)
  \emph{The TeXbook.}
  Reading, MA: Addison-Wesley.

  Lamport, L. (1994)
  \emph{LATEX: A Document Preparation System.}
  Reading, MA: Addison-Wesley.

  Goossens, M., Mittelbach, F. and Samarin, A. (1994)
  \emph{The LATEX Companion.}
  Reading, MA: Addison-Wesley.
}
\author{
  This driver was provided around 1996--7 by Valerio Aimale
  of the Department of Internal Medicine, University of Genoa, Italy.
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is 5 inches by 4 inches.
    \item There is no \code{pointsize} argument: the default size is
    interpreted as 10 point.
    \item The only font family is \code{cmss10}.
    \item Line widths are only used when setting the spacing on line textures.
    \item Circle of any radius are allowed.
    \item Colour is not supported.
  }
}
\seealso{
  \code{\link{postscript}}, \code{\link{pdf}}, \code{\link{Devices}}.

  The \code{tikzDevice} in the CRAN package of that name for more modern
  TeX-based graphics (\url{http://pgf.sourceforge.net/}, although
  including PDF figures \emph{via} \command{pdftex} is most common
  in (La)TeX documents).
}
\examples{
require(graphics)

pictex()
plot(1:11, (-5:5)^2, type = "b", main = "Simple Example Plot")
dev.off()
##--------------------
\dontrun{
\%\% LaTeX Example
\\documentclass{article}
\\usepackage{pictex}
\\usepackage{graphics} \% for \\rotatebox
\\begin{document}
\%...
\\begin{figure}[h]
  \\centerline{\\input{Rplots.tex}}
  \\caption{}
\\end{figure}
\%...
\\end{document}
}
##--------------------
unlink("Rplots.tex")
}
\keyword{device}
