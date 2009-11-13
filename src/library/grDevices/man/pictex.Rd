% File src/library/grDevices/man/pictex.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
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
  This function produces graphics suitable for inclusion in TeX and
  LaTeX documents.
}
\details{
  This driver does not have any font metric information, so the use of
  \code{\link{plotmath}} is not supported.

  Multiple plots will be placed as separate environments in the output file.

  Line widths are ignored except when setting the spacing of line textures.
  \code{pch="."} corresponds to a square of side 1pt.

  This device does not support colour (nor does the PicTeX package),
  and all colour settings are ignored.
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
  This driver was provided by Valerio Aimale
  \email{valerio@svpop.com.dist.unige.it}
  of the Department of Internal Medicine, University of Genoa, Italy.
}
\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is 5 inches by 5 inches.
    \item There is no \code{pointsize} argument: the default size is
    interpreted as 10 point.
    \item The only font family is \code{cmss10}.
    \item Line widths are only used when setting the spacing on line textures.
    \item Circle of any radius are allowed.
    \item Colour is not supported.
  }
}
\seealso{
  \code{\link{postscript}}, \code{\link{Devices}}.
}
\examples{
require(graphics)

pictex()
plot(1:11,(-5:5)^2, type='b', main="Simple Example Plot")
dev.off()
##--------------------
\dontrun{
\%\% LaTeX Example
\\documentclass{article}
\\usepackage{pictex}
\\begin{document}
\%...
\\begin{figure}[h]
  \\centerline{\\input{Rplots.tex}}
  \\caption{}
\\end{figure}
\%...
\\end{document}

\%\%-- plain TeX Example --
\\input pictex
$$ \\input Rplots.tex $$
}
##--------------------
unlink("Rplots.tex")
}
\keyword{device}
