\name{mtext}
\title{Write Text into the Margins of a Plot}
\usage{
mtext(text, side = 3, line = 0, outer = FALSE, at = NA,
      adj = NA, padj = NA, cex = NA, col = NA, font = NA, vfont = NULL, \dots)
}
\alias{mtext}
\arguments{
  \item{text}{one or more character strings or expressions.}
  \item{side}{on which side of the plot (1=bottom, 2=left, 3=top, 4=right).}
  \item{line}{on which MARgin line, starting at 0 counting outwards.}
  \item{outer}{use outer margins if available.}
  \item{at}{give location in user-coordinates.  If \code{length(at)==0}
    (the default), the location will be determined by \code{adj}.
  }
  \item{adj}{adjustment for each string in reading direction. For strings parallel
    to the axes, \code{adj=0} means left or bottom alignment, and
    \code{adj=1} means right or top alignment.
    If \code{adj} is not a finite value (the default), the value of
    \code{par("las")} determines the adjustment.  For strings plotted
    parallel to the axis the default is to centre the string.}
  \item{padj}{adjustment for each string perpendicular to the reading direction 
    (which is controlled by \code{adj}). For strings parallel
    to the axes, \code{padj=0} means right or top alignment, 
        and \code{padj=1} means left or bottom alignment.
    
    If \code{padj} is not a finite value (the default), the value of
    \code{par("las")} determines the adjustment.  For strings plotted
    perpendicular to the axis the default is to centre the string.}
  \item{\dots}{Further graphical parameters (see \code{\link{text}} and
    \code{\link{par}}) ; currently supported are:}
  \item{cex}{character expansion factor.  \code{NULL} and \code{NA}
    are equivalent to \code{1.0}.  This is an absolute measure, not
    scaled by \code{par("cex")} or by setting \code{par("mfrow")}
    or \code{par("mfcol")}.}
  \item{col}{color to use.}
  \item{font}{font for text.}
  \item{vfont}{vector font for text.}
}
% mtext is defined via do_mtext(.) in  src/main/plot.c ---
\description{
  Text is written in one of the four margins of the current figure region
  or one of the outer margins of the device region.
}
\details{
  The \dQuote{user coordinates} in the outer margins always range from zero to
  one, and are not affected by the user coordinates in the figure
  region(s) --- \R is differing here from other implementations of S.

  The arguments \code{side}, \code{line}, \code{at}, \code{at},
  \code{adj}, the further graphical parameters and even \code{outer} can
  be vectors, and recycling will take place to plot as many strings as
  the longest of the vector arguments.  Note that a vector \code{adj}
  has a different meaning from \code{\link{text}}.

  \code{adj = 0.5} will centre the string, but for \code{outer=TRUE} on
  the device region rather than the plot region.

  Parameter \code{las} will determine the orientation of the string(s).
  For strings plotted perpendicular to the axis the default justification
  is to place the end of the string nearest the axis on the specified
  line.  (Note that this differs from S, which uses \code{srt} if
  \code{at} is supplied and \code{las} if it is not.)

  Note that if the text is to be plotted perpendicular to the axis,
  \code{adj} determines the justification of the string \emph{and} the
  position along the axis unless \code{at} is specified.
}
\section{Side Effects}{The given text is written onto the current plot.}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{title}}, \code{\link{text}}, \code{\link{plot}},
  \code{\link{par}};
  \code{\link{plotmath}} for details on mathematical annotation.
}
\examples{
plot(1:10, (-4:5)^2, main="Parabola Points", xlab="xlab")
mtext("10 of them")
for(s in 1:4)
    mtext(paste("mtext(..., line= -1, {side, col, font} = ",s,
          ", cex = ", (1+s)/2, ")"), line = -1,
          side=s, col=s, font=s, cex= (1+s)/2)
mtext("mtext(..., line= -2)", line = -2)
mtext("mtext(..., line= -2, adj = 0)", line = -2, adj =0)
##--- log axis :
plot(1:10, exp(1:10), log='y', main="log='y'", xlab="xlab")
for(s in 1:4) mtext(paste("mtext(...,side=",s,")"), side=s)
}
\keyword{aplot}


