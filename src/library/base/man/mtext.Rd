\name{mtext}
\title{Write text into the margins of a plot}
\usage{
mtext(text, side=3, line=0, outer=FALSE, at=NULL, adj = NA, \dots)
}
\alias{mtext}
\arguments{
\item{text}{character string to be written.}
\item{side}{on which side of the plot (1=bottom, 2=left, 3=top, 4=right).}
\item{line}{on which MARgin line, starting at 0 counting outwards.}
\item{outer}{use outer margins if available.}
\item{at}{give location in user-coordinates.  If \code{length(at)==0}
  (default), the location will correspond to \code{par("adj")}.}
\item{adj}{length = 1 or 2 : adjustment in x- (and optionally y-)
  direction.  If \code{adj} isn't finite (default), a default is setup,
  using \code{par("las")} (and \emph{not} \code{par("adj")}!}
\item{\dots}{Further graphical parameters; currently supported are:}
\item{cex}{character expansion factor (default = 1).}
\item{col}{color to use.}
\item{font}{font for text.}
}
 %  --- mtext is defined via do_mtext(.) in  src/main/plot.c ---
\description{
\code{text} is written in one of the four margins (\code{side}) of the plot.
\code{line} measures the distance outwardly in character size units.
}
\section{Side Effects}{The given text is written onto the current plot.}
\seealso{
\code{\link{title}}, \code{\link{text}}, \code{\link{plot}}.
}
\examples{
plot(1:10, (-4:5)^2, main="Parabola Points", xlab="xlab")
mtext("10 of them")
for(s in c(1:2,4))
    mtext(paste("mtext(..,side=",s,")"), side=s)
mtext("mtext(.., line= -2)", line = -2)
mtext("mtext(.., line= -1, adj = 0)", line = -1, adj =0)
##--- log axis :
plot(1:10, exp(1:10), log='y', main="log='y'", xlab="xlab")
for(s in 1:4) mtext(paste("mtext(..,side=",s,")"), side=s)
}
\keyword{aplot}

