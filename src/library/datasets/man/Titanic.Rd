% File src/library/datasets/man/Titanic.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Titanic}
\docType{data}
\alias{Titanic}
\title{Survival of passengers on the Titanic}
\description{
  This data set provides information on the fate of passengers on the
  fatal maiden voyage of the ocean liner \sQuote{Titanic}, summarized according
  to economic status (class), sex, age and survival.
}
\usage{Titanic}
\format{
  A 4-dimensional array resulting from cross-tabulating 2201
  observations on 4 variables.  The variables and their levels are as
  follows:

  \tabular{rll}{
    No \tab Name \tab Levels\cr
    1 \tab Class \tab 1st, 2nd, 3rd, Crew\cr
    2 \tab Sex \tab Male, Female\cr
    3 \tab Age \tab Child, Adult\cr
    4 \tab Survived \tab No, Yes
  }
}
\details{
  The sinking of the Titanic is a famous event, and new books are still
  being published about it.  Many well-known facts---from the
  proportions of first-class passengers to the \sQuote{women and
    children first} policy, and the fact that that policy was not
  entirely successful in saving the women and children in the third
  class---are reflected in the survival rates for various classes of
  passenger.

  These data were originally collected by the British Board of Trade in
  their investigation of the sinking.  Note that there is not complete
  agreement among primary sources as to the exact numbers on board,
  rescued, or lost.

  Due in particular to the very successful film \sQuote{Titanic}, the last
  years saw a rise in public interest in the Titanic.  Very detailed
  data about the passengers is now available on the Internet, at sites
  such as \emph{Encyclopedia Titanica}
  (\url{http://www.rmplc.co.uk/eduweb/sites/phind}).
}
\source{
  Dawson, Robert J. MacG. (1995),
  The \sQuote{Unusual Episode} Data Revisited.
  \emph{Journal of Statistics Education}, \bold{3}.
  \url{https://www.amstat.org/publications/jse/v3n3/datasets.dawson.html}

  The source provides a data set recording class, sex, age, and survival
  status for each person on board of the Titanic, and is based on data
  originally collected by the British Board of Trade and reprinted in:

  British Board of Trade (1990),
  \emph{Report on the Loss of the \sQuote{Titanic} (S.S.)}.
  British Board of Trade Inquiry Report (reprint).
  Gloucester, UK: Allan Sutton Publishing.
}
\examples{
require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")
## Higher survival rates in children?
apply(Titanic, c(3, 4), sum)
## Higher survival rates in females?
apply(Titanic, c(2, 4), sum)
## Use loglm() in package 'MASS' for further analysis ...
}
\keyword{datasets}
