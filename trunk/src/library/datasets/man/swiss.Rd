% File src/library/datasets/man/swiss.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{swiss}
\docType{data}
\alias{swiss}
\title{Swiss Fertility and Socioeconomic Indicators (1888) Data}
\description{
  Standardized fertility measure and socio-economic indicators for each
  of 47 French-speaking provinces of Switzerland at about 1888.
}
\usage{swiss}
\format{
  A data frame with 47 observations on 6 variables, \emph{each} of which
  is in percent, i.e., in \eqn{[0, 100]}.

  \tabular{rll}{
    [,1] \tab Fertility \tab \eqn{I_g}{Ig},
    \sQuote{common standardized fertility measure}\cr
    [,2] \tab Agriculture\tab \% of males involved in agriculture
    as occupation\cr
    [,3] \tab Examination\tab \% draftees receiving highest mark
    on army examination\cr
    [,4] \tab Education \tab \% education beyond primary school for draftees.\cr
    [,5] \tab Catholic \tab \% \sQuote{catholic} (as opposed to \sQuote{protestant}).\cr
    [,6] \tab Infant.Mortality\tab live births who live less than 1
    year.
  }

  All variables but \sQuote{Fertility} give proportions of the
  population.
}
\details{
  (paraphrasing Mosteller and Tukey):

  Switzerland, in 1888, was entering a period known as the
  \emph{demographic transition}; i.e., its fertility was beginning to
  fall from the high level typical of underdeveloped countries.

  The data collected are for 47 French-speaking \dQuote{provinces} at
  about 1888.

  Here, all variables are scaled to \eqn{[0, 100]}, where in the
  original, all but \code{"Catholic"} were scaled to \eqn{[0, 1]}.
}
\source{
  Project \dQuote{16P5}, pages 549--551 in

  Mosteller, F. and Tukey, J. W. (1977)
  \emph{Data Analysis and Regression: A Second Course in Statistics}.
  Addison-Wesley, Reading Mass.

  indicating their source as
  \dQuote{Data used by permission of Franice van de Walle. Office of
  Population Research, Princeton University, 1976.  Unpublished data
  assembled under NICHD contract number No 1-HD-O-2077.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\note{
  Files for all 182 districts in 1888 and other years have been available at
  \url{https://opr.princeton.edu/archive/pefp/switz.aspx}.

  They state that variables \code{Examination} and \code{Education}
  are averages for 1887, 1888 and 1889.
}
\examples{
require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "swiss data",
      col = 3 + (swiss$Catholic > 50))
summary(lm(Fertility ~ . , data = swiss))
}
\keyword{datasets}
