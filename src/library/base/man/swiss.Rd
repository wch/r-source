\name{swiss}
\title{Swiss Fertility and Socioeconomic Indicators (1888) Data}
\usage{data(swiss)}
\alias{swiss}
\format{A data frame with 47 observations on 6 variables, \emph{each} of
  which is in percent, i.e., in \eqn{[0,100]}.

  \tabular{cll}{
      [,1] \tab Fertility \tab \eqn{I_g}{Ig}, ``common standardized
      fertility measure''\cr
      [,2] \tab Agriculture\tab \% involved in agriculture as occupation\cr
      [,3] \tab Examination\tab \% ``draftees'' receiving highest mark\cr
      \tab \tab on army examination\cr
      [,4] \tab Education \tab \% education beyond primary school.\cr
      [,5] \tab Catholic \tab \% catholic (as opposed to "protestant").\cr
      [,6] \tab Infant.Mortality\tab live births who live less than 1
      year.\cr
  }

  All variables but `Fertility' give proportions of the population.
}
\description{
    (paraphrasing Mosteller and Tukey):

    Switzerland, in 1888, was entering a  period known as the
    ``demographic transition''; i.e., its fertility was beginning to
    fall from the high level typical of underdeveloped countries.

    The data collected are for 47 seven French-speaking ``provinces'' at
    about 1888.

    Here, all variables are scaled to \eqn{[0,100]}, where in the
    original, all but \code{"Catholic"} were scaled to \eqn{[0,1]}.
}
\source{
    Project ``16P5'', pp.549-551 in \cr
    Mosteller, F. and Tukey, J. W. (1977).
    \emph{Data Analysis and Regression: A Second Course in Statistics},
    Addison-Wesley, Reading Mass.

    indicating \bold{Source} as\cr
    ``Data used by permission of Franice van de Walle. Office of
    Population Research, Princeton University, 1976.\cr
    Unpublished data assembled under NICHD contract number No
    1-HD-O-2077.''
}
\examples{
data(swiss)
pairs(swiss, panel = panel.smooth, main = "swiss data")
summary(lm(Fertility ~ . , data = swiss))
}
\keyword{datasets}
