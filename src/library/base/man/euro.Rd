\name{euro}
\title{Euro Conversion Rates}
\alias{euro}
\alias{euro.cross}
\usage{data(euro)}
\format{\code{euro}: A named vector of length 11.\cr
\code{euro.cross:}: A named matrix of size 11x11.}
\details{The data set \code{euro} contains the value of 1 Euro in
    all currencies participating in the European monetary union
    (Austrian Schilling ATS, Belgian Franc BEF, German Mark DEM, Spanish
    Peseta ESP, Finnish Markka FIM, French Franc FRF, Irish Punt IEP,
    Italian Lira ITL, Luxembourg Franc LUF, Dutch Guilder NLG and
    Portugese Escudo PTE). These conversion rates were fixed by the
    European Union on December 31, 1998. To
    convert old prices to Euro prices, divide by the respective rate and
    round to 2 digits.

    The data set \code{euro.cross} contains conversion rates between the
    various Euro currencies, i.e., the result of
    \code{outer(1 / euro, euro)}.
}
\examples{
data(euro)
cbind(euro)
all(euro == signif(euro,6))# TRUE: [6 digit precision in Euro's definition]

all(euro.cross == outer(1/euro, euro)) # TRUE

## Convert 20.- Euro to Belgian Franc
20 * euro["BEF"]

## Convert 20.- Austrian Schilling to Euro
20 / euro["ATS"]

## Convert 20.- Spanish Pesetas to Italian Lira
20 * euro.cross["ESP", "ITL"]
dotplot(euro, main = "euro data: 1 Euro in currency unit")
dotplot(1/euro, main = "euro data: 1 currency unit in Euros")
dotplot(log(euro, 10), main = "euro data: log10(1 Euro in currency unit)")
}
\keyword{datasets}
