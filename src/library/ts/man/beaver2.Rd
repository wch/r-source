% file beaver2.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{beaver2}
\alias{beaver2}
\title{
Body Temperature Series of Beaver 2
}
\usage{data(beavers)}
\description{
The \code{beaver2} data frame has 100 rows and 4 columns on body
temperature measurements at 10 minute intervals.
}
\format{
Reynolds (1994) describes a
small part of a study of the long-term temperature dynamics of beaver
\emph{Castor canadensis}
in north-central Wisconsin.  Body temperature was measured by
telemetry every 10 minutes for four females, but data from a one
period of less than a day for each of two animals is used there.


This data frame contains the following columns:
}
\arguments{
\item{day}{
Day of observation (in days since the beginning of 1990),
November 3-4.
}
\item{time}{
Time of observation, in the form \code{0330} for 3.30am
}
\item{temp}{
Measured body temperature in degrees Celcius
}
\item{activ}{
Indicator of activity outside the retreat
}}
\source{
P. S. Reynolds (1994) Time-series analyses of beaver body temperatures.
Chapter 11 of
Lange, N., Ryan, L., Billard, L., Brillinger, D., Conquest, L. 
and Greenhouse, J. eds (1994)
\emph{Case Studies in Biometry.}
New York: John Wiley and Sons.
}
\seealso{
\code{\link{beaver1}}
}
\keyword{datasets}
