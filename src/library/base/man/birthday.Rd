\name{birthday}
\alias{qbirthday}
\alias{pbirthday}
\title{Probability of coincidences}
\description{
Computes approximate answers to a generalised "birthday
paradox" problem. \code{pbirthday} computes the probability of a
coincidence and \code{qbirthday} computes the number of observations
needed to have a specified probability of coincidence.
}
\usage{
pbirthday(prob=0.5,classes=365, coincident=2)
qbirthday(n,classes=365,  coincident=2)
}
\arguments{
  \item{classes}{How many distinct categories the people could fall into}
  \item{prob}{The desired probability of coincidence}
  \item{coincident}{The number of people to fall in the same category}
}
\value{
  Number of people needed for a probability \code{prob} that \code{k} of 
  them have the same one out of \code{classes} equiprobable labels.
}
\details{
 The birthday paradox is that a very small number of
people, 23, suffices to have  a 50-50 chance that two of them have the
same birthday.  This function generalises the calculation to
probabilities other than 0.5, numbers of coincident events other than 2,
and numbers of classes other than 365. 
}
\references{Diaconis P, Mosteller F. "Methods for studying coincidences" 
JASA 84:853-861}
\examples{
 ## the standard version
qbirthday() 
 ## same 4-digit PIN number 
qbirthday(classes=10^4)
 ## 0.9 probability of three coincident birthdays
qbirthday(coincident=3,prob=0.9)
## Chance of 4 coincident birthdays in 150 people
pbirthday(150,coincident=4)
}
\keyword{distribution}%-- one or more ...
