\name{birthday}
\alias{qbirthday}
\alias{pbirthday}
\title{Probability of coincidences}
\description{
  Computes approximate answers to a generalised \dQuote{birthday paradox}
  problem. \code{pbirthday} computes the probability of a coincidence
  and \code{qbirthday} computes the number of observations needed to
  have a specified probability of coincidence.
}
\usage{
qbirthday(prob = 0.5, classes = 365, coincident = 2)
pbirthday(n, classes = 365, coincident = 2)
}
\arguments{
  \item{classes}{How many distinct categories the people could fall into}
  \item{prob}{The desired probability of coincidence}
  \item{n}{The number of people}
  \item{coincident}{The number of people to fall in the same category}
}
\value{
  \item{qbirthday}{
    Number of people needed for a probability \code{prob} that \code{k} of
    them have the same one out of \code{classes} equiprobable labels.
  }

  \item{pbirthday}{Probability of the specified coincidence}
}
\details{
 The birthday paradox is that a very small number of people, 23,
 suffices to have  a 50-50 chance that two of them have the same
 birthday.  This function generalises the calculation to probabilities
 other than 0.5, numbers of coincident events other than 2, and numbers
 of classes other than 365.

 This formula is approximate, as the example below shows.  For
 \code{coincident=2} the exact computation is straightforward and may be
 preferable.
}
\references{
  Diaconis P, Mosteller F., \dQuote{Methods for studying coincidences}.
  JASA 84:853-861}
\examples{
 ## the standard version
qbirthday()
 ## same 4-digit PIN number
qbirthday(classes=10^4)
 ## 0.9 probability of three coincident birthdays
qbirthday(coincident=3, prob=0.9)
## Chance of 4 coincident birthdays in 150 people
pbirthday(150,coincident=4)
## 100 coincident birthdays in 1000 people: *very* rare:
pbirthday(1000, coincident=100)

## Accuracy compared to exact calculation
x1<-  sapply(10:100, pbirthday)
x2<- 1-sapply(10:100, function(n)prod((365:(365-n+1))/rep(365,n)))
par(mfrow=c(2,2))
plot(x1, x2, xlab="approximate", ylab="exact")
abline(0,1)
plot(x1, x1-x2, xlab="approximate", ylab="error")
abline(h=0)
plot(x1, x2, log="xy", xlab="approximate", ylab="exact")
abline(0,1)
plot(1-x1, 1-x2, log="xy", xlab="approximate", ylab="exact")
abline(0,1)
}
\keyword{distribution}
