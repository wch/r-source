% File src/library/stats/man/birthday.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{birthday}
\alias{qbirthday}
\alias{pbirthday}
\title{Probability of coincidences}
\description{
  Computes answers to a generalised \emph{birthday paradox} problem.
  \code{pbirthday} computes the probability of a coincidence and
  \code{qbirthday} computes the smallest number of observations needed
  to have at least a specified probability of coincidence.
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
    Minimum number of people needed for a probability of at least
    \code{prob} that \code{k} or more of them have the same one out of
    \code{classes} equiprobable labels.
  }

  \item{pbirthday}{Probability of the specified coincidence.}
}
\details{
 The birthday paradox is that a very small number of people, 23,
 suffices to have a 50--50 chance that two or more of them have the same
 birthday.  This function generalises the calculation to probabilities
 other than 0.5, numbers of coincident events other than 2, and numbers
 of classes other than 365.

 The formula used is approximate for \code{coincident > 2}.  The
 approximation is very good for moderate values of \code{prob} but less
 good for very small probabilities.
}
\references{
  Diaconis, P. and Mosteller F. (1989)
  Methods for studying coincidences.
  \emph{J. American Statistical Association}, \bold{84}, 853--861.
}
\examples{
require(graphics)

## the standard version
qbirthday() # 23
## probability of > 2 people with the same birthday
pbirthday(23, coincident = 3)

## examples from Diaconis & Mosteller p. 858.
## 'coincidence' is that husband, wife, daughter all born on the 16th
qbirthday(classes = 30, coincident = 3) # approximately 18
qbirthday(coincident = 4)  # exact value 187
qbirthday(coincident = 10) # exact value 1181

## same 4-digit PIN number
qbirthday(classes = 10^4)

## 0.9 probability of three or more coincident birthdays
qbirthday(coincident = 3, prob = 0.9)

## Chance of 4 or more coincident birthdays in 150 people
pbirthday(150, coincident = 4)

## 100 or more coincident birthdays in 1000 people: very rare
pbirthday(1000, coincident = 100)
}
\keyword{distribution}
