\name{medpolish}
\title{Median Polish of a Matrix}
\usage{
medpolish(x, eps=0.01, maxiter=10)

plot(medpolish.obj)
print(medpolish.obj)
}
\alias{plot.medpolish}
\alias{print.medpolish}
\description{
\code{medpolish} fits an additive
constant-plus-rows-plus-columns
model to the values given in \code{x} using
Tukey's \emph{median polish} procedure.

Sweeping out for row and column effects continues
until the proportional reduction in the sum
of absolute residuals is less than \code{eps}
or until the have been \code{maxiter} iterations.
The sum of absolute residuals is printed at
each iteration of the fitting process.

The value returned by \code{medpolish} is an
object of class \code{medpolish}.
Printing and plotting methods are defined for this
class, and can be invoked via the generic functions
\code{print} and \code{plot}.
}
\value{
An object of class \code{medpolish}
with the following named components:
\item{overall}{the fitted constant term.}
\item{row}{the fitted row effects.}
\item{col}{the fitted column effects.}
\item{residuals}{the residuals.}
\item{name}{the name of the dataset.}
}
