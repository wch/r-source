\name{maxCol}
\alias{max.col}
\title{Find Maximum Position in Matrix}
\description{
  Find the maximum position for each row of a matrix, breaking ties at random.
}
\usage{
max.col(m, ties.method=c("random", "first", "last"))
}
\arguments{
  \item{m}{numerical matrix}
  \item{ties.method}{a character string specifying how \dQuote{ties} are
    handled, \code{"random"} by default; can be abbreviated; see
    \emph{Details}.}
}
\value{index of a maximal value for each row, an integer vector of
  length \code{nrow(m)}.
}
\details{
  When \code{ties.method = "random"}, as per default, ties are broken at
  random.  In this case, the determination of \dQuote{tie} assumes that
  the entries are probabilities: there is a relative tolerance of
  \eqn{10^{-5}}{1e-5}, relative to the largest (in magnitude, omitting
  infinity) entry in the row.

  If \code{ties.method = "first"}, \code{max.col} returns the
  column number of the \emph{first} of several maxima in every row, the
  same   as \code{\link{unname}(\link{apply}(m, 1, \link{which.max}))}.\cr
  Correspondingly, \code{ties.method = "last"} returns the \emph{last}
  of possibly several indices.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.}
  New York: Springer (4th ed).
}
\seealso{\code{\link{which.max}} for vectors.
}
\examples{
table(mc <- max.col(swiss))# mostly "1" and "5", 5 x "2" and once "4"
swiss[unique(print(mr <- max.col(t(swiss)))) , ] # 3 33 45 45 33 6

set.seed(1)# reproducible example:
(mm <- rbind(x = round(2*stats::runif(12)),
             y = round(5*stats::runif(12)),
             z = round(8*stats::runif(12))))
\dontrun{
  [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
x    1    1    1    2    0    2    2    1    1     0     0     0
y    3    2    4    2    4    5    2    4    5     1     3     1
z    2    3    0    3    7    3    4    5    4     1     7     5
}
## column indices of all row maxima :
utils::str(lapply(1:3, function(i) which(mm[i,] == max(mm[i,]))))
max.col(mm) ; max.col(mm) # "random"
max.col(mm, "first")# -> 4 6 5
max.col(mm, "last") # -> 7 9 11
\dontshow{
stopifnot(max.col(mm, "first") == c(4, 6, 5),
          max.col(mm, "last")  == c(7, 9,11))
}
}
\keyword{utilities}
\keyword{array}
