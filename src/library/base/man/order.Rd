% File src/library/base/man/order.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{order}
\title{Ordering Permutation}
\alias{order}
\alias{sort.list}
\concept{sort data frame}
\description{
  \code{order} returns a permutation which rearranges its first
  argument into ascending or descending order, breaking ties by further
  arguments. \code{sort.list} is the same, using only one argument.\cr
  See the examples for how to use these functions to sort data frames,
  etc.
}
\usage{
order(\dots, na.last = TRUE, decreasing = FALSE,
      method = c("shell", "radix"))

sort.list(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
          method = c("shell", "quick", "radix"))
}
\arguments{
  \item{\dots}{a sequence of numeric, complex, character or logical
    vectors, all of the same length, or a classed \R object.}
  \item{x}{an atomic vector.}
  \item{partial}{vector of indices for partial sorting.
    (Non-\code{NULL} values are not implemented.)}
  \item{decreasing}{logical.  Should the sort order be increasing or
    decreasing? For the \code{"radix"} method, this can be a vector of
    length equal to the number of arguments in \code{\dots}. For the
    other methods, it must be length one.}
  \item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed
    (see \sQuote{Note}.)}
  \item{method}{the method to be used: partial matches are allowed.  The
    default is \code{"shell"} except for some special cases: see
    \sQuote{Details}.  For details of methods \code{"shell"}, 
    \code{"quick"}, and \code{"radix"},
    see the help for \code{\link{sort}}.}
}
\details{
  In the case of ties in the first vector, values in the second are used
  to break the ties.  If the values are still tied, values in the later
  arguments are used to break the tie (see the first example).
  The sort used is \emph{stable} (except for \code{method = "quick"}),
  so any unresolved ties will be left in their original ordering.

  Complex values are sorted first by the real part, then the imaginary
  part.

  Except for method \code{"radix"}, the sort order for character vectors
  will depend on the collating sequence of the locale in use: see
  \code{\link{Comparison}}.

  The \code{"shell"} method is generally the safest bet and is the
  default method, except for factors and integer vectors, where
  \code{"radix"} is assumed. Method \code{"radix"} stably sorts logical,
  numeric and character vectors in linear time. It outperforms the other
  methods, although there are caveats (see \code{\link{sort}}).  Method
  \code{"quick"} for \code{sort.list} is only supported for numeric
  \code{x} with \code{na.last = NA}, is not stable, and is slower than
  \code{"radix"}. The \code{"radix"} method has less precision when
  sorting real-valued numbers.
  
  \code{partial = NULL} is supported for compatibility with other
  implementations of S, but no other values are accepted and ordering is
  always complete.

  For a classed \R object, the sort order is taken from
  \code{\link{xtfrm}}: as its help page notes, this can be slow unless a
  suitable method has been defined or \code{\link{is.numeric}(x)} is
  true.  For factors, this sorts on the internal codes, which is
  particularly appropriate for ordered factors.
}

\value{
  An integer vector unless any of the inputs has \eqn{2^{31}}{2^31} or
  more elements, when it is a double vector.
}

\section{Warning}{
  In programmatic use it is unsafe to name the \code{\dots} arguments,
  as the names could match current or future control
  arguments such as \code{decreasing}.  A sometimes-encountered unsafe
  practice is to call \code{do.call('order', df_obj)} where
  \code{df_obj} might be a data frame: copy \code{df_obj} and
  remove any names, for example using \code{\link{unname}}.
}

\note{
  \code{sort.list} can get called by mistake as a method for
  \code{\link{sort}} with a list argument: it gives a suitable error
  message for list \code{x}.

  There is a historical difference in behaviour for \code{na.last = NA}:
  \code{sort.list} removes the \code{NA}s and then computes the order
  amongst the remaining elements: \code{order} computes the order
  amongst the non-\code{NA} elements of the original vector.  Thus
\preformatted{   x[order(x, na.last = NA)]
   zz <- x[!is.na(x)]; zz[sort.list(x, na.last = NA)]
}
  both sort the non-\code{NA} values of \code{x}.

  Prior to \R 3.1.0 \code{method = "radix"} was only supported for
  non-negative integers. Prior to \R 3.3.0 it was only supported for
  integers of range less than 100,000.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Knuth, D. E. (1998)
  \emph{The Art of Computer Programming, Volume 3: Sorting and
    Searching.} 2nd ed. Addison-Wesley.
}
\seealso{
  \code{\link{sort}}, \code{\link{rank}}, \code{\link{xtfrm}}.
}
\examples{
require(stats)

(ii <- order(x <- c(1,1,3:1,1:4,3), y <- c(9,9:1), z <- c(2,1:9)))
## 6  5  2  1  7  4 10  8  3  9
rbind(x, y, z)[,ii] # shows the reordering (ties via 2nd & 3rd arg)

## Suppose we wanted descending order on y.
## A simple solution for numeric 'y' is
rbind(x, y, z)[, order(x, -y, z)]
## More generally we can make use of xtfrm
cy <- as.character(y)
rbind(x, y, z)[, order(x, -xtfrm(cy), z)]
## The radix sort supports multiple 'decreasing' values:
rbind(x, y, z)[, order(x, cy, z, decreasing = c(FALSE, TRUE, FALSE),
                       method="radix")]

## Sorting data frames:
dd <- transform(data.frame(x, y, z),
                z = factor(z, labels = LETTERS[9:1]))
## Either as above {for factor 'z' : using internal coding}:
dd[ order(x, -y, z), ]
## or along 1st column, ties along 2nd, ... *arbitrary* no.{columns}:
dd[ do.call(order, dd), ]

set.seed(1)  # reproducible example:
d4 <- data.frame(x = round(   rnorm(100)), y = round(10*runif(100)),
                 z = round( 8*rnorm(100)), u = round(50*runif(100)))
(d4s <- d4[ do.call(order, d4), ])
(i <- which(diff(d4s[, 3]) == 0))
#   in 2 places, needed 3 cols to break ties:
d4s[ rbind(i, i+1), ]

## rearrange matched vectors so that the first is in ascending order
x <- c(5:1, 6:8, 12:9)
y <- (x - 5)^2
o <- order(x)
rbind(x[o], y[o])

## tests of na.last
a <- c(4, 3, 2, NA, 1)
b <- c(4, NA, 2, 7, 1)
z <- cbind(a, b)
(o <- order(a, b)); z[o, ]
(o <- order(a, b, na.last = FALSE)); z[o, ]
(o <- order(a, b, na.last = NA)); z[o, ]

\donttest{
##  speed examples on an average laptop for long vectors:
##  factor/small-valued integers:
x <- factor(sample(letters, 1e7, replace = TRUE))
system.time(o <- sort.list(x, method = "quick", na.last = NA)) # 0.1 sec
stopifnot(!is.unsorted(x[o]))
system.time(o <- sort.list(x, method = "radix")) # 0.05 sec, 2X faster
stopifnot(!is.unsorted(x[o]))
##  large-valued integers:
xx <- sample(1:200000, 1e7, replace = TRUE)
system.time(o <- sort.list(xx, method = "quick", na.last = NA)) # 0.3 sec
system.time(o <- sort.list(xx, method = "radix")) # 0.2 sec
##  character vectors:
xx <- sample(state.name, 1e6, replace = TRUE)
system.time(o <- sort.list(xx, method = "shell")) # 2 sec
system.time(o <- sort.list(xx, method = "radix")) # 0.007 sec, 300X faster
##  double vectors:
xx <- rnorm(1e6)
system.time(o <- sort.list(xx, method = "shell")) # 0.4 sec
system.time(o <- sort.list(xx, method = "quick", na.last = NA)) # 0.1 sec
system.time(o <- sort.list(xx, method = "radix")) # 0.05 sec, 2X faster
}}
\keyword{univar}
\keyword{manip}
