% File src/library/grDevices/man/boxplot.stats.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{boxplot.stats}
\title{Box Plot Statistics}
\usage{
boxplot.stats(x, coef = 1.5, do.conf = TRUE, do.out = TRUE)
}
\alias{boxplot.stats}
\arguments{
  \item{x}{a numeric vector for which the boxplot will
    be constructed (\code{\link{NA}}s and \code{\link{NaN}}s are allowed
    and omitted).}
  \item{coef}{this determines how far the plot \sQuote{whiskers} extend out
    from the box.  If \code{coef} is positive, the whiskers extend to the
    most extreme data point which is no more than \code{coef} times
    the length of the box away from the box. A value of zero causes
    the whiskers
    to extend to the data extremes (and no outliers be returned).}
  \item{do.conf, do.out}{logicals; if \code{FALSE}, the \code{conf} or
    \code{out} component respectively will be empty in the result.}
}
\description{
  This function is typically called by another function to
  gather the statistics necessary for producing box plots,
  but may be invoked separately.
}
\value{
  List with named components as follows:
  \item{stats}{a vector of length 5, containing the extreme of the
    lower whisker, the lower \sQuote{hinge}, the median, the upper
    \sQuote{hinge} and the extreme of the upper whisker.}
  \item{n}{the number of non-\code{NA} observations in the sample.}
  \item{conf}{the lower and upper extremes of the \sQuote{notch}
    (\code{if(do.conf)}). See the details.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers (\code{if(do.out)}).}

  Note that \code{$stats} and \code{$conf} are sorted in \emph{in}creasing
  order, unlike S, and that \code{$n} and \code{$out} include any
  \code{+- Inf} values.
}
\details{
  The two \sQuote{hinges} are versions of the first and third quartile,
  i.e., close to \code{\link{quantile}(x, c(1,3)/4)}.  The hinges equal
  the quartiles for odd \eqn{n} (where \code{n <- length(x)}) and
  differ for even \eqn{n}.  Whereas the quartiles only equal observations
  for \code{n \%\% 4 == 1} (\eqn{n\equiv 1 \bmod 4}{n = 1 mod 4}),
  the hinges do so \emph{additionally} for \code{n \%\% 4 == 2}
  (\eqn{n\equiv 2 \bmod 4}{n = 2 mod 4}), and are in the middle of
  two observations otherwise.

  The notches (if requested) extend to \code{+/-1.58 IQR/sqrt(n)}.
  This seems to be based on the same calculations as the formula with 1.57 in
  Chambers \emph{et al} (1983, p.\sspace{}62), given in McGill \emph{et al}
  (1978, p.\sspace{}16).  They are based on asymptotic normality of the median
  and roughly equal sample sizes for the two medians being compared, and
  are said to be rather insensitive to the underlying distributions of
  the samples.  The idea appears to be to give roughly a 95\% confidence
  interval for the difference in two medians.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis}.
  Section 2C.

  McGill, R., Tukey, J. W. and Larsen, W. A. (1978).
  Variations of box plots.
  \emph{The American Statistician}, \bold{32}, 12--16.
  \doi{10.2307/2683468}.

  Velleman, P. F. and Hoaglin, D. C. (1981).
  \emph{Applications, Basics and Computing of Exploratory Data Analysis}.
  Duxbury Press.

  Emerson, J. D and Strenio, J. (1983).
  Boxplots and batch comparison.
  Chapter 3 of \emph{Understanding Robust and Exploratory Data
    Analysis}, eds. D. C. Hoaglin, F. Mosteller and J. W. Tukey.  Wiley.

  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983).
  \emph{Graphical Methods for Data Analysis}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{fivenum}},
  \code{\link{boxplot}},
  \code{\link{bxp}}.
}
\examples{
require(stats)
x <- c(1:100, 1000)
(b1 <- boxplot.stats(x))
(b2 <- boxplot.stats(x, do.conf = FALSE, do.out = FALSE))
stopifnot(b1 $ stats == b2 $ stats) # do.out = FALSE is still robust
boxplot.stats(x, coef = 3, do.conf = FALSE)
## no outlier treatment:
boxplot.stats(x, coef = 0)

boxplot.stats(c(x, NA)) # slight change : n is 101
(r <- boxplot.stats(c(x, -1:1/0)))
stopifnot(r$out == c(1000, -Inf, Inf))

%% extended example (for the NG of Rdoc):
\dontshow{
 ## Difference between quartiles and hinges :
 nn <- 1:17 ;  n4 <- nn \%\% 4
 hin <- sapply(sapply(nn, seq), function(x) boxplot.stats(x)$stats[c(2,4)])
 q13 <- sapply(sapply(nn, seq), quantile, probs = c(1,3)/4, names = FALSE)
 m <- t(rbind(q13,hin))[, c(1,3,2,4)]
 dimnames(m) <- list(paste(nn), c("q1","lH", "q3","uH"))
 stopifnot(m[n4 == 1, 1:2] == (nn[n4 == 1] + 3)/4,   # quart. = hinge
           m[n4 == 1, 3:4] == (3*nn[n4 == 1] + 1)/4,
           m[,"lH"] == ( (nn+3) \%/\% 2) / 2,
           m[,"uH"] == ((3*nn+2)\%/\% 2) / 2)
 cm <- noquote(format(m))
 cm[m[,2] == m[,1], 2] <- " = "
 cm[m[,4] == m[,3], 4] <- " = "
 cm
}

}
\keyword{dplot}
