\name{symnum}
\title{Symbolic Number Coding}
\usage{
symnum(x, cutpoints=c(0.3, 0.6, 0.8, 0.9, 0.95),
          symbols=c(" ", ".", ",", "+", "*", "B"), na="?", eps=1e-5,
       corr = TRUE, show.max = if(corr)"1", show.min = NULL,
       lower.triangular = corr & is.matrix(x))

}
\alias{symnum}
\arguments{
  \item{x}{numeric vector or array.}
  \item{cutpoints}{numeric vector whose values \code{cutpoints[j]}%
    \eqn{= c_j}{ == c[j]}  (\emph{after} augmentation, see \code{corr}
    below) are used for intervals.}
  \item{symbols}{character vector, one shorter than (the
    \emph{augmented}, see \code{corr} below) \code{cutpoints}.
    \code{symbols[j]}\eqn{= s_j}{ == s[j]} are used as ``code'' for
    the (half open) interval \eqn{(c_j,c_{j+1}]}{(c[j], c[j+1]]}.}
  \item{na}{character. How \code{\link{NA}s} are coded.}
  \item{eps}{absolute precision to be used at left and right boundary.}
  \item{corr}{logical.  If \code{TRUE}, \code{x} contains correlations.
    The cutpoints are augmented by \code{0} and \code{1} and  \code{abs(x)}
    is coded.}
  \item{show.max}{If \code{TRUE}, or of mode \code{character}, the
    maximal cutpoint is coded especially.}
  \item{show.min}{If \code{TRUE}, or of mode \code{character}, the
    minmal cutpoint is coded especially.}
  \item{lower.triangular}{logical. If \code{TRUE} and \code{x} is a
    matrix, only the \emph{lower triangular} part of the matrix is coded
    as non-blank.}
}
\value{
  An atomic character object of class \code{\link{noquote}} and the same
  dimensions as \code{x}.
  It has an \code{\link{attribute}}, \code{"legend"} containing a legend
  of the returned character codes, in the form
  \deqn{c_1 s_1 c_2 s_2 \dots s_n c_{n+1}}{% the `..' are UGLY in LaTeX Math
    c[1] `s[1]' c[2] `s[2]' \dots `s[n]' c_[n+1]}
  where \eqn{c_j}{c[j]}\code{ = cutpoints[j]} and
  \eqn{s_j}{s[j]}\code{ = symbols[j]}.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{\code{\link{as.character}}}
\examples{
symnum(1:7, cut= 2*(0:4), sym = c(".", "-", "+", "$"), corr=F)
symnum(0:8, cut= 2*(0:4), sym = c(".", "-", "+", "$"), corr=F, show.max=T)

##-- Symbolic correlation matrices:
symnum(cor(rbind(1, rnorm(25), rnorm(25)^2)))
symnum(cor(matrix(rexp(30, 1), 5, 18))) # <<-- PATTERN ! --
symnum(cor(matrix(rnorm(90) ,  5, 18))) # < White Noise SMALL n
symnum(cor(matrix(rnorm(900), 50, 18))) # < White Noise "BIG" n

## NA's:
Cm <- cor(matrix(rnorm(60),  10, 6)); Cm[c(3,6), 2] <- NA
symnum(Cm, show.max=NULL)

## Significance stars for P-values:
pval <- rev(sort(c(outer(1:6,10^-(1:3)))))
symp <- symnum(pval, corr=F,
		cutpoints = c(0,  .001,.01,.05, .1, 1),
		symbols   =  c("***","**","*","."," "))
noquote(cbind(P.val = format(pval), Signif= symp))
}
\keyword{utilities}
\keyword{character}
