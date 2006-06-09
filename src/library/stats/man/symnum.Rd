\name{symnum}
\alias{symnum}
\title{Symbolic Number Coding}
\concept{matrix visualization}
\description{
  Symbolically encode a given numeric or logical vector or array.
  Particularly useful for \dQuote{visualization} of structured matrices,
  e.g., correlation, sparse, or logical ones.
}
\usage{
symnum(x, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
       symbols= if(numeric.x) c(" ", ".", ",", "+", "*", "B") else c(".", "|"),
       legend = length(symbols) >= 3,
       na = "?", eps = 1e-5, numeric.x = is.numeric(x),
       corr = missing(cutpoints) && numeric.x,
       show.max = if(corr) "1", show.min = NULL,
       abbr.colnames = has.colnames,
       lower.triangular = corr && is.numeric(x) && is.matrix(x),
       diag.lower.tri   = corr && !is.null(show.max))
}
\arguments{
  \item{x}{numeric or logical vector or array.}
  \item{cutpoints}{numeric vector whose values \code{cutpoints[j]}%
    \eqn{= c_j}{ == c[j]}  (\emph{after} augmentation, see \code{corr}
    below) are used for intervals.}
  \item{symbols}{character vector, one shorter than (the
    \emph{augmented}, see \code{corr} below) \code{cutpoints}.
    \code{symbols[j]}\eqn{= s_j}{ == s[j]} are used as \dQuote{code} for
    the (half open) interval \eqn{(c_j,c_{j+1}]}{(c[j], c[j+1]]}.

    When \code{numeric.x} is \code{FALSE}, i.e., by default when
    argument \code{x} is \code{logical}, the default is
    \code{c(".","|")} (graphical 0 / 1 s).}
  \item{legend}{logical indicating if a \code{"legend"} attribute is
    desired.}
  \item{na}{character or logical. How \code{\link{NA}s} are coded.  If
    \code{na == FALSE}, \code{NA}s are coded invisibly, \emph{including}
    the \code{"legend"} attribute below, which otherwise mentions NA
    coding.}
  \item{eps}{absolute precision to be used at left and right boundary.}
  \item{numeric.x}{logical indicating if \code{x} should be treated as numbers,
    otherwise as logical.}
  \item{corr}{logical.  If \code{TRUE}, \code{x} contains correlations.
    The cutpoints are augmented by \code{0} and \code{1} and
    \code{abs(x)} is coded.}
  \item{show.max}{if \code{TRUE}, or of mode \code{character}, the
    maximal cutpoint is coded especially.}
  \item{show.min}{if \code{TRUE}, or of mode \code{character}, the
    minimal cutpoint is coded especially.}
  \item{abbr.colnames}{logical, integer or \code{NULL} indicating how
    column names should be abbreviated (if they are); if \code{NULL}
    (or \code{FALSE} and \code{x} has no column names),
    the column names will all be empty, i.e., \code{""}; otherwise if
    \code{abbr.colnames} is false, they are left unchanged.  If
    \code{TRUE} or integer, existing column names will be abbreviated to
    \code{\link{abbreviate}(*, minlength = abbr.colnames)}.}
  \item{lower.triangular}{logical.  If \code{TRUE} and \code{x} is a
    matrix, only the \emph{lower triangular} part of the matrix is coded
    as non-blank.}
  \item{diag.lower.tri}{logical.  If \code{lower.triangular} \emph{and}
    this are \code{TRUE}, the \emph{diagonal} part of the matrix is
    shown.}
}
\note{
  The optional (mostly logical) arguments all try to use smart defaults.
  Specifying them explicitly may lead to considerably improved output in
  many cases.
}
\value{
  An atomic character object of class \code{\link{noquote}} and the same
  dimensions as \code{x}.

  If \code{legend} is \code{TRUE} (as by default when there are more
  than two classes), the result has an attribute \code{"legend"}
  containing a legend of the returned character codes, in the form
  \deqn{c_1 s_1 c_2 s_2 \dots s_n c_{n+1}}{% the dots are UGLY in LaTeX Math
    c[1] \sQuote{s[1]} c[2] \sQuote{s[2]} \dots \sQuote{s[n]} c_[n+1]}
  where \eqn{c_j}{c[j]}\code{ = cutpoints[j]} and
  \eqn{s_j}{s[j]}\code{ = symbols[j]}.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{\code{\link{as.character}}; \code{\link[graphics]{image}}}
\examples{
ii <- 0:8; names(ii) <- ii
symnum(ii, cut= 2*(0:4), sym = c(".", "-", "+", "$"))
symnum(ii, cut= 2*(0:4), sym = c(".", "-", "+", "$"), show.max=TRUE)

symnum(1:12 \%\% 3 == 0)# --> "|" = TRUE, "." = FALSE  for logical

## Pascal's Triangle modulo 2 -- odd and even numbers:
N <- 38
pascal <- t(sapply(0:N, function(n) round(choose(n, 0:N - (N-n)\%/\%2))))
rownames(pascal) <- rep("", 1+N) # <-- to improve "graphic"
symnum(pascal \%\% 2, symbols = c(" ", "A"), numeric = FALSE)

##-- Symbolic correlation matrices:
symnum(cor(attitude), diag = FALSE)
symnum(cor(attitude), abbr.= NULL)
symnum(cor(attitude), abbr.= FALSE)
symnum(cor(attitude), abbr.= 2)

symnum(cor(rbind(1, rnorm(25), rnorm(25)^2)))
symnum(cor(matrix(rexp(30, 1), 5, 18))) # <<-- PATTERN ! --
symnum(cm1 <- cor(matrix(rnorm(90) ,  5, 18))) # < White Noise SMALL n
symnum(cm1, diag=FALSE)
symnum(cm2 <- cor(matrix(rnorm(900), 50, 18))) # < White Noise "BIG" n
symnum(cm2, lower=FALSE)

## NA's:
Cm <- cor(matrix(rnorm(60),  10, 6)); Cm[c(3,6), 2] <- NA
symnum(Cm, show.max=NULL)

## Graphical P-values (aka "significance stars"):
pval <- rev(sort(c(outer(1:6, 10^-(1:3)))))
symp <- symnum(pval, corr=FALSE,
               cutpoints = c(0,  .001,.01,.05, .1, 1),
               symbols = c("***","**","*","."," "))
noquote(cbind(P.val = format(pval), Signif= symp))
}
\keyword{utilities}
\keyword{character}
