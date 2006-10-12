\name{plotmath}
\alias{plotmath}
\title{Mathematical Annotation in R}
\description{
  If the \code{text} argument to one of the text-drawing functions
  (\code{\link{text}}, \code{\link{mtext}}, \code{\link{axis}},
  \code{\link{legend}}) in \R is an expression, the argument is
  interpreted as a mathematical expression and the output will be
  formatted according to TeX-like rules.  Expressions can also be used
  for titles, subtitles and x- and y-axis labels (but not for axis
  labels on \code{persp} plots).

  In most cases other language objects (names and calls) are coerced to
  expressions and so can also be used.
}
\details{
  A mathematical expression must obey the normal rules of syntax for any
  \R expression, but it is interpreted according to very different rules
  than for normal \R expressions.

  It is possible to produce many different mathematical symbols, generate
  sub- or superscripts, produce fractions, etc.

  The output from \code{demo(plotmath)} includes several tables which
  show the available features.  In these tables, the columns of grey text
  show sample \R expressions, and the columns of black text show the
  resulting output.

  The available features are also described in the tables below:

  \tabular{ll}{
    \bold{Syntax} \tab \bold{Meaning} \cr

    \code{x + y}   \tab x plus y   \cr
    \code{x - y}   \tab x minus y \cr
    \code{x*y}    \tab juxtapose x and y \cr
    \code{x/y}    \tab x forwardslash y \cr
    \code{x \%+-\% y}   \tab x plus or minus y \cr
    \code{x \%/\% y}   \tab x divided by y \cr
    \code{x \%*\% y}   \tab x times y \cr
    \code{x \%.\% y}   \tab x cdot y \cr
    \code{x[i]}   \tab x subscript i \cr
    \code{x^2}    \tab x superscript 2 \cr
    \code{paste(x, y, z)} \tab juxtapose x, y, and z \cr
    \code{sqrt(x)}   \tab square root of x \cr
    \code{sqrt(x, y)}   \tab yth root of x \cr
    \code{x == y}   \tab x equals y \cr
    \code{x != y}   \tab x is not equal to y \cr
    \code{x < y}   \tab x is less than y \cr
    \code{x <= y}   \tab x is less than or equal to y \cr
    \code{x > y}   \tab x is greater than y \cr
    \code{x >= y}   \tab x is greater than or equal to y \cr
    \code{x \%~~\% y}   \tab x is approximately equal to y \cr
    \code{x \%=~\% y}   \tab x and y are congruent \cr
    \code{x \%==\% y}   \tab x is defined as y \cr
    \code{x \%prop\% y}  \tab x is proportional to y \cr
    \code{plain(x)}   \tab draw x in normal font \cr
    \code{bold(x)}   \tab draw x in bold font \cr
    \code{italic(x)}   \tab draw x in italic font \cr
    \code{bolditalic(x)} \tab draw x in bolditalic font \cr
    \code{list(x, y, z)} \tab comma-separated list \cr
    \code{...}    \tab ellipsis (height varies) \cr
    \code{cdots}   \tab ellipsis (vertically centred) \cr
    \code{ldots}   \tab ellipsis (at baseline) \cr
    \code{x \%subset\% y} \tab x is a proper subset of y \cr
    \code{x \%subseteq\% y} \tab x is a subset of y \cr
    \code{x \%notsubset\% y} \tab x is not a subset of y \cr
    \code{x \%supset\% y} \tab x is a proper superset of y \cr
    \code{x \%supseteq\% y} \tab x is a superset of y \cr
    \code{x \%in\% y}   \tab x is an element of y \cr
    \code{x \%notin\% y} \tab x is not an element of y \cr
    \code{hat(x)}   \tab x with a circumflex \cr
    \code{tilde(x)}   \tab x with a tilde \cr
    \code{dot(x)} \tab x with a dot \cr
    \code{ring(x)}   \tab x with a ring \cr
    \code{bar(xy)}   \tab xy with bar \cr
    \code{widehat(xy)}   \tab xy with a wide circumflex \cr
    \code{widetilde(xy)} \tab xy with a wide tilde \cr
    \code{x \%<->\% y}   \tab x double-arrow y \cr
    \code{x \%->\% y}   \tab x right-arrow y \cr
    \code{x \%<-\% y}   \tab x left-arrow y \cr
    \code{x \%up\% y}   \tab x up-arrow y \cr
    \code{x \%down\% y}  \tab x down-arrow y \cr
    \code{x \%<=>\% y}   \tab x is equivalent to y \cr
    \code{x \%=>\% y}   \tab x implies y \cr
    \code{x \%<=\% y}   \tab y implies x \cr
    \code{x \%dblup\% y}   \tab x double-up-arrow y \cr
    \code{x \%dbldown\% y} \tab x double-down-arrow y \cr
    \code{alpha} -- \code{omega} \tab Greek symbols \cr
    \code{Alpha} -- \code{Omega} \tab uppercase Greek symbols \cr
    \code{theta1, phi1, sigma1, omega1} \tab cursive Greek symbols\cr
    \code{Upsilon1} \tab capital upsilon with hook\cr
    \code{infinity}   \tab infinity symbol \cr
    \code{partialdiff} \tab partial differential symbol \cr
    \code{32*degree}   \tab 32 degrees \cr
    \code{60*minute}   \tab 60 minutes of angle \cr
    \code{30*second}   \tab 30 seconds of angle \cr
    \code{displaystyle(x)} \tab draw x in normal size (extra spacing) \cr
    \code{textstyle(x)}  \tab draw x in normal size \cr
    \code{scriptstyle(x)} \tab draw x in small size \cr
    \code{scriptscriptstyle(x)} \tab draw x in very small size \cr
    \code{underline(x)}   \tab draw x underlined\cr
    \code{x ~~ y}        \tab put extra space between x and y \cr
    \code{x + phantom(0) + y} \tab leave gap for "0", but don't draw it \cr
    \code{x + over(1, phantom(0))} \tab leave vertical gap for "0" (don't draw) \cr
    \code{frac(x, y)}   \tab x over y \cr
    \code{over(x, y)}   \tab x over y \cr
    \code{atop(x, y)}   \tab x over y (no horizontal bar) \cr
    \code{sum(x[i], i==1, n)} \tab sum x[i] for i equals 1 to n \cr
    \code{prod(plain(P)(X==x), x)} \tab product of P(X=x) for all values of x \cr
    \code{integral(f(x)*dx, a, b)} \tab definite integral of f(x) wrt x \cr
    \code{union(A[i], i==1, n)} \tab union of A[i] for i equals 1 to n \cr
    \code{intersect(A[i], i==1, n)} \tab intersection of A[i] \cr
    \code{lim(f(x), x \%->\% 0)} \tab limit of f(x) as x tends to 0 \cr
    \code{min(g(x), x > 0)} \tab minimum of g(x) for x greater than 0 \cr
    \code{inf(S)}        \tab infimum of S \cr
    \code{sup(S)}   \tab supremum of S \cr
    \code{x^y + z}   \tab normal operator precedence \cr
    \code{x^(y + z)}    \tab visible grouping of operands \cr
    \code{x^{y + z}}  \tab invisible grouping of operands \cr
    \code{group("(",list(a, b),"]")} \tab specify left and right delimiters \cr
    \code{bgroup("(",atop(x,y),")")} \tab use scalable delimiters \cr
    \code{group(lceil, x, rceil)} \tab special delimiters \cr
  }

  Note to TeX users: TeX's \code{\Upsilon} is \code{Upsilon1}, TeX's
  \code{\varepsilon} is close to \code{epsilon}, and there is no
  equivalent of TeX's \code{\epsilon}.  TeX's \code{\varpi} is close to
  \code{omega1}.  \code{vartheta}, \code{varphi} and \code{varsigma} are
  allowed as synonyms for \code{theta1}, \code{phi1} and \code{sigma1}.

  \code{sigma1} is also known as \code{stigma}, its Unicode name.

  Control characters (e.g. \code{\\n}) are not interpreted in character
  strings in plotmath, unlike normal plotting.
}
\references{
  Murrell, P. and Ihaka, R. (2000) An approach to providing
  mathematical annotation in plots.
  \emph{Journal of Computational and Graphical Statistics},
  \bold{9}, 582--599.
}
\seealso{
  \code{demo(plotmath)},
  \code{\link{axis}},
  \code{\link{mtext}},
  \code{\link{text}},
  \code{\link{title}},
  \code{\link{substitute}}
  \code{\link{quote}}, \code{\link{bquote}}
}
\examples{
x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))


## How to combine "math" and numeric variables :
plot(1:10, type="n", xlab="", ylab="", main = "plot math & numbers")
theta <- 1.23 ; mtext(bquote(hat(theta) == .(theta)))
for(i in 2:9)
    text(i,i+1, substitute(list(xi,eta) == group("(",list(x,y),")"),
                           list(x=i, y=i+1)))
## note that both of these use calls rather than expressions.

plot(1:10, 1:10)
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
     cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))",
     cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                            plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
     cex = 1.2)
}
\keyword{aplot}
