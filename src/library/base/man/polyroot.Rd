\name{polyroot}
\title{find zeros of a complex polynomial}
\usage{
polyroot(z)
}
\alias{polyroot}
\arguments{
\item{z}{the vector of polynomial coefficients in decreasing order.}
}
\description{
A polynomial of degree \eqn{n - 1},
\deqn{p(x) = {z}_{1} + {z}_{2} x + \ldots + {z}_{n} {x}^{n-1}}{p(x) = z1 + z2*x + \ldots + z[n] * x^(n-1)}
is given by its coefficient vector \code{z[1:n]}.
\code{polyroot} returns the \eqn{n-1} complex zeros of \eqn{p(x)} using the
Jenkins-Traub algorithm.
}
\value{
A complex vector of length \eqn{n-1 =} \code{length(z) - 1}.
}
\references{
Jenkins and Traub (1972).
\emph{TOMS Algorithm 419}.  Comm. ACM \bold{15}, 97-99.
}
\seealso{
\code{\link{uniroot}} for numerical root finding of arbitray functions;
\code{\link{complex}} and the \code{zero} example in the demos directory.
}
\examples{
polyroot(c(1,2,1))
round(polyroot(choose(8,0:8)), 11) # guess what!
for(n1 in 1:4) print(polyroot(1:n1), digits = 4)
}
