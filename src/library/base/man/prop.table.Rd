\name{prop.table}
\title{Express table entries as fraction of marginal table}
\usage{
prop.table(x, margin)
}
\alias{prop.table}
 \item{x}{ table }
 \item{margin}{ index, or vector of indices to sum over for margin }
}

\description{ This is really \code{sweep(x, margin, margin.table(x,
margin), "/")} for newbies.  }

\value{
  Table like \code{x} expressed relative to \code{margin} 
}
\author{ Peter Dalgaard }

\seealso{\code{\link{margin.table}}}

\examples{
m<-matrix(1:4,2)
m
prop.table(m,1)
}
\keyword{ array }%-- one or more ...
