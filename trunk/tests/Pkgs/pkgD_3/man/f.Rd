\name{f}
\alias{f}
\title{Function f -- a Test}
\description{ An Rd test only. }
\usage{
f(a)
}
\arguments{
  \item{a}{a number.}
}
\value{a number.}

\examples{
PKG <- "stats4"
pd <- packageDescription(PKG)
cat(sprintf("\%s -- built: \%s\n\%*s -- file: \%s\n",
            PKG, pd$Built, nchar(PKG), "", attr(pd, "file")))
## Now, with a typo - forgetting to escape some of the percent signs :
##  -- 2012-03-12 -- error message is +- ok now:
##> 2013-02-x : the new parse_Rd() now chokes on this (with a "good"
##> -------     error message)  already during package *build* --> cannot keep it here!
##> cat(sprintf("\%s -- built: %s\n%*s -- file: %s\n",
##>             PKG, pd$Built, nchar(PKG), "", attr(pd, "file")))
}

