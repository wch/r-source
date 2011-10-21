% A regression test example of Rd conversion
\name{testit}
\title{An Rd Regression Test}
\alias{\{}
\usage{
\\x \\y \%\{\}

foo(\var{x}, \var{y})
}
\details{
  Escaped backslash \code{\\x}.

  \code{\%\{\}}
  
  \link{a link} and one to a known destination:
  \link[stats]{weighted.mean}
  
  var in \var{text}.
  
  \code{foo(\var{x}, \var{y})}.
  \preformatted{
  Escaped backslash \\x.

  \%\{\}

  var in \var{text}.
  
  foo(\var{x}, \var{y}).
  
  From gap: pedtodot.Rd -- n=split(m,par,"\034") 
  }
  Yihui Xie's example:
  \Sexpr[results=rd,stage=render]{"\\\\describe{\\\\item{def}{ghi}}"}
}
\examples{
\\x
\%\{\}
foo(\var{x}, \var{y})
}
