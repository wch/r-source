% A regression test example of Rd conversion
\name{testit}
\title{An Rd Regression Test}
\alias{\{}
\description{
  \tabular{rl}{
    \href{https://example.org/a&b#c}{linked text in \verb{\tabular}}
    \tab \url{https://example.org/a&b#c}
  }
}
\usage{
\\x \\y \%\{\}

foo(\var{x}, \var{y})
}
\details{
  Escaped backslash \code{\\x}.

  \code{\%\{\}}
  
  \link{a link} and some to known destinations:
  \link[stats]{weighted.mean}, \code{\link[=Paren]{\{}}
  
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
