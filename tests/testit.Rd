% A regression test example of Rd conversion
\name{testit}
\alias{{}
\usage{
\\x \y \%\{\}

foo(\var{x}, \var{y})
\link{a link}
}
\details{
  Escaped backslash \code{\\x}.
  Unescaped backslash \code{\y}.

  \code{\%\{\}}

  var in \var{text}.
  
  \code{foo(\var{x}, \var{y})}.
  \preformatted{
  Escaped backslash \\x.
  Unescaped backslash \y.

  \%\{\}

  var in \var{text}.
  
  foo(\var{x}, \var{y}).
  }
}
\examples{
\\x
\y
\%\{\}
\link{a link}
foo(\var{x}, \var{y})
}