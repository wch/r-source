% A regression test example of Rd conversion
\name{testit}
\title{An Rd Regression Test}
\alias{\{}
\usage{
\\x \\y \%\{\}

foo(\var{x}, \var{y}, ...)
}
\arguments{
  \item{
    x,
    y
  }{
    combined arguments, in multiple Rd lines

    paragraph
  }
  \item{...}{description of \dots: \ldots}
}
\details{
#ifndef nonexistent
  Included
% neither this comment line
  % nor this one should render a new paragraph
  conditional content.
#endif
#ifdef nonexistent
  This is not included.
#endif

  Escaped backslash \code{\\x}.

  \code{\%\{\}}
  
  \link{a link} and some to known destinations:
  \code{\link[stats]{weighted.mean}}, \link[stats:weighted.mean]{dito},
  \code{\link[=Paren]{\{}}, \link[=R_HOME]{\env{R_HOME}}.
  
  var in \var{text}.
  
  \code{foo(\var{x}, \var{y})}.
  
  Preformatted:\preformatted{
  Escaped backslash \\x.

  \%\{\}

  var in \var{text}.
  
  foo(\var{x}, \var{y}).
  
  From gap: pedtodot.Rd -- n=split(m,par,"\034") 
  }
}
\RdOpts{stage=render}
\section{\verb{\Sexpr}}{
  Yihui Xie's example:
  \Sexpr[results=rd]{"\\\\describe{\\\\item{def}{ghi}}"}

  --
  \Sexpr[results=verbatim,echo=TRUE,strip.white=TRUE]{cat("\noutput\n")}
  --

  \Sexpr[echo=TRUE,results=verbatim,keep.source=TRUE]{x<-007}
}
\value{
  [NULL]\cr\cr\dots
}
\section{\verb{\tabular}}{
  \tabular{crl}{
    1 \tab \href{https://example.org/a&b#c}{linked text in \verb{\tabular}}
    \tab \url{https://example.org/a&b#c}\cr
    2 \tab 2nd-column entry with \emph{line break}
    in Rd source\tab third
    column\cr
    --3-- \tab third
    row\tab % nothing here
  }
}
\section{\verb{\eqn}}{
  This should not start a new paragraph:
  \eqn{\frac{1}{n-1}}{
             1/(n-1)}.
}
\section{\verb{\deqn}}{
  1-arg form, centered also in text conversion:
  \deqn{
    \theta = (\mu^*, \sigma^2, \rho)
  }
  2-arg form, where the text version is collapsed before centering
  (for the sake of existing Rd files):
  \deqn{E = mc^2}{    E =
    m c^2}

  With AMS extension (R \eqn{\ge} 4.2.2),
  where the text version spans more than 3 source lines (5 in fact)
  and is thus output as-is in R \eqn{\ge} 4.4.0:
  \deqn{
    f(x) = \begin{cases}
      0 & \text{if } x < 0, \\
      1 & \text{if } x \ge 0.
    \end{cases}
  }{
             0  if x < 0,
    f(x) =
             1  if x >= 0.
  }
}
\examples{
\\x
\%\{\}

\dontrun{stop("doomed to fail")}

foo(\var{x},
% pure comment lines should be dropped
    \var{y})
}
