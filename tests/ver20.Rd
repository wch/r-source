% Test file for Rd version 1.1
\name{ver20}
\Rdversion{1.1}

\title{Johnson & Johnson,  $ _  ### ^ ~}

\arguments{
    \item{foo}{item 1}
    \item{bar}{space, the item 2}
    \item{bah}{newline, then item 3}
}

\description{
  This is the description
}

\details{
  \R \R{} \\R \eqn{\alpha\beta\Gamma}
  
  In code, all that needs to be escaped is percent, backslash and unpaired
  braces: \code{\% \\ \{}. Unescaped: \code{$ # _ ^ ~ < > |}, and as a
  pair, \code{{...}}.  If we escape: \code{ \_ \# \$ \& \{ \} \\ }
  And some backslashes: \code{\ \\ \\\ \\\\} should
  give one, one, two, two backslashes.

  How about \ (that is backslash space) in text?
  
  Elsewhere, \{ \} \% \\ need to be escaped, but not $ # _ ^ ~ < > | .
  Let's try that again in \code{\\samp}: \samp{ \{ \} \% \\ $ # _ ^ ~ < > |}.
  And if we do escape: \$ \# \_ , and with three backslashes, \\\#.

  \deqn{a < b > c | d, x^y}
  \deqn{\$k > \#k, x_y, x\_y}

  % From BaM/man/hanjack.Rd
  group-weighting from $[N_iP_i(1-P_i)]$
  
  \preformatted{
    \% a
    \\% b
    \\\% c
    \{
    \\\{
  }
}

\section{URLs and emails}{
  URLs need to be verbatim:  \url{testit ~^_$#<>|}.

  Markus Kuhn, \dQuote{ASCII and Unicode quotation marks}.
  \url{http://www.cl.cam.ac.uk/~mgk25/ucs/quotes.html}
  
  No escape is needed in \email{Micah_Altman@harvard.edu}
}

\examples{
"\\\\r\\\\n"
}
