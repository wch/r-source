\name{Rd-Sexpr-warning}
\title{Trigger a \code{checkRd} Warning for \verb{\\Sexpr} Output}
\description{Regression test for c75410}
\section{Rd issue}{ % in line 5 (strong is invalid in code block)
  \Sexpr[results=rd,stage=build]{"\\\\code{\\\\strong{x}}"}
}
