\name{nestedDefinesOK}
\alias{nestedDefinesOK}
\title{Nested \verb{#ifdef} inside Post-Build \verb{\Sexpr}}
\description{
  In R < 4.4.0, the dynamic flag (and the options) got lost during
  \verb{#ifdef} processing, so the \verb{\Sexpr} was not processed
  afterwards.
}
\section{OS type}{\Sexpr[stage=install, results=verbatim]{
% using verbatim output so we notice when both are evaluated
#ifdef unix
% a multi-expression block
os <- "unix"
# a comment
cat(os)
#endif
#ifdef windows
cat("windows")
#endif
}}
