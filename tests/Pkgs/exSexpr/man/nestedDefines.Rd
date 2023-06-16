\name{nestedDefines}
\alias{nestedDefines}
\title{Nested \verb{#ifdef} Macros}
\description{
  \verb{#ifdef} is invalid inside R-like arguments of build-stage
  \verb{\Sexpr} macros.
}
\section{Bad}{
\Sexpr[stage=build]{
#ifdef unix
"Unix"
#endif
#ifdef windows
"Windows"
#endif
}}
