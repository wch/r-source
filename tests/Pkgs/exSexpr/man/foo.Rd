\name{foo}
\alias{foo}
\title{Foo Title}
\description{
  Does nothing.  Here is pi:  \Sexpr{pi}.
}
\usage{
foo(x, ...)
}
\arguments{
  \item{x}{something}
  \item{\dots}{another thing}
}
\Sexpr[stage=install,results=rd]{"\\\\examples{foo()}"}
