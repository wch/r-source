\name{source}
\title{Redirect Input}
\usage{
source(file, local = FALSE, echo = verbose, print.eval = echo,
       verbose = getOption("verbose"),  prompt.echo = getOption("prompt"),
       max.deparse.length = 150, chdir = FALSE)
}
\alias{source}
\arguments{
    \item{file}{character; the name of the file to read from (quoted).}
    \item{local}{if \code{local} is \code{FALSE}, the statements scanned
        are evaluated in the global environment, otherwise in the local
        calling source.}
    \item{echo}{logical; if \code{TRUE}, each expression is printed
        after parsing, before evaluation.}
    \item{print.eval}{logical; if \code{TRUE}, the result of
        \code{eval(i)} is printed for each expression \code{i}; defaults
        to \code{echo}.}
    \item{verbose}{if \code{TRUE}, more diagnostics (than just
        \code{echo = TRUE}) are printed during parsing and evaluation of
        input, including extra info for \bold{each} expression.}
    \item{prompt.echo}{character; gives the prompt to be used if
        \code{echo = TRUE}.}
    \item{max.deparse.length}{integer; is used only if \code{echo} is
        \code{TRUE} and gives the maximal length of the ``echo'' of a
        single expression.}
    \item{chdir}{logical; if \code{TRUE}, the R working directory is
        changed to the directory containing \code{file} for evaluating.}
}
\description{
    \code{source} causes \R to accept its input from the named file (the
    name must be quoted). Input is read from that file until the end of
    the file is reached.   \code{\link{parse}} is used to scan the
    expressions in, they are then evaluated sequentially in the chosen
    environment.
}
\seealso{
  \code{\link{demo}} which uses \code{source};
  \code{\link{eval}}, \code{\link{parse}} and \code{\link{scan}}; \code{\link{options}("keep.source")}.
}
\keyword{file}
\keyword{programming}
