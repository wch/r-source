% File src/library/methods/man/promptMethods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{promptMethods}
\alias{promptMethods}
\title{ Generate a Shell for Documentation of Formal Methods }
\description{
  Generates a shell of documentation for the methods of a generic
  function.
}
\usage{
promptMethods(f, filename = NULL, methods)
}
\arguments{
  \item{f}{a character string naming the generic function whose methods
    are to be documented.}
  \item{filename}{usually, a connection or a character string giving the
    name of the file to which the documentation shell should be written.
    The default corresponds to the coded topic name for these methods
    (currently, \code{f} followed by \code{"-methods.Rd"}).  Can also be
    \code{FALSE} or \code{NA} (see below).}
  \item{methods}{Optional methods list object giving the methods to be
    documented.  By default, the first methods object for this generic
    is used (for example, if the current global environment has some
    methods for \code{f}, these would be documented).

    If this argument is supplied, it is likely to be
    \code{getMethods(f, where)}, with \code{where} some package
    containing methods for \code{f}.}
}
\value{
  If \code{filename} is \code{FALSE}, the text generated;
  if \code{filename} is \code{NA}, a list-style representation of the
  documentation shell.
  Otherwise, the name of the file written to is returned invisibly.
}
\details{
  If \code{filename} is \code{FALSE}, the text created is returned,
  presumably to be inserted some other documentation file, such as the
  documentation of the generic function itself (see
  \code{\link{prompt}}).

  If \code{filename} is \code{NA}, a list-style representation of the
  documentation shell is created and returned.  Writing the shell to a
  file amounts to \code{cat(unlist(x), file = filename, sep = "\\n")},
  where \code{x} is the list-style representation.

  Otherwise, the documentation shell is written to the file specified by
  \code{filename}.
}
\seealso{
  \code{\link{prompt}} and
  \code{\link{promptClass}}
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\keyword{programming}
\keyword{methods}
