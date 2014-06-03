% File src/library/base/man/dump.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{dump}
\title{Text Representations of R Objects}
\usage{
dump(list, file = "dumpdata.R", append = FALSE,
     control = "all", envir = parent.frame(), evaluate = TRUE)
}
\alias{dump}
\description{
  This function takes a vector of names of \R objects and produces
  text representations of the objects on a file or connection.
  A \code{dump} file can usually be \code{\link{source}}d into another
  \R session.
}
\arguments{
  \item{list}{character.  The names of one or more \R objects to be dumped.}

  \item{file}{either a character string naming a file or a
    \link{connection}.  \code{""} indicates output to the console.}

  \item{append}{if \code{TRUE} and \code{file} is a character string,
    output will be appended to \code{file}; otherwise, it will overwrite
    the contents of \code{file}.}

  \item{control}{character vector indicating deparsing options.
    See \code{\link{.deparseOpts}} for their description.}

  \item{envir}{the environment to search for objects.}

  \item{evaluate}{logical.  Should promises be evaluated?}
}
\details{
  If some of the objects named do not exist (in scope), they are
  omitted, with a warning.  If \code{file} is a file and no objects
  exist then no file is created.

  \code{source}ing may not produce an identical copy of
  \code{dump}ed objects.  A warning is issued if it is likely that
  problems will arise, for example when dumping exotic or complex
  objects (see the Note).

  \code{dump} will also warn if fewer characters were written to a file
  than expected, which may indicate a full or corrupt file system.

  A \code{dump} file can be \code{\link{source}}d into another \R (or
  perhaps S) session, but the function \code{\link{save}} is designed to
  be used for transporting \R data, and will work with \R objects that
  \code{dump} does not handle.  For maximal reproducibility use
  \code{control = c("all", "hexNumeric")}: for slightly less
  reproducibility an alternative is \code{control = c("all", "digits16")}.

  To produce a more readable representation of an object, use
  \code{control = NULL}.  This will skip attributes, and will make other
  simplifications that make \code{source} less likely to produce an
  identical copy.  See \code{\link{deparse}} for details.

  To deparse the internal representation of a function rather than
  displaying the saved source, use \code{control = c("keepInteger",
    "warnIncomplete", "keepNA")}.  This will lose all formatting and
  comments, but may be useful in those cases where the saved source is
  no longer correct.
  
  Promises will normally only be encountered by users as a result of
  lazy-loading (when the default \code{evaluate = TRUE} is essential)
  and after the use of \code{\link{delayedAssign}},
  when \code{evaluate = FALSE} might be intended.
}
\value{
  An invisible character vector containing the names of the objects
  which were dumped.
}
\note{
  As \code{dump} is defined in the base namespace, the \pkg{base}
  package will be searched \emph{before} the global environment unless
  \code{dump} is called from the top level prompt or the \code{envir}
  argument is given explicitly.

  To avoid the risk of a source attribute becoming out of sync with the
  actual function definition, the source attribute of a function will
  never be dumped as an attribute.

  Currently environments, external pointers, weak references and objects
  of type \code{S4} are not deparsed in a way that can be
  \code{source}d.  In addition, \link{language objects} are deparsed in a
  simple way whatever the value of \code{control}, and this includes not
  dumping their attributes (which will result in a warning).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}

\seealso{
  \code{\link{dput}}, \code{\link{dget}}, \code{\link{write}}.\cr
  \code{\link{save}} for a more reliable way to save \R objects.
}
\examples{
x <- 1; y <- 1:10
dump(ls(pattern = '^[xyz]'), "xyz.Rdmped")
print(.Last.value)
unlink("xyz.Rdmped")
}
\keyword{file}
\keyword{connection}
