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
  \R (or S) session.
}
\arguments{
  \item{list}{character. The names of one or more \R objects to be dumped.}

  \item{file}{either a character string naming a file or a
    connection. \code{""} indicates output to the console.}

  \item{append}{if \code{TRUE}, output will be appended to
    \code{file}; otherwise, it will overwrite the contents of
    \code{file}.}
    
  \item{control}{character vector indicating deparsing options.
    See \code{\link{.deparseOpts}} for their description.}

  \item{envir}{the environment to search for objects.}

  \item{evaluate}{logical.  Should promises be evaluated?}
}
\details{
  At present \code{source}ing may not produce an identical copy of
  \code{dump}ed objects. A warning is issued if it is likely that
  problems will arise, for example when dumping exotic objects such
  as \link{environment}s and external pointers.

  \code{dump} will also warn if fewer characters were written to a file
  than expected, which may indicate a full or corrupt file system.

  A \code{dump} file can be \code{\link{source}}d into another \R (or
  perhaps S) session, but the function \code{\link{save}} is designed to
  be used for transporting \R data, and will work with \R objects that
  \code{dump} does not handle.
  
  To produce a more readable representation of an object, use
  \code{control = NULL}.  This will skip attributes, and will make other
  simplifications that make \code{source} less likely to produce an
  identical copy.  See \code{\link{deparse}} for details.
  
  To deparse the internal function representation rather than displaying
  the saved source, use \code{control = c("keepInteger", "quoteExpressions", 
  "showAttributes", "warnIncomplete")}.  This will lose all
  formatting and comments, but may be useful in those cases where
  the saved source is no longer correct.

  Promises will normally only be encountered by users as a result of
  lazy-loading (when the default \code{evaluate = TRUE} is essential)
  and after the use of \code{\link{delay}}, when \code{evaluate = FALSE}
  might be intended.
}
\note{
  The \code{envir} argument was added at version 1.7.0, and changed the
  default search path for named objects to include the environment from
  which \code{dump} was called.

  As \code{dump} is defined in the base namespace, the \pkg{base}
  package will be searched \emph{before} the global environment unless
  \code{dump} is called from the top level or the \code{envir} argument
  is given explicitly.
  
  To avoid the risk of a source attribute out of sync with the actual
  function definition, the source attribute of a function will never 
  be dumped as an attribute.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}

\seealso{
  \code{\link{dput}}, \code{\link{dget}}, \code{\link{write}}.\cr
  \code{\link{save}} for a more reliable way to save \R objects.
}
\examples{
x <- 1; y <- 1:10
dump(ls(patt='^[xyz]'), "xyz.Rdmped")
unlink("xyz.Rdmped")
}
\keyword{file}
\keyword{connection}
