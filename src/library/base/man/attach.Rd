\name{attach}
\title{Attach Set of R Objects to Search Path}
\usage{
attach(what, pos = 2, name = deparse(substitute(what)),
       warn.conflicts = TRUE)
}
\alias{attach}
\arguments{
  \item{what}{\dQuote{database}.  This may currently be a
    \code{data.frame} or a \code{list} or a \R data file created with
    \code{\link{save}} or \code{NULL} or an environment.  See also Details.}
  \item{pos}{integer specifying position in \code{\link{search}()} where
    to attach.}
  \item{name}{alternative way to specify the database to be attached.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the database,
    unless that database contains an object \code{.conflicts.OK}.  A
    conflict is a function masking a function, or a non-function masking
    a non-function.
  }
}
\description{
  The database is attached to the \R search path.  This means that the
  database is searched by \R when evaluating a variable, so objects in
  the database can be accessed by simply giving their names.}

\details{
  When evaluating a variable or function name \R searches for
  that name in the databases listed by \code{\link{search}}.  The first
  name of the appropriate type is used.

  By attaching a data frame (or list) to the search path it is possible
  to refer to the variables in the data frame by their names alone,
  rather than as components of the data frame (e.g. in the example below,
  \code{height} rather than \code{women$height}).

  By default the database is attached in position 2 in the search path,
  immediately after the user's workspace and before all previously
  loaded packages and previously attached databases. This can be altered
  to attach later in the search path with the \code{pos} option, but you
  cannot attach at \code{pos=1}.

  The database is not actually attached.  Rather, a new environment is
  created on the search path and the elements of a list (including
  columns of a data frame) or objects in a save file or an environment
  are \emph{copied} into the new environment.  If you use
  \code{\link{<<-}} or \code{\link{assign}} to assign to an attached
  database, you only alter the attached copy, not the original object.
  (Normal assignment will place a modified version in the user's
  workspace: see the examples.)
  For this reason \code{attach} can lead to confusion.

  One useful \sQuote{trick} is to use \code{what = NULL} (or equivalently a
  length-zero list) to create a new environment on the search path into
  which objects can be assigned by \code{\link{assign}} or
  \code{\link{load}} or \code{\link{sys.source}}.

  There are hooks to attach user-defined table objects of class
  \code{"UserDefinedDatabase"}, supported by the Omegahat package
  \pkg{RObjectTables}.  See \url{http://www.omegahat.org/RObjectTables/}.
}
\value{
  The \code{\link{environment}} is returned invisibly with a
  \code{"name"} attribute.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{environment}}, \code{\link{with}}.
}
\examples{
summary(women$height)   # refers to variable 'height' in the data frame
attach(women)
summary(height)         # The same variable now available by name
height <- height*2.54   # Don't do this. It creates a new variable
                        # in the user's workspace
find("height")
summary(height)         # The new variable in the workspace
rm(height)
summary(height)         # The original variable.
height <<- height*25.4  # Change the copy in the attached environment
find("height")
summary(height)         # The changed copy
detach("women")
summary(women$height)   # unchanged

\dontrun{## create an environment on the search path and populate it
sys.source("myfuns.R", envir=attach(NULL, name="myfuns"))
}}
\keyword{data}
