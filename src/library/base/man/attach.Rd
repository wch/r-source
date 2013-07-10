% File src/library/base/man/attach.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{attach}
\alias{attach}
\alias{.conflicts.OK}

\title{Attach Set of R Objects to Search Path}
\usage{
attach(what, pos = 2L, name = deparse(substitute(what)),
       warn.conflicts = TRUE)
}
\arguments{
  \item{what}{\sQuote{database}.  This can be a
    \code{data.frame} or a \code{list} or a \R data file created with
    \code{\link{save}} or \code{NULL} or an environment.  See also
    \sQuote{Details}.}
  \item{pos}{integer specifying position in \code{\link{search}()} where
    to attach.}
  \item{name}{name to use for the attached database. Names starting with
    \code{package:} are reserved for \code{\link{library}}.}
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
  the database can be accessed by simply giving their names.
}
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
  attached packages and previously attached databases.  This can be
  altered to attach later in the search path with the \code{pos} option,
  but you cannot attach at \code{pos = 1}.

  The database is not actually attached.  Rather, a new environment is
  created on the search path and the elements of a list (including
  columns of a data frame) or objects in a save file or an environment
  are \emph{copied} into the new environment.  If you use
  \code{\link{<<-}} or \code{\link{assign}} to assign to an attached
  database, you only alter the attached copy, not the original object.
  (Normal assignment will place a modified version in the user's
  workspace: see the examples.)  For this reason \code{attach} can lead
  to confusion.

  One useful \sQuote{trick} is to use \code{what = NULL} (or equivalently a
  length-zero list) to create a new environment on the search path into
  which objects can be assigned by \code{\link{assign}} or
  \code{\link{load}} or \code{\link{sys.source}}.

  Names starting \code{"package:"} are reserved for
  \code{\link{library}} and should not be used by end users.  Attached
  files are by default given the name \code{file:\var{what}}.  The
  \code{name} argument given for the attached environment will be used
  by \code{\link{search}} and can be used as the argument to
  \code{\link{as.environment}}.

  There are hooks to attach user-defined table objects of class
  \code{"UserDefinedDatabase"}, supported by the Omegahat package
  \pkg{RObjectTables}.  See \url{http://www.omegahat.org/RObjectTables/}.
}

\value{
  The \code{\link{environment}} is returned invisibly with a
  \code{"name"} attribute.
}

\section{Good practice}{
  \code{attach} has the side effect of altering the search path and this
  can easily lead to the wrong object of a particular name being found.
  People do often forget to \code{\link{detach}} databases.

  In interactive use, \code{\link{with}} is usually preferable to the
  use of \code{attach}/\code{detach}.

  In programming, functions should not change the search path unless
  that is their purpose.  Often \code{\link{with}} can be used within a
  function. If not, good practice is to
  \itemize{
    \item Always use a distinctive \code{name} argument, and
    \item To immediately follow the \code{attach} call by an
    \code{\link{on.exit}} call to \code{detach} using the distinctive name.
  }
  This ensures that the search path is left unchanged even if the
  function is interrupted or if code after the \code{attach} call
  changes the search path.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{library}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{environment}}, \code{\link{with}}.
}
\examples{
require(utils)

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
sys.source("myfuns.R", envir = attach(NULL, name = "myfuns"))
}}
\keyword{data}
