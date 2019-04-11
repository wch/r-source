% File src/library/base/man/taskCallbackManager.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{taskCallbackManager}
\alias{taskCallbackManager}
\title{Create an R-level Task Callback Manager}
\description{
  This provides an entirely S-language mechanism
  for managing callbacks or actions  that are invoked at
  the conclusion of each top-level task.  Essentially,
  we register a single R function from this manager
  with the underlying, native
  task-callback mechanism and this function handles invoking the other
  R callbacks under the control of the manager.
  The manager consists of a collection of functions that access shared
  variables to manage the list of user-level callbacks.
}
\usage{
taskCallbackManager(handlers = list(), registered = FALSE,
                    verbose = FALSE)
}
\arguments{
  \item{handlers}{this can be a list of callbacks in which each element
    is a list with  an element  named \code{"f"}
    which is a callback function, and an optional
    element named \code{"data"} which is the 5-th argument to be
    supplied  to the callback when it is invoked.
    Typically this argument is not specified, and one uses \code{add} to
    register callbacks after the manager is created.}
  \item{registered}{a logical value indicating whether
    the \code{evaluate} function has already been registered
    with the internal task callback mechanism.
    This is usually \code{FALSE} and
    the first time a callback is added
    via the \code{add} function, the
    \code{evaluate} function is automatically registered.
    One can control when the function is registered
    by specifying \code{TRUE} for this argument
    and calling \code{\link{addTaskCallback}} manually.
  }
  \item{verbose}{a logical value, which if \code{TRUE},
    causes information to be printed to the console
    about certain activities this dispatch manager performs.
    This is useful for debugging callbacks and the handler
    itself.
  }
}
\value{
  A \code{\link{list}} containing 6 functions:
  \item{\code{add()}}{register a callback with this manager, giving the
    function, an optional 5-th argument, an optional name
    by which the callback is stored in the list,
    and a \code{register} argument which controls whether
    the \code{evaluate} function is registered with the internal
    C-level dispatch mechanism if necessary.}
  \item{\code{remove()}}{remove an element from the manager's collection
    of callbacks, either by name or position/index.}
  \item{\code{evaluate()}}{the \sQuote{real} callback function that is registered
    with the C-level dispatch mechanism and which invokes each of the
    R-level callbacks within this manager's control.}
  \item{\code{suspend()}}{a function to set the suspend state
    of the manager.  If it is suspended, none of the callbacks will be
    invoked when a task is completed.  One sets the state by specifying
    a logical value for the \code{status} argument.
  }
  \item{\code{register()}}{a function to register the \code{evaluate}
    function with the internal C-level dispatch mechanism.  This is
    done automatically by the \code{add} function, but can be called
    manually.}
  \item{\code{callbacks()}}{returns the list of callbacks being maintained
    by this manager.}
}
\seealso{
  \code{\link{addTaskCallback}},
  \code{\link{removeTaskCallback}},
  \code{\link{getTaskCallbackNames}} and the reference.
}
\references{
  Duncan Temple Lang (2001)
  \emph{Top-level Task Callbacks in R}, % TaskHandlers.tex is in R-dev-web svn
  \url{https://developer.r-project.org/TaskHandlers.pdf}
}
\examples{
# create the manager
h <- taskCallbackManager()

# add a callback
h$add(function(expr, value, ok, visible) {
                       cat("In handler\n")
                       return(TRUE)
                     }, name = "simpleHandler")

# look at the internal callbacks.
getTaskCallbackNames()

# look at the R-level callbacks
names(h$callbacks())

removeTaskCallback("R-taskCallbackManager")
}
\keyword{environment}
