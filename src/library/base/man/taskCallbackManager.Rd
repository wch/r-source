\name{taskCallbackManager}
\alias{taskCallbackManager}
\title{Create an R-level task callback manager}
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
  A list containing 6 functions:
  \item{add}{register a callback with this manager, giving the
    function, an optional 5-th argument, an optional name
    by which the callback is stored in the list,
    and a \code{register} argument which controls whether
    the \code{evaluate} function is registered with the internal
    C-level dispatch mechanism if necessary.}
  \item{remove}{remove an element from the manager's collection
    of callbacks, either by name or position/index.}
  \item{evaluate}{the \sQuote{real} callback function that is registered
    with the C-level dispatch mechanism and which invokes each of the
    R-level callbacks within this manager's control.}
  \item{suspend}{a function to set the suspend state
    of the manager. If it is suspended, none of the callbacks will be
    invoked when a task is completed. One sets the state by specifying
    a logical value for the \code{status} argument.
  }
  \item{register}{a function to register the \code{evaluate}
    function with the internal C-level dispatch mechanism. This is
    done automatically by the \code{add} function, but can be called
    manually.}
  \item{callbacks}{returns the list of callbacks being maintained by this
   manager.}
}
\note{
  This is an experimental feature and the interface may be changed
  in the future.
}
\seealso{
  \code{\link{addTaskCallback}},
  \code{\link{removeTaskCallback}},
  \code{\link{getTaskCallbackNames}}\\
  \url{http://developer.r-project.org/TaskHandlers.pdf}  
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
  names(h$callback())
 
   #
  getTaskCallbackNames()
  removeTaskCallback("R-taskCallbackManager")
}
\keyword{environment}
