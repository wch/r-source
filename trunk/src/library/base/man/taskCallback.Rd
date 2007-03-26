\name{taskCallback}
\alias{addTaskCallback}
\alias{removeTaskCallback}
\title{Add or remove a top-level task callback}
\description{
  \code{addTaskCallback} registers an R function
  that is to be called each time a top-level task
  is completed.

  \code{removeTaskCallback} un-registers a function
  that was registered earlier via \code{addTaskCallback}.

  These provide low-level access to the internal/native
  mechanism for managing task-completion actions.
  One can use \code{\link{taskCallbackManager}}
  at the S-language level to manage S functions
  that are called at the completion of each task.
  This is easier and more direct.
}
\usage{
addTaskCallback(f, data = NULL, name = character(0))
removeTaskCallback(id)
}
\arguments{
  \item{f}{the function that is to be invoked each time a top-level task
    is successfully completed. This is called with 5 or 4 arguments
    depending on whether \code{data} is specified or not, respectively.
    The return value should be a logical value indicating whether to
    keep the callback in the list of active callbacks or discard it.}
  \item{data}{if specified, this is the 5-th argument in the call to the
    callback function \code{f}.}
  \item{id}{a string or an integer identifying the element in the
    internal callback list to be removed.
    Integer indices are 1-based, i.e the first element is 1.
    The names of currently registered handlers is available
    using \code{\link{getTaskCallbackNames}}
    and is also returned in a call to \code{\link{addTaskCallback}}.
  }
  \item{name}{character: names to be used.}
}
\value{
  \code{addTaskCallback} returns
  an integer value giving  the position in the list
  of task callbacks that this new callback occupies.
  This is only the current position of the callback.
  It can be used to remove the entry as long as
  no other values are removed from earlier positions
  in the list first.

  \code{removeTaskCallback} returns a logical value
  indicating whether the specified element was removed.
  This can fail (i.e., return \code{FALSE})
  if an incorrect name or index is given that does not
  correspond to the name or position of an element in the list.
}
\details{
Top-level tasks are individual expressions
rather than entire lines of input. Thus an input
line of the form \code{expression1 ; expression2}
will give rise to 2 top-level tasks.
  
A top-level task callback is called with the expression for the
top-level task, the result of the top-level task, a logical value
indicating whether it was successfully completed or not (always TRUE
at present), and a logical value indicating whether the result was
printed or not. If the \code{data} argument was specified in the call
to \code{addTaskCallback}, that value is given as the fifth argument.

The callback function should return a logical value.
If the value is FALSE, the callback is removed from the task
list and will not be called again by this mechanism.
If the function returns TRUE, it is kept in the list and
will be called on the completion of the next top-level task.
}
\note{
  This is an experimental feature and the interface may be changed
  in the future.

  There is also C-level access to top-level task callbacks
  to allow C routines rather than R functions be used.
}
\seealso{
  \code{\link{getTaskCallbackNames}}
  \code{\link{taskCallbackManager}}
  \url{http://developer.r-project.org/TaskHandlers.pdf}
}
\examples{
 times <- function(total = 3, str="Task a") {
   ctr <- 0

   function(expr, value, ok, visible) {
    ctr <<- ctr + 1
    cat(str, ctr, "\n")
    if(ctr == total) {
      cat("handler removing itself\n")
    }
    return(ctr < total)
   }
 }

 # add the callback that will work for
 # 4 top-level tasks and then remove itself.
 n <- addTaskCallback(times(4))

 # now remove it, assuming it is still first in the list.
 removeTaskCallback(n)

\dontrun{
# There is no point in running this
# as 
 addTaskCallback(times(4))

 sum(1:10)
 sum(1:10)
 sum(1:10)
 sum(1:10)
 sum(1:10)
}
}
\keyword{environment}
