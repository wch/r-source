\name{taskCallbackNames}
\alias{getTaskCallbackNames}
\title{Query the names of the current internal top-level task callbacks}
\usage{
getTaskCallbackNames()
}
\arguments{
}
\value{
  A character vector giving the name for each of the
  registered callbacks which are invoked when
  a top-level task is completed successfully.
  Each name is the one used when registering
  the callbacks and returned as the in the
  call to \code{\link{addTaskCallback}}. 
}
\note{
  This is an experimental feature and the interface may be changed
  in the future.
}
\seealso{
  \code{\link{addTaskCallback}}
  \code{\link{removeTaskCallback}}
  \code{\link{taskCallbackManager}}
  \url{http://developer.r-project.org/TaskHandlers.pdf}  
}
\examples{
  n <- addTaskCallback(function(expr, value, ok, visible) {
                         cat("In handler\n")
                         return(TRUE)
                       }, name = "simpleHandler")

  getTaskCallbackNames()

    # now remove it by name
  removeTaskCallback("simpleHandler")


  h <- taskCallbackManager()
  h$add(function(expr, value, ok, visible) {
                         cat("In handler\n")
                         return(TRUE)
                       }, name = "simpleHandler")
  getTaskCallbackNames()
  removeTaskCallback("R-taskCallbackManager")
}
\keyword{environment}
