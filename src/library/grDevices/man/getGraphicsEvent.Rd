\name{getGraphicsEvent}
\alias{getGraphicsEvent}
\title{Wait for a mouse or keyboard event from a graphics window}
\description{
This function waits for input from a graphics window in the
form of a mouse or keyboard event.
}
\usage{
getGraphicsEvent(prompt = "Waiting for input", 
	         onMouseDown = NULL, onMouseMove = NULL, onMouseUp = NULL, 
	         onKeybd = NULL)
}
\arguments{
  \item{prompt}{prompt to be displayed to the user}
  \item{onMouseDown}{a function to respond to mouse clicks}
  \item{onMouseMove}{a function to respond to mouse movement}
  \item{onMouseUp}{a function to respond to mouse button releases}
  \item{onKeybd}{a function to respond to key presses}
}
\details{
This function allows user input from some graphics devices (currently
only the Windows screen display).  When called, event handlers may be 
installed to respond to events involving the mouse or keyboard.  The 
mouse event handlers should be functions with header \code{function(buttons, x, y)},
and the keyboard event handler should be a function with header
\code{function(key)} (not implemented yet).  The coordinates \code{x}
and \code{y} will be 

The event handlers are standard R functions, and will be executed in
an environment as though they had been called directly from \code{getGraphicsEvent}.

Events will be collected until one of the event handlers returns a non-\code{NULL}
value, which will be returned as the value of \code{getGraphicsEvent},
or the user interrupts the function from the console.
}
\value{
A non-\code{NULL} value returned from one of the event handlers.
}
\author{Duncan Murdoch}
\examples{
\dontrun{
    mousedown <- function(buttons, x, y) {
	cat("Buttons ", paste(buttons, collapse=" "), " at ", x, y, "\n")
	points(x, y)
	if (x > 0.85 && y > 0.85) "Done"
	else NULL
    }
    
    mousemove <- function(buttons, x, y) {
	points(x, y)
	NULL
    }
    
    plot(0:1, 0:1, type='n')
    getGraphicsEvent("Click on upper right to quit",
                     onMouseDown = mousedown,
                     onMouseMove = mousemove)
}
}
\keyword{ iplot }% at least one, from doc/KEYWORDS
