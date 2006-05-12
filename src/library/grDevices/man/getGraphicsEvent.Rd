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
installed to respond to events involving the mouse or keyboard.  

The mouse event handlers should be functions with header 
\code{function(buttons, x, y)}.  The coordinates \code{x}
and \code{y} will be passed to mouse event handlers in device independent
coordinates (i.e. the lower left corner of the window is \code{(0,0)}, 
the upper right is \code{(1,1)}).  The \code{buttons} argument 
will be a vector listing the buttons
that are pressed at the time of the event, with 0 for left, 1 for middle, and 2 
for right.

The keyboard event handler should be a function with header
\code{function(key)}.  A single element character vector will be passed
to this handler, corresponding to the key press.  Shift and other modifier
keys will have been processed, so \code{shift-a} will be passed as
\code{"A"}.  The following special keys may also be passed to the handler:
\itemize{
     \item Control keys, passed as \code{"Ctrl-A"}, etc.
     \item Navigation keys, passed as one of \code{"Left", "Up", "Right", "Down",
     "PgUp", "PgDn", "End", "Home"}
     \item Edit keys, passed as one of \code{"Ins", "Del"}
     \item Function keys, passed as one of \code{"F1", "F2", ...}
}

The event handlers are standard R functions, and will be executed in
an environment as though they had been called directly from \code{getGraphicsEvent}.

Events will be processed until
\itemize{
     \item one of the event handlers returns
a non-\code{NULL} value which will be returned as the value of
\code{getGraphicsEvent}, or 
      \item the user interrupts the function from the
console.
}
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
    
    keybd <- function(key) {
	cat("Key <", key, ">\n", sep = "")
    }
    
    plot(0:1, 0:1, type='n')
    getGraphicsEvent("Click on upper right to quit",
                     onMouseDown = mousedown,
                     onMouseMove = mousemove,
                     onKeybd = keybd)
}
}
\keyword{ iplot }% at least one, from doc/KEYWORDS
