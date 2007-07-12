\name{recover}
\alias{recover}
\alias{limitedLabels}
\title{Browsing after an Error}
\description{
  This function allows the user to browse directly on any of the
  currently active function calls, and is suitable as an error option.
  The expression \code{options(error=recover)} will make this
  the error option.
}
\usage{
recover()
}
\details{
  When called, \code{recover} prints the list of current calls, and
  prompts the user to select one of them.  The standard \R
  \code{\link{browser}} is then invoked from the corresponding
  environment; the user can type ordinary S language expressions to be
  evaluated in that environment.

  When finished browsing in this call, type \code{c} to return to
  \code{recover} from the browser.  Type another frame number to browse
  some more, or type \code{0} to exit \code{recover}.

  The use of \code{recover} largely supersedes \code{\link{dump.frames}}
  as an error option, unless you really want to wait to look at the
  error.  If \code{recover} is called in non-interactive mode, it
  behaves like \code{dump.frames}.  For computations involving large
  amounts of data, \code{recover} has the advantage that it does not
  need to copy out all the environments in order to browse in them.  If
  you do decide to quit interactive debugging, call
  \code{\link{dump.frames}} directly while browsing in any frame (see
  the examples).
%
% seems no longer needed (tested 2.4.1)
%   \emph{WARNING}: The special \code{Q} command to go directly from the
%   browser to the prompt level of the evaluator currently interacts with
%   \code{recover} to effectively turn off the error option for the next
%   error (on subsequent errors, \code{recover} will be called normally).
}
\value{
  Nothing useful is returned.  However, you \emph{can} invoke
  \code{recover} directly from a function, rather than through the error
  option shown in the examples.  In this case, execution continues
  after you type \code{0} to exit \code{recover}.
}

\section{Compatibility Note}{
  The \R \code{recover} function can be used in the same way as the
  S-Plus function of the same name; therefore, the error option shown is
  a compatible way to specify the error action.  However, the actual
  functions are essentially unrelated and interact quite differently
  with the user.  The navigating commands \code{up} and \code{down} do
  not exist in the \R version; instead, exit the browser and select
  another frame.
}
\references{
  John M. Chambers (1998).
  \emph{Programming with Data}; Springer. \cr
  See the compatibility note above, however.
}
\seealso{
  \code{\link{browser}} for details about the interactive computations;
  \code{\link{options}} for setting the error option;
  \code{\link{dump.frames}} to save the current environments for later
  debugging.
}
\examples{
\dontrun{

options(error = recover) # setting the error option

### Example of interaction

> myFit <- lm(y ~ x, data = xy, weights = w)
Error in lm.wfit(x, y, w, offset = offset, ...) :
        missing or negative weights not allowed

Enter a frame number, or 0 to exit
1:lm(y ~ x, data = xy, weights = w)
2:lm.wfit(x, y, w, offset = offset, ...)
Selection: 2
Called from: eval(expr, envir, enclos)
Browse[1]> objects() # all the objects in this frame
[1] "method" "n"      "ny"     "offset" "tol"    "w"
[7] "x"      "y"
Browse[1]> w
[1] -0.5013844  1.3112515  0.2939348 -0.8983705 -0.1538642
[6] -0.9772989  0.7888790 -0.1919154 -0.3026882
Browse[1]> dump.frames() # save for offline debugging
Browse[1]> c # exit the browser

Enter a frame number, or 0 to exit
1:lm(y ~ x, data = xy, weights = w)
2:lm.wfit(x, y, w, offset = offset, ...)
Selection: 0 # exit recover
>

}%dontrun
}
\keyword{programming}
\keyword{debugging}
