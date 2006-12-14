\name{ps.options}
\alias{ps.options}
\title{Auxiliary Function to Set/View Argument of postscript}
\description{
  The auxiliary function \code{ps.options} can be used to set and view
  (if called without arguments) default values for the arguments to
  \code{\link{postscript}}.
  
  \code{ps.options} needs to be called before calling \code{postscript},
  and the default values it sets can be overridden by supplying
  arguments to \code{postscript}.
  }
\usage{
ps.options(\dots, reset = FALSE, override.check = FALSE)
}
\arguments{
  \item{\dots}{arguments \code{paper}, \code{horizontal}, \code{width},
    \code{height}, \code{family}, \code{encoding}, \code{pointsize},
    \code{bg}, \code{fg}, \code{onefile}, \code{print.it},
    \code{colormodel} and \code{append} (ignored) can be supplied.}
  \item{reset, override.check}{logical arguments passed to
    \code{\link{check.options}}.  See the Examples.}
}
\value{
  A named list of arguments.
}
\seealso{
  \code{\link{postscript}}
}
\examples{
ps.options(bg = "pink")
utils::str(ps.options(reset = TRUE))

### ---- error checking of arguments: ----
ps.options(width=0:12, onefile=0, bg=pi)
# override the check for 'onefile', but not the others:
utils::str(ps.options(width=0:12, onefile=1, bg=pi,
                      override.check = c(FALSE,TRUE,FALSE)))
}
\keyword{device}
