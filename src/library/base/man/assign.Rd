\name{assign}
\alias{assign}
\alias{<-}
\alias{<<-}
\alias{->}
\alias{->>}
\title{Assign a Value to a Name}
\description{
  Assign a value to a name in an environment.
}
\usage{
assign(x, value, pos = -1, envir = pos.to.env(pos),
       inherits = FALSE, immediate = TRUE)
x <- value
x <<- value
value -> x
value ->> x
}
\arguments{
  \item{x}{a variable name (given as a quoted string).}
  \item{value}{a value to be assigned to \code{x}.}
  \item{pos}{an index into the search list which determines which
    environment the assignment is to take place in.  A character string
    may also be used.  The environment can also be specified directly
    with \code{envir}.}
  \item{envir}{the \code{\link{environment}} in which to assign.  The
    default is the environment where the call to \code{assign} takes
    place.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
  \item{immediate}{an ignored compatibility feature.}
}
\value{
  This function is invoked for its side effect, which is assigning
  \code{value} to the variable \code{x}.  If no \code{envir} is
  specified, then the assignment takes place in the currently active
  environment.

  If \code{inherits} is \code{TRUE}, enclosing environments of the supplied
  environment are searched until the variable \code{x} is encountered.
  The value is then assigned in the environment in which the variable is
  encountered.  If the symbol is not encountered then assignment takes
  place in the global environment.

  If \code{inherits} is \code{FALSE}, assignment takes place in the
  initial frame of \code{envir}.


}
\details{
  The arrow forms of assignment provide shortcut ways to carry out
  assignment.  The \code{<-} and \code{->} forms carry out assignment
  in the local environment frame, while the \code{<<-} and \code{->>}
  forms cause a search to made through the environment for an existing
  definition of the variable being assigned.  If such a variable is
  found then its value is redefined, otherwise assignment takes place
  globally.

  Note that the action of \code{<<-} and \code{->>} differs from that in
  the S language, but is useful in conjunction with the scoping rules of
  R.

  \code{assign} does not dispatch assignment methods, so it cannot be
  used to set elements of vectors, names, attributes, etc.
  }
\seealso{
  \code{\link{get}},
  \code{\link{exists}},
  \code{\link{environment}}.}
\examples{
for(i in 1:6) { #-- Create objects  'r1', 'r2', ... 'r6' --
 nam <- paste("r",i, sep=".")
 assign(nam, 1:i)
}
ls(pat="^r..$")

##-- Global assignment within a function:
myf <- function(x) {
 innerf <- function(x) assign("Global.res", x^2, env = .GlobalEnv)
 innerf(x+1)
}
myf(3)
Global.res # 16

a<-1:4
assign("a[1]",2)
a[1]==2          #FALSE
get("a[1]")==2   #TRUE

}
\keyword{data}
