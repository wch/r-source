\name{method.skeleton}
\alias{method.skeleton}
\title{Create a Skeleton File for a New Method}
\description{
  This function writes a source file containing a call to \code{\link{setMethod}} to define a method
  for the generic function and signature supplied.  By default the
  method definition is in line in the call, but can be made an
  external function assigned in the same source file.
}
\usage{
method.skeleton(generic, signature, file, external = FALSE)
}
\arguments{
  \item{generic}{the character string name of the generic function, or
      the generic function itself.  In the first case, the function
      need not currently be a generic, as it would not for the
      resulting call to \code{\link{setMethod}}.}
  \item{signature}{the method signature, as it would be given to \code{\link{setMethod}}}
  \item{file}{a character string name for the output file, or a
      writable connection.  By default the generic function name and
      the classes in the signature are concatenated, with separating
      underscore characters.  The file name should normally end in \code{".R"}. }

    To write multiple method skeletons to one file, open the file
    connection first and then pass it to \code{method.skeleton()} in
    multiple calls.
  \item{external}{flag to control whether the function definition for
      the method should be a separate external object assigned in the
      source file, or included in line in the call to
      \code{\link{setMethod}}.
    If supplied as a character string, this will be used as the name
    for the external function; by default the name concatenates the
    generic and signature names, as in the example below.}
}
\value{
  The \code{file} argument, invisibly, but the function is used for its side effect.
}
}
\seealso{ \code{\link{setMethod}}, \code{\link{package.skeleton}}}
\examples{
\dontrun{
setClass("track", representation(x ="numeric", y="numeric"))
method.skeleton("show", "track")  ## writes show_track.R
method.skeleton("Ops", c("track", "track")) ##writes "Ops_track_track.R"
## write multiple method skeletons to one file
con = file("./Math_track.R", "w")
method.skeleton("Math", "track", con)
method.skeleton("exp", "track", con)
method.skeleton("log", "track", con)
close(con)
}
}
\keyword{programming}
\keyword{methods}
