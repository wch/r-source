\name{reshapeLong-deprecated}
\alias{reshapeLong-deprecated}
\title{Reshape data frame to long format}
\description{
  (EXPERIMENTAL). Convert data frame with repeated measurements in
  wide format with repeated
  observations in multiple variables across rows to long
  format, 1 row per observation.
}
\usage{
reshapeLong(x, jvars, ilev=row.names(x), jlev=names(x)[jvars],
            iname="reshape.i", jname="reshape.j", vname="reshape.v")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{The data frame to convert}
  \item{jvars}{The variables to reshape by}
  \item{ilev}{Levels of 1st indexing factor}
  \item{jlev}{Levels of 2nd indexing factor}
  \item{iname}{Name of 1st indexing factor}
  \item{jname}{Name of 2nd indexing factor }
  \item{vname}{Name of variable holding the combined values of the
    "across" variables}
}
\details{
  This causes the values in \code{jvars} to be combined into a single
  variable, all other variables being replicated the relevant number of
  times. Two factors are added to the data frame indicating rows and
  columns of the original data format.
}
\value{
   The reshaped data frame.
}
\note{The same substitution tricks are used for \code{jvars} as for the
  \code{select} argument to \code{subset}. I.e.\ the argument is treated
  as an expression and variables are replaced with their number in the
  data frame, allowing ranges of variables to be specified.
}
\section{Bugs}{There ought to be a way to have multiple sets of
  \code{jvars} variables.} 

\seealso{\code{\link{reshapeWide}}, \code{\link{stack}} }

\examples{
(dd<-as.data.frame(matrix(1:24,4)))
reshapeLong(dd,V3:V5)
}
\keyword{ manip }%-- one or more ...
