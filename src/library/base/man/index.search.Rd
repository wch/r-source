\name{index.search}
\alias{index.search}
\title{Search Indices for Help Files}
\usage{
index.search(topic, path, file="AnIndex", type = "help")
}
\arguments{
  \item{topic}{The keyword to be searched for in the indices.}
  \item{path}{The path(s) to the packages to be searched.}
  \item{file}{The index file to be searched.  Normally
    \file{"AnIndex"}.}
  \item{type}{The type of file required.}
}
\description{
  Used to search the indices for help files, possibly under aliases.
}
\details{
  For each package in \code{path}, examine the file \code{file} in
  directory \file{type}, and look up the matching file stem for topic
  \code{topic}, if any.  
}
\value{
  A character vector of matching files, as if they are in directory
  \code{type} of the corresponding package. In the special cases of
  \code{type = "html"}, \code{"R-ex"} and \code{"latex"} the file
  extensions \code{".html"}, \code{".R"} and \code{".tex"} are added.
}
\seealso{
  \code{\link{help}},
  \code{\link{example}}
}
\keyword{utilities}
