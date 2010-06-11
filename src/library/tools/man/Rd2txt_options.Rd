\name{Rd2txt_options}
\alias{Rd2txt_options}
\title{
Set formatting options for text help.
}
\description{
This function sets various options for displaying text help.
}
\usage{
Rd2txt_options(...)
}
\arguments{
  \item{\dots}{A list containing named options, or 
  options passed as individual named arguments.  
  See below for currently defined ones.}
}
\details{
This function persistently sets various formatting options for the \code{\link{Rd2txt}} function
which is used in displaying text format help.  Currently defined options:
\describe{
\item{width}{(default 80):  The width of the output page.}
\item{minIndent}{(default 10):  The minimum indent to use in a list.}
\item{extraIndent}{(default 4):  The extra indent to use in each level of nested lists.}
\item{sectionIndent}{(default 5):  The indent level for a section.}
\item{sectionExtra}{(default 2):  The extra indentation for each nested section level.}
\item{itemBullet}{(default \code{"* "}):  The symbol to use as a bullet in itemized lists.}
\item{enumFormat}{:  A function to format item numbers in enumerated lists.}
}
}
\value{ 
If called with no arguments, returns all option settings in a list.  Otherwise, 
it changes the named settings and 
invisibly returns their previous values.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Rd2txt}}
}
\examples{
saveOpts <- Rd2txt_options()
saveOpts
Rd2txt_options(minIndent=4)
Rd2txt_options()
Rd2txt_options(saveOpts)
Rd2txt_options()
}
\keyword{  documentation  }
