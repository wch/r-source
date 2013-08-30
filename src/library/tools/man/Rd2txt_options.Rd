% File src/library/tools/man/Rd2txt_options.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010 R Core Team
% Distributed under GPL 2 or later

\name{Rd2txt_options}
\alias{Rd2txt_options}
\title{
Set formatting options for text help
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
  This function persistently sets various formatting options for the
  \code{\link{Rd2txt}} function which is used in displaying text format
  help.  Currently defined options are:
  \describe{
    \item{width}{(default 80):  The width of the output page.}
    \item{minIndent}{(default 10):  The minimum indent to use in a list.}
    \item{extraIndent}{(default 4):  The extra indent to use in each
      level of nested lists.}
    \item{sectionIndent}{(default 5):  The indent level for a section.}
    \item{sectionExtra}{(default 2):  The extra indentation for each
      nested section level.}
    \item{itemBullet}{(default \code{"* "}, with the asterisk replaced by a
      Unicode bullet in UTF-8 and most Windows locales):
      The symbol to use as a bullet in itemized lists.}
    \item{enumFormat}{:  A function to format item numbers in enumerated lists.}
    \item{showURLs}{(default \code{FALSE}):  Whether to show URLs when
      expanding \verb{\href} tags.}
    \item{code_quote}{(default \code{TRUE}):  Whether to render
      \verb{\code} and similar with single quotes.}
    \item{underline_titles}{default \code{TRUE}):  Whether to render
      section titles with underlines (via backspacing).}
  }
}
\value{
  If called with no arguments, returns all option settings in a list.
  Otherwise, it changes the named settings and invisibly returns their
  previous values.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{Rd2txt}}
}
\examples{
\donttest{
# The itemBullet is locale-specific
saveOpts <- Rd2txt_options()
saveOpts
Rd2txt_options(minIndent = 4)
Rd2txt_options()
Rd2txt_options(saveOpts)
Rd2txt_options()
}
}
\keyword{  documentation  }
