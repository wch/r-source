% File src/library/tools/man/Rd2txt_options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-2025 R Core Team
% Distributed under GPL 2 or later

\name{Rd2txt_options}
\alias{Rd2txt_options}
\title{Set Formatting Options for Text Help}
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
  help.  Currently defined options (defaults) are:
  \describe{
    \item{width (80)}{The width of the output page.}
    \item{minIndent (10)}{The minimum indent to use in a list.}
    \item{extraIndent (4)}{The extra indent to use in each
      level of nested lists.}
    \item{sectionIndent (5)}{The indent level for a section.}
    \item{sectionExtra (2)}{The extra indentation for each
      nested section level.}
    \item{itemBullet (\code{"* "}, but \code{"\\u2022 "} in UTF-8 locales)}{
      The symbol to use as a bullet in \verb{\itemize} lists.}
    \item{enumFormat (\code{function(n) sprintf("\%d. ", n)})}{
      A function to format item numbers in \verb{\enumerate} lists.}
    \item{descStyle (\code{"linebreak"})}{
      How to separate the item label from the body in \verb{\describe}
      lists.  The alternative \code{"colon"} inserts a colon if the label
      does not already end with one.  Use \code{NULL} for a simple space
      (as in \R < 4.6.0).}
    \item{showURLs (\code{FALSE})}{Whether to show URLs when
      expanding \verb{\href} tags.}
    \item{code_quote (\code{TRUE})}{Whether to render
      \verb{\code} and similar with single quotes.}
    \item{underline_titles (\code{TRUE})}{Whether to render
      section titles with underlines (via backspacing).}
  }
}
\value{
  If called with no arguments, returns all option settings in a list.
  Otherwise, it changes the named settings and invisibly returns their
  previous values.
}
\author{
Duncan Murdoch and the R Core Team
}
\seealso{
\code{\link{Rd2txt}}
}
\examples{
saveOpts <- Rd2txt_options()
\donttest{saveOpts}% itemBullet is locale-specific
Rd2txt_options(minIndent = 4)
Rd2txt_options()[["minIndent"]]
Rd2txt_options(saveOpts)
stopifnot(identical(Rd2txt_options(), saveOpts))
}
\keyword{  documentation  }
