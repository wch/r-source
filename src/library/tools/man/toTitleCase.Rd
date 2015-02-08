% File src/library/tools/man/toTitleCase.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
% Distributed under GPL 2 or later

\name{toTitleCase}
\alias{toTitleCase}
\title{Convert Titles to Title Case}
\description{
  Convert a character vector to title case, especially package titles.
}
\usage{
toTitleCase(text)
}
\arguments{
  \item{text}{a character vector.}
}
\details{
  This is intended for English text only.

  No definition of\sQuote{title case} is universally accepted: all agree
  that \sQuote{principal} words are capitalized and common words like
  \sQuote{for} are not.  Generally words in all capitals are left alone:
  this implementation knows about conventional mixed-case words such as
  \sQuote{LaTeX} and \sQuote{OpenBUGS} and a few technical terms which
  are not usually capitalized such as \sQuote{jar} and \sQuote{xls}.
  However, unknown technical terms (including names of libraries and
  packages) will be capitalized.
}
\value{
  A character vector of the same length as \code{text}, without names.
}
