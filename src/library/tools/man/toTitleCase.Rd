% File src/library/tools/man/toTitleCase.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2015--2024 R Core Team
% Distributed under GPL 2 or later

\name{toTitleCase}
\alias{toTitleCase}
\title{Convert Titles to Title Case}
\description{
  Convert a character vector to title case for English, especially package titles.
}
\usage{
toTitleCase(text)
}
\arguments{
  \item{text}{a character vector.}
}
\details{
  This is intended for English text only.

  No definition of \sQuote{title case} is universally accepted: all agree
  that \sQuote{principal} words are capitalized and common words like
  \sQuote{for} are not, but not which words fall into each category.

  Generally words in all capitals are left alone: this implementation
  knows about conventional mixed-case words such as \sQuote{\I{LaTeX}} and
  \sQuote{\I{OpenBUGS}} and a few technical terms which are not usually
  capitalized such as \sQuote{\I{jar}} and \sQuote{\I{xls}}.  However, unknown
  technical terms will be capitalized unless they are single words
  enclosed in single quotes: names of packages and libraries should be
  quoted in titles.
}
\value{
  A character vector of the same length as \code{text}, without names.
}
\examples{
toTitleCase("bayesian network modeling and analysis")
toTitleCase("ensemble tool for predictions from species distribution models")
## Treatment after "-":
toTitleCase("small- and large-scale analysis") # lowercase "and"

toTitleCase("a small fox is jumping")    # "a Small Fox is .."     (the 'a' may change)% i.e. BUG
toTitleCase("is a small fox jumping?")   # "Is a Small Fox .."     (fine)
## After ":", start a new sentence
toTitleCase("a pangram: the quick brown fox jumps over the lazy dog")
toTitleCase("asking -- 'is a small fox jumping?'") # ".. -- Is a Small ..."     (fine)
}
