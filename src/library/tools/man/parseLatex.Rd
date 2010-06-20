\name{parseLatex}
\alias{parseLatex}
\alias{deparseLatex}
\alias{latexToUtf8}
\title{
These experimental functions work with a subset of LaTeX code.
}
\description{
The \code{parseLatex} function parses LaTeX source, producing a structured object;
\code{deparseLatex} reverses the process.  The \code{latexToUtf8} function takes a 
LaTeX object, and processes a number of different macros to convert them into 
the corresponding UTF-8 characters.
}
\usage{
parseLatex(text, filename = deparse(substitute(text)), verbose = FALSE)
deparseLatex(x, dropBraces=FALSE)
latexToUtf8(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
A character vector containing LaTeX source code.
}
  \item{filename}{
A filename to use in syntax error messages.
}
  \item{verbose}{
If \code{TRUE}, print debug error messages.
}
  \item{x}{
A \code{"LaTeX"} object.
}
  \item{dropBraces}{
Drop unnecessary braces when displaying a \code{"LaTeX"} object.
}
}
\details{
The parser does not recognize all legal LaTeX code.  In particular, 
verbose environments will likely confuse it.  It does not associate
arguments with macros, that needs to be done after parsing, with knowledge
of the definitions of each macro.  The main
intention for this function is to process simple LaTeX code used
in bibliographic references, not fully general LaTeX documents.
}
\value{
The \code{parseLatex()} function returns a recursive object of class
\code{"LaTeX"}.  

The \code{deparseLatex()} function returns a single element character
vector, possibly containing embedded newlines.

The \code{latexToUtf8()} function returns a modified version of the
\code{"LaTeX"} object that was passed to it.
}
\author{
Duncan Murdoch
}
\examples{\donttest{
latex <- parseLatex("fa\\\\c{c}ile")
deparseLatex(latexToUtf8(latex))
}}
\keyword{ utilities }
\keyword{ documentation }