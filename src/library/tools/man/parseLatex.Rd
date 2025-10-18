% File src/library/tools/man/parseLatex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{parseLatex}
\alias{parseLatex}
\alias{deparseLatex}
\alias{latexToUtf8}
\title{Experimental Functions to Work with LaTeX Code}
\description{
  The \code{parseLatex} function parses \LaTeX source, producing a
  structured object; \code{deparseLatex} reverses the process.  The
  \code{latexToUtf8} function takes a \LaTeX object, and processes a number
  of different macros to convert them into the corresponding UTF-8
  characters.
}
\usage{
parseLatex(text, filename = "text",
           verbose = FALSE,
           verbatim = c("verbatim", "verbatim*",
                        "Sinput", "Soutput"),
           verb = "\\\\Sexpr",
           defcmd = c("\\\\newcommand", "\\\\renewcommand",
                      "\\\\providecommand", "\\\\def", "\\\\let"),
           defenv = c("\\\\newenvironment",
                      "\\\\renewenvironment"))
deparseLatex(x, dropBraces = FALSE)
latexToUtf8(x)
}
\arguments{
\item{text}{
A character vector containing \LaTeX source code.
}
\item{filename}{
No longer used.
}
\item{verbose}{
If \code{TRUE}, print debug error messages.
}
\item{verbatim}{
A character vector containing the names of \LaTeX environments holding
verbatim text.
}
\item{verb}{
A character vector containing \LaTeX macros that should be 
assumed to hold verbatim text.
}
\item{defcmd, defenv}{
Character vectors containing \LaTeX macros that should be assumed
to define macros or environments respectively.
}
\item{x}{
A \code{"LaTeX"} object.
}
\item{dropBraces}{
Drop unnecessary braces when displaying a \code{"LaTeX"} object.
}
}
\details{
The parser does not recognize all legal \LaTeX code, only relatively
simple examples.  It does not associate arguments with macros, that
needs to be done after parsing, with knowledge of the definitions of
each macro.  The main intention for this function is to process simple
\LaTeX code used in bibliographic references, not fully general \LaTeX
documents.

Verbatim text is allowed in three forms:  the \verb{\\verb} macro (with
single character delimiters), environments whose names are listed
in the \code{verbatim} argument, and other macros (with 
brace delimiters) whose names are listed in the \code{verb}
argument.
}
\value{
The \code{parseLatex()} function returns a recursive object of class
\code{"LaTeX"}.  Each of the entries in this object will have a
\code{"latex_tag"} attribute identifying its syntactic role.

The \code{deparseLatex()} function returns a single element character
vector, possibly containing embedded newlines.

The \code{latexToUtf8()} function returns a modified version of the
\code{"LaTeX"} object that was passed to it.
}
\author{
Duncan Murdoch
}
\examples{\donttest{
latex <- parseLatex("fran\\\\c{c}ais")
deparseLatex(latexToUtf8(latex))
}}
\keyword{ utilities }
\keyword{ documentation }
