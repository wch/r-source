%From: https://raw.githubusercontent.com/GeoBosh/reprexes/master/disappearingParagraphs/a.Rd
\name{disappearingPar}
\alias{disappearingPar}
\title{Disappearing Paragraphs When Rendering Sexpr from '*.Rd'}
\description{An example of \dQuote{disappearing paragraphs} when a
  \verb{\Sexpr} in \file{*.Rd} is rendered to HTML, via
  \code{\pkg{tools}::\link{Rd2HTML}("...../a0.Rd", stages = "build")}.
}
\details{
  The following result:

\Sexpr[stage=build,results=rd]{paste0("a", 1:3, collapse = "\n\n")}

  is from a \verb{\Sexpr} starting at the very beginning of the line and
  fine: in the HTML, each letter above is inside a \verb{<p>} tag (also
  in R-devel).

  The same, but not at the beginning of the line, fails in R-devel (2018-10-31):

  \Sexpr[stage=build,results=rd]{paste0("a", 1:3, collapse = "\n\n")}

  In html, the empty lines are there in the html source, but in R-devel
  there is a single \verb{<p>} tag enclosing the whole Sexpr,
  previously, and correctly, each letter is enclosed with p-tags.

  Now, the same \emph{without} surrounding new lines in the
  \emph{\file{Rd}}:\cr
  Line beginning:
\Sexpr[stage=build,results=rd]{paste0("a", 1:3, collapse = "\n\n")}
  \cr
  Indented \verb{\Sexpr[*]{..}} (by 2 spaces):
  \Sexpr[stage=build,results=rd]{paste0("a", 1:3, collapse = "\n\n")}
}
\note{
  System information:

\Sexpr[stage=build,results=rd]{paste0("\\\\preformatted{\n", paste0(capture.output(print(sessionInfo())), collapse="\n"), "\n}")}
}
