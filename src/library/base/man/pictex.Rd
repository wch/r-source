\name{pictex}
\title{A PicTeX Graphics Driver}
\usage{
pictex(file = "Rplots.tex", width = 5, height = 4, debug = FALSE,
       bg = "white", fg = "black")
}
\alias{pictex}
\arguments{
  \item{file}{the file where output will appear.}
  \item{width}{The width of the plot in inches.}
  \item{height}{the height of the plot in inches.}
  \item{debug}{should debugging information be printed.}
  \item{bg}{the background color for the plot.}
  \item{fg}{the foreground color for the plot.}
}
\description{
  This function produces graphics suitable for inclusion in TeX and
  LaTeX documents.
}
\references{
  Knuth, D. E. (1984).
  \emph{The TeXbook}.
  Reading, MA: Addison-Wesley.

  Lamport, L. (1994).
  \emph{LATEX: A Document Preparation System}.
  Reading, MA: Addison-Wesley.

  Goossens, M., F. Mittelbach and A. Samarin (1994).
  \emph{The LATEX Companion}.
  Reading, MA: Addison-Wesley.
}
\note{
  This driver was provided by Valerio Aimalea
  \email{valerio@svpop.com.dist.unige.it}
  of the Department of Internal Medicine, University of Genoa, Italy.
}
\seealso{
  \code{\link{postscript}}, \code{\link{Devices}}.
}
\examples{
%% LaTeX Example
\documentclass{article}
\usepackage{pictex}
\begin{document}
%...
\begin{figure}[h]
  \centerline{\input{Rplots.tex}}
  \caption{}
\end{figure}
%...
\end{document}

% TeX Example
\input pictex
$$ \input Rplots.tex $$
}
\keyword{device}
