\name{pictex}
\alias{pictex}
\title{A PicTeX Graphics Driver}
\usage{
pictex(file = "Rplots.tex", width = 5, height = 4, debug = FALSE,
       bg = "white", fg = "black")
}
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
\details{
  This driver does not have any font metric information, so the use of
  \code{\link{plotmath}} is not supported.

  Multiple plots will be placed as separate environments in the output file.
}
\references{
  Knuth, D. E. (1984)
  \emph{The TeXbook.}
  Reading, MA: Addison-Wesley.

  Lamport, L. (1994)
  \emph{LATEX: A Document Preparation System.}
  Reading, MA: Addison-Wesley.

  Goossens, M., Mittelbach, F. and Samarin, A. (1994)
  \emph{The LATEX Companion.}
  Reading, MA: Addison-Wesley.
}
\author{
  This driver was provided by Valerio Aimale
  \email{valerio@svpop.com.dist.unige.it}
  of the Department of Internal Medicine, University of Genoa, Italy.
}
\seealso{
  \code{\link{postscript}}, \code{\link{Devices}}.
}
\examples{
pictex()
plot(1:11,(-5:5)^2, type='b', main="Simple Example Plot")
dev.off()
##--------------------
\dontrun{
\%\% LaTeX Example
\\documentclass{article}
\\usepackage{pictex}
\\begin{document}
\%...
\\begin{figure}[h]
  \\centerline{\\input{Rplots.tex}}
  \\caption{}
\\end{figure}
\%...
\\end{document}

\%\%-- TeX Example --
\\input pictex
$$ \\input Rplots.tex $$
}
##--------------------
unlink("Rplots.tex")
}
\keyword{device}
%%-
%%--- on CTAN, searching for "pictex" in file FILES.byname  [April 3, 1998]:
% 1994/04/20 |        473 | graphics/pictex/addon/00index
% 1994/04/20 |       1568 | graphics/pictex/addon/piccorr.sty
% 1994/04/20 |       2952 | graphics/pictex/addon/picmore.tex
% 1994/04/20 |        311 | graphics/pictex/addon/pictex.sty
% 1994/04/20 |        416 | graphics/pictex/addon/pictexwd.sty
% 1994/04/20 |     133232 | graphics/pictex/addon/pictexwd.tex
% 1994/04/20 |        457 | graphics/pictex/addon/pictexzusatz.txt
% 1992/08/27 |       3041 | graphics/pictex/errorbars.tex
% 1992/08/27 |      10519 | graphics/pictex/latexpicobjs.tex
% 1992/08/27 |     129709 | graphics/pictex/pictex.tex
% 1992/08/27 |       1322 | graphics/pictex/pointers.tex
% 1992/08/27 |       1399 | graphics/pictex/postpictex.tex
% 1992/08/27 |       1082 | graphics/pictex/prepictex.tex
% 1992/08/27 |        175 | graphics/pictex/readme
% 1992/08/27 |        876 | graphics/pictex/readme.errorbars
% 1989/11/26 |       7270 | graphics/pictex/texpictex.tex
% 1992/08/31 |      11382 | graphics/pictex/tree.sty
% 1994/01/13 |        162 | info/stammtisch/wuppertal/updates/pictex.sty
% 1997/07/26 |      58723 | macros/context/generic/doc/m-pictex.pdf
% 1997/04/29 |       8053 | macros/context/generic/tex/m-pictex.tex
% 1997/08/20 |      58724 | macros/context/pictex/doc/m-pictex.pdf
% 1997/08/20 |       1261 | macros/context/pictex/doc/readme.de
% 1997/08/20 |       1207 | macros/context/pictex/doc/readme.en
% 1997/08/20 |       1323 | macros/context/pictex/doc/readme.nl
% 1997/08/20 |        145 | macros/context/pictex/tex/m-pictex.sty
% 1997/08/20 |       8689 | macros/context/pictex/tex/m-pictex.tex
% 1997/06/30 |         39 | macros/context/ppchtex/tex/m-pictex.sty
% 1997/04/29 |       8053 | macros/context/ppchtex/tex/m-pictex.tex
% 1994/01/14 |        162 | macros/latex209/contrib/konstruk/pictex.sty
% 1992/08/27 |       7251 | macros/plain/contrib/texpictex.tex
% 1991/07/29 |      27946 | support/pictex-converter/PiCUNIX.PASCAL
% 1991/07/25 |      23812 | support/pictex-converter/piccon1.uue
% 1991/07/25 |      25756 | support/pictex-converter/piccon2.uue
% 1991/07/25 |        379 | support/pictex-converter/picconv.anleit
% 1991/07/25 |      35840 | support/pictex-converter/picconv.lzh
% 1991/07/25 |       1906 | support/pictex-converter/pictex.info-cb
% 1991/07/25 |      54800 | support/pictex-converter/pictexte.exe
% 1991/07/25 |      43757 | support/pictex-converter/pictexte.pas
% 1992/08/25 |        835 | support/qfig/q-pictex.tex
% 1991/10/11 |      51141 | systems/atari/texutils/texdraw/pictex.tos
% 1995/07/13 |      45973 | systems/msdos/4alltex/disk04/pictex.zip
% 1995/06/26 |      39887 | systems/msdos/emtex/pictex.zip
% 1998/02/02 |        676 | systems/unix/linux/fastpictex-0.3.lsm
% 1998/02/02 |     309581 | systems/unix/linux/fastpictex-0.3.tar.gz
% 1997/01/27 |      73930 | systems/unix/teTeX/distrib/goodies/pictex.tar.gz
