\name{interaction.plot}
\alias{interaction.plot}
\title{Two-way Interaction Plot}
\description{
  Plots the mean (or other summary) of the response for two-way
  combinations of factors, thereby illustrating possible interactions.
}
\usage{
interaction.plot(x.factor, trace.factor, response, fun = mean,
                 type = c("l", "p"), legend = TRUE,
                 trace.label = deparse(substitute(trace.factor)),
                 fixed = FALSE, xlab, ylab, ylim, lty, col = 1,
                 pch = c(1:9, 0, letters), \dots)
}
\arguments{
  \item{x.factor}{a factor whose levels will form the x axis.}
  \item{trace.factor}{another factor whose levels will form the traces.}
  \item{response}{a numeric variable giving the response}
  \item{fun}{the function to compute the summary. Should return a single
    real value.}
  \item{type}{the type of plot: lines or points.}
  \item{legend}{logical. Should a legend be included?}
  \item{trace.label}{overall label for the legend.}
  \item{fixed}{Should the legend be in the order of the levels of
    \code{trace.factor} or in the order of the traces at their
    right-hand ends?}
  \item{xlab}{the x label of the plot.}  
  \item{ylab}{the y label of the plot.}
  \item{ylim}{numeric of length 2 giving the y limits for the plot.}
  \item{lty}{line type for the lines drawn, with sensible default.}
  \item{col}{the color to be used for plotting.}
  \item{pch}{a vector of plotting symbols or characters, with sensible
    default.}
  \item{\dots}{graphics parameters to be passed to the plotting routines.}
}
\details{
  By default the levels of \code{x.factor} are plotted on the x axis in
  their given order, with extra space left at the right for the legend
  (if specified). If \code{x.factor} is an ordered factor and the levels
  are numeric, these numeric values are used for the x axis.

  The response and hence its summary can contain missing values. If so,
  the missing values and the line segments joining them are omitted from
  the plot (and this can be somewhat disconcerting).
  
  The graphics parameters \code{xlab}, \code{ylab}, \code{ylim},
  \code{lty}, \code{col} and \code{pch} are given suitable defaults
  (and \code{xlim} and \code{xaxs} are set and cannot be overriden).
  The defaults are to cycle through the line types, use the foreground
  colour, and to use the symbols 1:9, 0, and the capital letters to plot
  the traces.
}
\note{
  Some of the argument names and the precise behaviour are chosen for
  S-compatibility.
}

\examples{
data(ToothGrowth)
attach(ToothGrowth)
interaction.plot(dose, supp, len, fixed=TRUE)
dose <- ordered(dose)
interaction.plot(dose, supp, len, fixed=TRUE)
detach()

data(OrchardSprays)
attach(OrchardSprays)
interaction.plot(treatment, rowpos, decrease)
interaction.plot(rowpos, treatment, decrease)
## order the rows by their mean effect
rowpos <- factor(rowpos, levels=sort.list(tapply(decrease, rowpos, mean)))
interaction.plot(rowpos, treatment, decrease)
detach()

data(esoph)
attach(esoph)
interaction.plot(agegp, alcgp, ncases/ncontrols)
interaction.plot(agegp, tobgp, ncases/ncontrols, trace.label="tobacco",
                 fixed=TRUE)
detach()
}
\keyword{hplot}
