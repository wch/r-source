\name{interaction.plot}
\alias{interaction.plot}
\title{Two-way Interaction Plot}
\description{
  Plots the mean (or other summary) of the response for two-way
  combinations of factors, thereby illustrating possible interactions.
}
\usage{
interaction.plot(x.factor, trace.factor, response, fun = mean,
                 type = c("l", "p", "b"), legend = TRUE,
                 trace.label = deparse(substitute(trace.factor)),
                 fixed = FALSE,
                 xlab = deparse(substitute(x.factor)),
                 ylab = ylabel,
                 ylim = range(cells, na.rm=TRUE),
                 lty = nc:1, col = 1, pch = c(1:9, 0, letters),
                 xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
                 xtick = FALSE, xaxt = par("xaxt"), axes = TRUE,
                 \dots)
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
  \item{fixed}{logical.  Should the legend be in the order of the levels
    of \code{trace.factor} or in the order of the traces at their right-hand ends?}
  \item{xlab,ylab}{the x and y label of the plot each with a sensible default.}
  \item{ylim}{numeric of length 2 giving the y limits for the plot.}
  \item{lty}{line type for the lines drawn, with sensible default.}
  \item{col}{the color to be used for plotting.}
  \item{pch}{a vector of plotting symbols or characters, with sensible
    default.}
  \item{xpd}{determines clipping behaviour for the \code{\link{legend}}
    used, see \code{\link{par}(xpd)}.  Per default, the legend is
    \emph{not} clipped at the figure border.}
  \item{leg.bg, leg.bty}{arguments passed to \code{\link{legend}()}.}
  \item{xtick}{logical. Should tick marks be used on the x axis?}
  \item{xaxt, axes, \dots}{graphics parameters to be passed to the plotting routines.}
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
\references{
  Chambers, J. M., Freeny, A and Heiberger, R. M. (1992)
  \emph{Analysis of variance; designed experiments.}
  Chapter 5 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}

\examples{
attach(ToothGrowth)
interaction.plot(dose, supp, len, fixed=TRUE)
dose <- ordered(dose)
interaction.plot(dose, supp, len, fixed=TRUE, col = 2:3, leg.bty = "o")
interaction.plot(dose, supp, len, fixed=TRUE, col = 2:3, type = "p")
detach()

with(OrchardSprays, {
  interaction.plot(treatment, rowpos, decrease)
  interaction.plot(rowpos, treatment, decrease, cex.axis=0.8)
  ## order the rows by their mean effect
  rowpos <- factor(rowpos, levels=sort.list(tapply(decrease, rowpos, mean)))
  interaction.plot(rowpos, treatment, decrease, col = 2:9, lty = 1)
})

with(esoph, {
  interaction.plot(agegp, alcgp, ncases/ncontrols, main = "'esoph' Data")
  interaction.plot(agegp, tobgp, ncases/ncontrols, trace.label="tobacco",
		   fixed=TRUE, xaxt = "n")
})
## deal with NAs:
esoph[66,] # second to last age group: 65-74
esophNA <- esoph; esophNA$ncases[66] <- NA
with(esophNA, {
  interaction.plot(agegp, alcgp, ncases/ncontrols, col= 2:5)
				# doesn't show *last* group either
  interaction.plot(agegp, alcgp, ncases/ncontrols, col= 2:5, type = "b")
  ## alternative take non-NA's  {"cheating"}
  interaction.plot(agegp, alcgp, ncases/ncontrols, col= 2:5,
                   fun = function(x) mean(x, na.rm=TRUE),
                   sub = "function(x) mean(x, na.rm=TRUE)")
})
rm(esophNA) # to clear up
}
\keyword{hplot}
