.Defunct <- function() {
    stop(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
	       "is defunct.\n",
	       "See ?Defunct.",
	       sep = ""))
}
dnchisq <- function(x, df, lambda) .Defunct()
pnchisq <- function(q, df, lambda) .Defunct()
qnchisq <- function(p, df, lambda) .Defunct()
#rnchisq <- function(...) .NotYetImplemented()

print.plot <- function() .Defunct()
save.plot <- function(file = "Rplots.ps") .Defunct()

## From print.R :
## This is not used anymore [replaced by  print.anova() -> ./anova.R ]
##- print.tabular <-
##-	function(x, digits = max(3, .Options$digits - 3), na.print = "")
##- {
##-	cat("\n", if(!is.null(x$title))
##-	x$title else "Analysis of Variance:", "\n\n", sep="")
##-	if(!is.null(x$topnote))
##-	cat(paste(x$topnote, collapse="\n"), "\n\n", sep="")
##-	print.default(x$table, digits=digits, na = "", print.gap = 2)
##-	if(!is.null(x$botnote))
##-	cat("\n", paste(x$botnote, collapse="\n"), sep="")
##-	cat("\n")
##- }
print.tabular <-
    function(table, digits = max(3, .Options$digits - 3), na.print = "", ...)
	.Defunct()

## From lm.R :
## Unused (0.63, Sept.25 1998) --- print.anova()  now in ./print.R
##- print.anova.lm <- function(x, digits = max(3, .Options$digits - 3), ...)
##- {
##-	cat("\nAnalysis of Variance:\n\n")
##-	print.default(round(unclass(x), digits), na="", print.gap=2)
##-	cat("\n")
##-	invisible(x)
##- }
print.anova.lm <- function(x, digits = max(3, .Options$digits - 3), ...)
    .Defunct()

## From glm.R :
## Not used anymore..
##- print.anova.glm <- function(x, digits = max(3, .Options$digits - 3),
##-			    na.print = "", ...)
##- {
##-	cat("\n", x$title, sep="")
##-	print.default(x$table, digits=digits, na = "", print.gap = 2)
##-	cat("\n")
##- }
print.anova.glm <- .Alias(print.anova.lm)

system.test <- function(...)
  .Defunct()
