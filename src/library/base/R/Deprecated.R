###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste("`", as.character(sys.call(sys.parent())[[1]]), "' ",
		  "is deprecated.\n",
		  if (!missing(new))
		  paste("Use `", new, "' instead.\n", sep = ""),
		  "See ?Deprecated.",
		  sep = ""))
}

dnchisq <- function(x, df, lambda) {
    .Deprecated("dchisq")
    .Internal(dnchisq(x, df, lambda))
}
pnchisq <- function(q, df, lambda) {
    .Deprecated("pchisq")
    .Internal(pnchisq(q, df, lambda))
}
qnchisq <- function(p, df, lambda) {
    .Deprecated("qchisq")
    .Internal(qnchisq(p, df, lambda))
}
rnchisq <- function(...) .NotYetImplemented()

print.plot <- function() {
    .Deprecated("dev.print")
    FILE <- tempfile()
    dev.print(file = FILE)
    system(paste(options()$printcmd, FILE))
    unlink(FILE)
}
save.plot <- function(file = "Rplots.ps") {
    .Deprecated("dev.print")
    dev.print(file = file)
}

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
    function(table, digits = max(3, .Options$digits - 3), na.print = "", ...) {
	.Deprecated("print.anova")
	print.anova(table, digits=digits, na.print=na.print, ...)
    }

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
{
    .Deprecated("print.anova")
    print.anova(x, digits=digits, ...)
}
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
{
  .Deprecated("file.exists  etc")
  system(paste("test", ...)) == 0
}
