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
