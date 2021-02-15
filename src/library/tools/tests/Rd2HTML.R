require("tools")

x <- Rd_db("base")
system.time(y <- lapply(x, function(e)
    tryCatch(Rd2HTML(e, out = nullfile()), error = identity))) # 3-5 sec
stopifnot(!vapply(y, inherits, NA, "error"))
## Gave error when "running" \Sexpr{.} DateTimeClasses.Rd


## PR#18052: \dots must not be interpreted inside \preformatted
Rdsnippet <- tempfile()
writeLines(r"(\preformatted{
\item{\dots}{foo(arg = "\\\\dots", ...)}
})", Rdsnippet)
#file.show(Rdsnippet)
stopifnot(exprs = {
    identical(capture.output(Rd2HTML(Rdsnippet, fragment = TRUE))[2L],
              r"(\item{\dots}{foo(arg = "\\dots", ...)})")
    identical(capture.output(Rd2txt(Rdsnippet, fragment = TRUE))[2L],
              r"(\item{\dots}{foo(arg = "\\dots", ...)})")
    identical(capture.output(Rd2latex(Rdsnippet, fragment = TRUE))[2L],
              r"(\bsl{}item\{\bsl{}dots\}\{foo(arg = "\bsl{}\bsl{}dots", ...)\})")
}) # the last two failed in R < 4.1.0

## also do not translate \dots in R code lines in \examples
Rdsnippet <- tempfile()
writeLines(r"(\examples{
foo <- function(arg = "\\\\dots", ...) NULL # \dots
})", Rdsnippet)
#file.show(Rdsnippet)
stopifnot(exprs = {
    identical(capture.output(Rd2ex(parse_Rd(Rdsnippet), fragment = TRUE))[5L],
              r"(foo <- function(arg = "\\dots", ...) NULL # \dots)")
}) # failed in R < 4.1.0
