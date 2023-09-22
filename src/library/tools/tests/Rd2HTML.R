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

## \usage: keep quoted "\\\\dots", but _do_ translate formal \dots arg
Rdsnippet <- tempfile()
writeLines(r"(\name{foo}\title{foo}\usage{
## keep this comment to ensure a newline at the end
foo(arg = "\\\\dots", \dots)
})", Rdsnippet)
Rdobj <- parse_Rd(Rdsnippet)
check_dots_usage <- function(FUN) {
    out <- trimws(grep("foo(", capture.output(FUN(Rdobj)),
                       value = TRUE, fixed = TRUE))
    if (!identical(out, r"(foo(arg = "\\dots", ...))"))
        stop("unexpected output: ", out)
}
check_dots_usage(Rd2HTML)
check_dots_usage(Rd2txt)
check_dots_usage(Rd2latex)
## the last two failed in R < 4.1.0; output was foo(arg = "\...", ...)
