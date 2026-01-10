require("tools")

# -------------------------------------------------------------------
# find_wide_Rd_lines_in_Rd_object: render stage=render \Sexpr
# expressions within \examples if installed = TRUE.

rd <- sprintf("
\\name{foo}
\\title{Title}
\\description{Desc.}
\\examples{
  \\Sexpr[stage=render]{\"# foobar\"}
  \\Sexpr[stage=render]{strrep(\"long \", 30)}
  # %s
}", strrep("123456789 ", 10))

rd <- parse_Rd(con <- textConnection(rd)); close(con)

# does not error, but finds long lines, dynamic ones as well
bad <- tools:::find_wide_Rd_lines_in_Rd_object(rd, installed = TRUE)
stopifnot(
  "examples" %in% names(bad),
  "warn" %in% names(bad$examples),
  any(grepl("123456789 ", bad$examples$warn)),
  any(grepl("long ", bad$examples$warn))
)

# does error currently
err <- NULL
tryCatch(
  tools:::find_wide_Rd_lines_in_Rd_object(rd, installed = FALSE),
  error = function(e) err <<- e
)
stopifnot(!is.null(err))


## Rdiff(nullPointers=TRUE) did not convert glibc's (nil) and so
## reported false positive differences if <pointer: (nil)> was not
## a leading substring
RdiffLines <- function(txt0, txt1) {
    con0 <- textConnection(txt0)
    con1 <- textConnection(txt1)
    on.exit({ close(con0); close(con1) })
    Rdiff(con0, con1, Log = TRUE)
}
txt0 <- c(LETTERS, "<pointer: (nil)>", "... <pointer: (nil)>", letters)
txt1 <- c(LETTERS, "<pointer: 0xfff>", "... <pointer: 0xfff>", letters)
L <- RdiffLines(txt0, txt1) # previous output:
## 
## 27c27
## < ... <pointer: (nil)>
## ---
## > ... <pointer: 0>
stopifnot(identical(L, list(status = 0L, out = character(0L))))
