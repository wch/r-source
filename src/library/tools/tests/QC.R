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
