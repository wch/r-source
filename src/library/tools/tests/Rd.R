require("tools")

# -------------------------------------------------------------------
# prepare_Rd() is OK with a top level \Sexpr that is yet to be rendered

txt <- "
\\name{foo}
\\title{Title}
\\description{Desc.}
\\Sexpr[stage=render,results=rd]{\"\\\\\\details{This is dynamic.}\"}
"

rd <- parse_Rd(con <- textConnection(txt)); close(con)

warn <- NULL
withCallingHandlers(
  rd2 <- tools:::prepare_Rd(rd),
  warning = function(w) { warn <<- w; invokeRestart("muffleWarning") }
)
stopifnot(is.null(warn))
stopifnot("\\Sexpr" %in% tools:::RdTags(rd2))

## checkRd() gives file name and correct line number of \Sexpr[results=rd] chunk
stopifnot(grepl("Rd-Sexpr-warning.Rd:5:",
                print(checkRd("Rd-Sexpr-warning.Rd", stages = "build")),
                fixed = TRUE))

## processRdChunk() gives file name and location of eval error
(msg <- tryCatch(checkRd(file_path_as_absolute("Rd-Sexpr-error.Rd")),
                 error = conditionMessage))
stopifnot(startsWith(msg, "Rd-Sexpr-error.Rd:4-7:"),
          length(checkRd("Rd-Sexpr-error.Rd", stages = NULL)) == 0)
## file name and line numbers were missing in R < 4.2.0
