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
