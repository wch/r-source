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


## \Sexpr[stage=build, results=hide]{ <a dozen "empty" lines> }
tf <- textConnection("RdTeX", "w")
Rd2latex("Rd-Sexpr-hide-empty.Rd", tf, stages="build")
tex <- textConnectionValue(tf); close(tf); rm(tf)
(H2end <- tex[grep("^Hello", tex):length(tex)])
stopifnot((n <- length(H2end)) <= 4, # currently '3'; was 13 in R < 4.2.0
          H2end[-c(1L,n)] == "")     # also had \\AsIs{ .. }  " "  "   "


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


## \doi with hash symbol or Rd specials
rd <- parse_Rd("doi.Rd")
writeLines(out <- capture.output(Rd2txt(rd, stages = "build")))
stopifnot(grepl("10.1000/456#789", out[5], fixed = TRUE),
          grepl("doi.org/10.1000/456%23789", out[5], fixed = TRUE),
          grepl("10.1000/{}", out[7], fixed = TRUE),
          grepl("doi.org/10.1000/%7B%7D", out[7], fixed = TRUE))
## R < 4.2.0 failed to encode the hash and lost {}
