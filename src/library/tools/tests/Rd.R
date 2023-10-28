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


## \title and \section name should not end in a period
rd <- parse_Rd(textConnection(r"(
\name{test}
\title{title.}
\description{description}
\section{section.}{nothing}
)"))
stopifnot(identical(endsWith(print(checkRd(rd)), "end in a period"),
                    rep(TRUE, 2)))

## checkRd() with duplicated \name (is documented to fail from prepare_Rd)
assertError(checkRd(parse_Rd(textConnection(r"(
\name{test}\title{test}\name{test2}
)"))), verbose = TRUE)
## no error in R < 4.4.0

## prepared NEWS should check cleanly
NEWS_Rd <- readRDS(file.path(R.home("doc"), "NEWS.rds"))
stopifnot(inherits(NEWS_Rd, "Rd"),
          length(print(checkRd(NEWS_Rd))) == 0L)
## "Must have a \description" in R < 4.4.0, now moved to checkRdContents()

## checkRd() raises some instances of "lost braces"
Sys.setenv("_R_CHECK_RD_NOTE_LOST_BRACES_" = TRUE)
bad <- function (Rd) sum(startsWith(checkRd(Rd), "checkRd: (-1) "))
stopifnot(bad("Rd-braces_ignored.Rd") == 0L,
          bad("Rd-braces_reported.Rd") == 10L)


## An unmatched un-escaped '{' in a comment in \examples{} ... should *NOT* trip up, but does

txt <- r"(\title{Commented left-brace in Example}
\name{ex-comm-brace}
\examples{
  if(1 <= 11) { # if(require("MASS")) \{  << only works when escaped with '\\'
    fractions(355, 112)
  }% if(.)
}
\keyword{misc})"
## these all work fine:

(rd1 <- parse_Rd(con <- textConnection(txt))); close(con)
Rd2ex(rd1)
Rd2txt(rd1)
## etc

## however: When I try the bare "{" instead of  "\{"
txt0 <- sub("\\{", "{", txt, fixed=TRUE)
stopifnot(nchar(txt0) == nchar(txt) - 1)
## This currently gives a warning .. and later problems {-> package checking etc}
rd0 <- parse_Rd(con0 <- textConnection(txt0)); close(con0)
## Warning message:
## In parse_Rd(con0) : <connection>:8: unexpected section header '\keyword'
checkRd(rd0)
Rd2ex(rd0) # shows extra "}" and "{misc}"
