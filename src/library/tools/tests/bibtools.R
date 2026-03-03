## To be compared against saved reference output
if (isNamespaceLoaded("tools")) unloadNamespace("tools")
Sys.setenv("R_HELP_BIBSTYLE" = "JSS") # fix \bibshow style
library(tools)
Rd2txt_options(underline_titles = FALSE)

## Test processing and conversion of bibtools.Rd
Rd2txt("bibtools.Rd", stages = "build") |>
    assertWarning(verbose = TRUE)

## Also test for changes in HTML conversion
Rd2HTML("bibtools.Rd", htmlfile <- tempfile(),
        stages = "build", standalone = FALSE) |>
    assertWarning() # the same
## FIXME: carry-over of unshown citation (cache not cleared)
Rdiff(htmlfile, "bibtools.html.save")

## Check cited but not shown
pkgdir <- package.skeleton("bibtools", list = ".NotYetImplemented",
                           path = tempdir()) |> suppressMessages()
file.copy("bibtools.Rd", file.path(pkgdir, "man")) |> stopifnot()
tools:::.check_Rd_bibentries_cited_not_shown(dir = pkgdir)
## ("unexpected_macro_expansion" as there is no build/partial.rdb)


## FIXME: "srcref" of usermacro expansion such as from \bibshow{*}
rd <- parse_Rd(textConnection(r"(\newcommand{\Emph}{\emph{#1}}
\Emph{this}
)"), fragment = TRUE, verbose = TRUE, macros = FALSE)
rd
print(lapply(rd, getSrcref), useSource = FALSE)
## In R < 4.6.0, the expansion [[4]] referred to source "chars 2:12 to 2:11".
## With the correct start column 1, also the old Rd2HTML() does addParaBreaks()
## for multiple references separated by blank lines in bibshow's \Sexpr result.
