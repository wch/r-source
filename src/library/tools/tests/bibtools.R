## To be compared against saved reference output
if (isNamespaceLoaded("tools")) unloadNamespace("tools")
Sys.setenv("R_HELP_BIBSTYLE" = "JSS") # fix \bibshow style
library(tools)
Rd2txt_options(underline_titles = FALSE)

## Test processing and conversion of bibtools.Rd
Rd2txt("bibtools.Rd", stages = "build") |>
    assertWarning(verbose = TRUE)

## Also test for changes in HTML conversion
Rd2HTML("bibtools.Rd", out <- "bibtools.html",
        stages = "build", standalone = FALSE) |>
    assertWarning() # the same
## FIXME: carry-over of unshown citation (cache not cleared)
if (!Rdiff(out, "bibtools.html.save")) unlink(out)

## Check cited but not shown
pkgdir <- package.skeleton("bibtools", list = ".NotYetImplemented",
                           path = tempdir()) |> suppressMessages()
file.copy("bibtools.Rd", file.path(pkgdir, "man")) |> stopifnot()
tools:::.check_Rd_bibentries_cited_not_shown(dir = pkgdir)
## ("unexpected_macro_expansion" as there is no build/partial.rdb)
