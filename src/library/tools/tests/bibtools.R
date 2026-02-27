## To be compared against saved reference output
library(tools)
Rd2txt_options(underline_titles = FALSE)
Rd2txt("bibtools.Rd", stages = "build") |>
    assertWarning(verbose = TRUE)

## Check cited but not shown
pkgdir <- package.skeleton("bibtools", list = ".NotYetImplemented",
                           path = tempdir()) |> suppressMessages()
file.copy("bibtools.Rd", file.path(pkgdir, "man")) |> stopifnot()
tools:::.check_Rd_bibentries_cited_not_shown(dir = pkgdir)
## ("unexpected_macro_expansion" as there is no build/partial.rdb)
