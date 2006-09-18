.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    ## Set default options() related to functionality in 'utils' pkg
    op <- options()
    op.utils <-
	list(help.try.all.packages = FALSE,
	     internet.info = 2,
	     pkgType = .Platform$pkgType,
	     str = list(strict.width = "no", digits.d = 3, vec.len = 4),
	     example.ask = "default",
	     HTTPUserAgent = defaultUserAgent(),
	     menu.graphics = TRUE)
    extra <-
        if(.Platform$OS.type == "windows") {
            list(mailer = "none",
                 unzip = "internal",
                 editor = if(any(grep("Rgui", commandArgs(), TRUE))) "internal" else "notepad",
                 repos = c(CRAN="@CRAN@",
                 CRANextra="http://www.stats.ox.ac.uk/pub/RWin")
                 )
        } else
            list(mailer = "mailx",
                 unzip = as.vector(Sys.getenv("R_UNZIPCMD")),
                 editor = as.vector(Sys.getenv("EDITOR")),
                 repos = c(CRAN="@CRAN@"))
    op.utils <- c(op.utils, extra)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}
