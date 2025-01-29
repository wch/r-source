## test rendering the PDF manual of a package with non-ASCII help and \figure
library("tools")

if (interactive()) setwd(tempdir()) # keep srcdir clean
## unlink(c("pkg.*.pdf", "lib", "lib_latex", "pkg.latin1", "pkg.utf8"), recursive=TRUE)

create_pkg <- function (name = "pkg.utf8", encoding = "UTF-8")
{
    write_DESCRIPTION <- function (dir, package = "pkg", encoding = "UTF-8")
    {
        desc <- c(
            "Package"  = package,
            "Title"    = "Package with Non-ASCII Help and Figure",
            "Version"  = "1.0",
            "Author"   = "D\u00e9j\u00e0 Vu", # Déjà Vu
            "Maintainer" = "\u00d3RF\u00c3O <noreply@R-project.org>", # ÓRFÃO
            "Description" = "Gr\u00fc\u00dfe.", # Grüße
            "License"  = "GPL-2",
            "Encoding" = encoding
        )
        write.dcf(t(desc), file(file.path(dir, "DESCRIPTION"), encoding = encoding))
    }

    dir.create(pkgdir <- name)
    dir.create(mandir <- file.path(pkgdir, "man"))
    dir.create(figuresdir <- file.path(mandir, "figures"))
    file.copy(file.path(R.home("doc"), "html", "Rlogo.pdf"), figuresdir) |> stopifnot()
    write_DESCRIPTION(pkgdir, package = name, encoding = encoding)
    promptPackage(name,
                  filename = file(file.path(mandir, paste0(name, "-package.Rd")),
                                  encoding = encoding),
                  final = TRUE) |> suppressMessages()
    writeLines(c("\\name{figure}", "\\alias{figure}",
                 "\\title{Ilustraci\u00f3n}",
                 "\\description{\\figure{Rlogo.pdf}{options: width=1cm}}"),
               file(file.path(mandir, "figure.Rd"), encoding = encoding))
    pkgdir
}

Rd2pdf_opts <- c("--quiet", "--no-preview", "--force", "--no-index")
INSTALL_opts <- c("--no-staged-install", "--no-lock", "--no-test-load")

for (encoding in c("UTF-8", "latin1"))
{
    pkgname <- paste0("pkg.", tools:::latex_canonical_encoding(encoding))
    pkgdir <- create_pkg(pkgname, encoding)

    ## Render a single Rd file
    rdfile <- file.path(pkgdir, "man", "figure.Rd")
    Rcmd(c("Rd2pdf", Rd2pdf_opts, paste0("--encoding=", encoding),
           sprintf("--output=%s-figure.pdf", pkgname), rdfile)) == 0L ||
        stop("failed to render ", sQuote(rdfile))

    ## Render the package manual
    ## - the following three variants all (over)write <pkgname>.pdf
    ## - the first two are skipped as they are also exercised via check-all
    ##   (base pkgs are rendered from source, recommended from installed help)
    ## - currently only the exit code is checked, not generated content
    if (interactive()) {
        Rcmd(c("Rd2pdf", Rd2pdf_opts, pkgdir)) == 0L ||
            stop("failed to render from source directory of ", sQuote(pkgname))

        stopifnot(dir.exists("lib") || dir.create("lib"))
        install.packages(pkgdir, lib = "lib", repos = NULL, type = "source",
                         INSTALL_opts = INSTALL_opts)
        Rcmd(c("Rd2pdf", Rd2pdf_opts, file.path("lib", pkgname))) == 0L ||
            stop("failed to render from installed ", sQuote(pkgname))
    }
    stopifnot(dir.exists("lib_latex") || dir.create("lib_latex"))
    install.packages(pkgdir, lib = "lib_latex", repos = NULL, type = "source",
                     INSTALL_opts = c(INSTALL_opts, "--latex"))
    Rcmd(c("Rd2pdf", Rd2pdf_opts, file.path("lib_latex", pkgname))) == 0L ||
        stop("failed to render preconverted LaTeX help for ", sQuote(pkgname))
    ## this failed in R 4.4.[012] due to undefined \inputencoding, but also,
    ## in R < 4.5.0, due to undefined \includegraphics and unset \graphicspath,
    ## and for "latin1" input from a fragile sub() of the \HeaderA line.
}
