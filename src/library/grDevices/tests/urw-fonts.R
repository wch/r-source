### Tests of non-Latin-1 plotting in PDF with URW fonts
## Derived from encodings2.R

### Test original URW fonts.

## These have good coverage except KOI8-[RU] (0x80 to 0xLBF are missing)
## but no metric info, not even widths for most Greek glyphs.

options(warn = 1L)

### only do this in a UTF-8 locale
if (!l10n_info()[["UTF-8"]]) {
    warning("URWfonts.R requires a UTF-8 locale")
    q("no")
}
musl <- grepl("musl", R.version$os)

do_one <-function(name, encoding, all = FALSE)
{
    par(pty="s")
    plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="", xaxs="i", yaxs="i")
    title(paste("Centred chars in", name))
    grid(17, 17, lty=1)
    known <- if(all) c(32:126, 128:255) else c(32:126, 160:255)
    x <- rawToChar(as.raw(known), TRUE)
    z <- rep(NA_character_, 256)
    z[known] <- iconv(x, encoding, "UTF-8")

    for(i in known) {
        x <- i %% 16
        y <- i %/% 16
        points(x, y, pch = z[i])
    }
    par(pty="m")
}

pdfenc <- c("ISOLatin1", "ISOLatin2", "ISOLatin7", "ISOLatin9",
           "Greek", "Cyrillic", "KOI8-R", "KOI8-U",
           "WinAnsi", "CP1250", "CP1251", "CP1253", "CP1257")
if(musl) {
    enc <- c("latin1", "iso88592", "iso885913", "iso885915",
             "iso88597", "iso88595", "KOI8-R", "KOI8-U",
             "CP1252", "CP1250", "CP1251", "CP1253", "CP1257")
} else {
    enc <- c("latin1", "latin2", "iso-8859-13", "latin-9",
             "iso-8859-7", "iso-8859-5", "KOI8-R", "KOI8-U",
             "CP1252", "CP1250", "CP1251", "CP1253", "CP1257")
}

for(i in seq_along(pdfenc)) {
    message("\ntesting ", pdfenc[i])
    ff <- paste0("URW-", pdfenc[i], ".pdf")
    f <- tempfile(fileext = ".pdf")
    ## encoding may not work (e.g. latin2 did not in musl), so use try()
    res <- try(pdf(f, encoding = pdfenc[i], width = 9, height = 9,
                   family = "NimbusSan"))
    if(!inherits(res, "try-error")) {
        ## and this may not work either
        try(do_one(pdfenc[i], enc[i], i >= 7))
        dev.off()
        ## needs ghostscript
        try(embedFonts(f, , ff))
        unlink(f)
    }
}

## and repeat for URW 2.0 fonts

## These have metrix info for Greek glyphs, but a few widths missing
## and a few Greek glyphs (drachma, Omega, mu, sigma1) are not
## renedered or not centered.  In part this is because the afms do not
## contain data for these.

## <FIXME>
## With c86124 this causes make check-devel to fail if Ghostscript is
## not found.
## ## follow embedFonts
## if (!nzchar(tools::find_gs_cmd()))
##     stop("GhostScript is required and was not found")
## </FIXME>

for(i in seq_along(pdfenc)) {
    message("\ntesting ", pdfenc[i])
    ff <- paste0("URW2-", pdfenc[i], ".pdf")
    f <- tempfile(fileext = ".pdf")
    ## encoding may not work (e.g. latin2 did not in musl), so use try()
    res <- try(pdf(f, encoding = pdfenc[i], width = 9, height = 9,
                   family = "URW2Helvetica"))
    if(!inherits(res, "try-error")) {
        ## and this may not work either
        try(do_one(pdfenc[i], enc[i], i >= 7))
        dev.off()
        try(embedFonts(f, , ff))
        unlink(f)
    }
}
