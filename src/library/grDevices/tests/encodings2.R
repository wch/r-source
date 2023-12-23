### Tests of non-Latin-1 plotting in PDF and PS

## Help says pdf() and postscropt() support encoding =
## ISOLatin1 ISOLatin2 ISOLatin7 ISOLatin9
## Cyrillic Greek
## KOI8-R KOI8-U
## WinAnsi aka CP1252 CP1250 CP1251 CP1253 CP1257
## and a few more (PDFDdoc, AdobeStd, AdobeSym).

options(warn = 1L)

### only do this in a UTF-8 locale
if (!l10n_info()[["UTF-8"]]) {
    warning("encodings2.R requires a UTF-8 locale")
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
    ##print(z)

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

## Expect centring to fail for Euro and most chars in
## CP1253 KOI8-* Cyrillic CP1251 Greek CP1253
## as those chars are not in the Adobe afms.
for(i in seq_along(pdfenc)) {
    message("\ntesting ", pdfenc[i])
    ff <- paste0("pdf-", pdfenc[i], ".pdf")
    ## use compress = FALSE to make the output human readable
    ## encoding may not work (e.g. latin2 did not in musl), so use try()
    res <- try(pdf(ff, encoding = pdfenc[i], width = 9, height = 9,
                   compress = FALSE))
    if(!inherits(res, "try-error")) {
        ## and this may not work either
        try(do_one(pdfenc[i], enc[i], i >= 7))
        dev.off()
    }
}

for(i in seq_along(pdfenc)) {
    message("\ntesting postscript in ", pdfenc[i])
    ff <- paste0("PS-", pdfenc[i], ".ps")
    ## use compress = FALSE to make the output human readable
    ## encoding may not work (e.g. latin2 did not in musl), so use try()
    res <- try(postscript(ff, encoding = pdfenc[i], width = 9, height = 9))
    if(!inherits(res, "try-error")) {
        ## and this may not work either
        try(do_one(pdfenc[i], enc[i], i >= 7))
        dev.off()
    }
}

if(!capabilities("cairo")) q("no")
## Now test cairo_pdf(): do not expect any messages
cairo_pdf("cairo_pdf-encodings.pdf", width = 9, height = 9, onefile = TRUE)
for (e in enc) {
    message("testing ", e, " with cairo_pdf")
    do_one(e, e, TRUE)
}
dev.off()
