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

pdfenc <- c("ISOLatin1", "ISOLatin2", "ISOLatin7", "ISOLatin9",
           "Greek", "Cyrillic", "KOI8-R", "KOI8-U",
           "WinAnsi", "CP1250", "CP1251", "CP1253", "CP1257")

enc <- c("latin1", "latin2", "iso-8859-13", "latin-9",
         "iso-8859-7", "iso-8859-5", "KOI8-R", "KOI8-U",
         "CP1252", "CP1250", "CP1251", "CP1253", "CP1257")

## only use valid printable characters from the encodings and exclude those
## that have a diffent UTF-8 mapping on Windows (iso-8859-7, KOI8-R)
##
## invalid characters are sometimes converted to unexpected UTF-8 mappings on
## Windows (rather than NA)
valid_printable <- list(
    "latin1"      = c(32:126,160:255),
    "latin2"      = c(32:126,160:255),
    "iso-8859-13" = c(32:126,160:255),
    "latin-9"     = c(32:126,160:255),
    "iso-8859-7"  = c(32:126,160,163,166:169,171:173,175:209,211:254),
      # 161, 162 - Windows uses normal quotes, but Linux/macOS uses fancy
      # 164, 165, 170 - 2003 additions not supported by Windows
    "iso-8859-5"  = c(32:126,160:255),
    "KOI8-R"      = c(32:126,160:255),
    "KOI8-U"      = c(32:126,128:173,175:189,191:255),
      # 174, 190 - Windows uses characters from KOI8-RU (U+45e, U+40e),
      #            which differs from Unix (R+255d, U+256c)
    "CP1252"      = c(32:126,128,130:140,142,145:156,158:255),
    "CP1250"      = c(32:126,128,130,132:135,137:143,145:151,153:255),
    "CP1251"      = c(32:126,128:151,153:255),
    "CP1253"      = c(32:126,128,130:135,137,139,145:151,153,155,160:169,171:209,211:254),
    "CP1257"      = c(32:126,128,130,132:135,137,139,141:143,145:151,153,155,157:158,160,162:164,166:255)
)

do_one <-function(name, encoding)
{
    par(pty="s")
    plot(c(-1,16), c(-1,16), type="n", xlab="", ylab="", xaxs="i", yaxs="i")
    title(paste("Centred chars in", name))
    grid(17, 17, lty=1)
    known <- valid_printable[[encoding]]
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


## Expect centring to fail for most chars in
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
        try(do_one(pdfenc[i], enc[i]))
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
        try(do_one(pdfenc[i], enc[i]))
        dev.off()
    }
}

if(!capabilities("cairo")) q("no")
## Now test cairo_pdf(): do not expect any messages
cairo_pdf("cairo_pdf-encodings.pdf", width = 9, height = 9, onefile = TRUE)
for (e in enc) {
    message("testing ", e, " with cairo_pdf")
    do_one(e, e)
}
dev.off()
