### Tests of non-ASCII plotting in PDF and PS


## Silence substitution warnings to make this more diff-able.
Sys.setenv("_R_SILENT_PDF_SUBSTITUTION_" =  "true")

options(warn = 1L)

### only do this in a UTF-8 locale
if (!l10n_info()[["UTF-8"]]) {
    warning("encodings.R requires a UTF-8 locale")
    q("no")
}
musl <- grepl("musl", R.version$os)

Sys.unsetenv("_R_CHECK_MBCS_CONVERSION_FAILURE_")


## characters remapped in mbcsToSbcs
## (unless libiconv remaps first which in most cases macOS >= 14 does)

## One-char fixups
one <- c(0x2013, 0x2014, 0x2212, 0x2018, 0x2019, 0x201C, 0x201D,
         0x2022, 0x2605, 0x2737)
one <- intToUtf8(one, TRUE)
## macOS transliterates the first 8, 8th to "o", quotes to grave/acute accent.
cat(one, sep ="  "); cat("\n")

## Two-ckar fixups
two <- c(0x2190, 0x2192, 0x2794, 0x279C, 0x279D, 0x279E, 0x279F, 0x27a1,
         0x27a2, 0xFB00, 0xFB01, 0xFB02, 0x0152, 0x0153, 0x2264, 0x2265)
two <- intToUtf8(two, TRUE)
cat(two, sep ="  "); cat("\n")
two_1 <- two[1:9]    # macOS transliterates the first 2 only
two_2 <- two[-(1:9)] # macOS transliterates these.

## Three-char fixups
three <- c(0xFB03, 0xFB04)
three <- intToUtf8(three, TRUE) # macOS transliterates these
cat(three, sep ="  "); cat("\n")

## Four-char fixups
four <- "\u2030" # permille.  macOS and R map to o/oo.
cat(four, sep ="  "); cat("\n")

## compress = FALSE makes this humna-readable and diff-able.
## hard-code encoding ISOLatin1.enc to match reference output
## (e.g. Windows default WinAnsi.enc, so CP-1252, has a special character
##  for bullet, fancy double quotes, per mille and other Unicode characters
##  used in the tests, while ISOLatin1.enc does not)
pdf("PDF-encoding.pdf", width = 5, height = 5, compress = FALSE,
    encoding="ISOLatin1.enc")
plot(1:11, 0:10, type = "n")
text(0.5+seq_along(one), 1, one, adj = c(0,0))
text(0.5+seq_along(two_1), 3, two_1, adj = c(0,0))
text(0.5+seq_along(two_2), 2, two_2, adj = c(0,0))
text(0.5+seq_along(three), 4, three, adj = c(0,0))
text(0.5+seq_along(four), 5, four, adj = c(0,0))

## Things in packages which cannot be plotted on pdf(encoding = "latin1")
five <- c("\u03b1", "\u03bc", "\u2211", "\u25cf", "\u25b2", "\u221e", "\uB3C4")
cat(five, sep ="  "); cat("\n")
six <- "\U0001f604" # emoji from WhatsR
cat(six, sep ="  "); cat("\n")
# cyrillic from mapmisc
seven <- "\u0423\u043b\u0430\u0430\u043d\u0431\u0430\u0430\u0442\u0430\u0440"
cat(seven, sep ="  "); cat("\n")
## Latin-2 example from package 'ggenealogy'
## Transliterations of \u010d (c caron) differ
eight <- "Lubom\u00edr  Kub\u00e1\u010dek"
cat(eight, sep ="  "); cat("\n")

text(0.5+seq_along(five), 6, five, adj = c(0,0))
text(1.5, 7, six, adj = c(0,0))
text(1.5, 8, seven, adj = c(0,0))
text(1.5, 9, eight, adj = c(0,0))

## Now try centring
plot(1:11, 0:10, type = "n")
text(0.5+seq_along(one), 1, one)
text(0.5+seq_along(two_1), 3, two_1)
text(0.5+seq_along(two_2), 2, two_2)
text(0.5+seq_along(three), 4, three)
text(0.5+seq_along(four), 5, four)
text(0.5+seq_along(five), 6, five)
text(1.5, 7, six)
text(5, 8, seven)
text(5, 9, eight)
dev.off()

## avoid case clash with pdf-Cyrillic.pdf in encodings2.R
pdf("PDF-cyr.pdf", width = 5, height = 5,
    encoding = "KOI8-R", compress = FALSE)
plot(1:10, 1:10, type = "n")
text(1.5, 8, seven, adj = c(0,0))
dev.off()

##if(!musl) { ## musl's iconv does not support "latin2", only "iso88592"
    ## works in default Helvetica but not in NimbusSan
    pdf("PDF-latin2.pdf", width = 5, height = 5,
        encoding = "ISOLatin2", compress = FALSE)
    plot(1:10, 1:10, type = "n")
    text(1.5, 9, eight, adj = c(0,0))
    dev.off()
##}


## Also try postscript
postscript("PS-encoding.ps", width = 5, height = 5, encoding="ISOLatin1.enc")
plot(1:11, 0:10, type = "n")
text(0.5+seq_along(one), 1, one, adj = c(0,0))
text(0.5+seq_along(two_1), 3, two_1, adj = c(0,0))
text(0.5+seq_along(two_2), 2, two_2, adj = c(0,0))
text(0.5+seq_along(three), 4, three, adj = c(0,0))
text(0.5+seq_along(four), 5, four, adj = c(0,0))
text(0.5+seq_along(five), 6, five, adj = c(0,0))
text(1.5, 7, six, adj = c(0,0))
text(1.5, 8, seven, adj = c(0,0))
text(1.5, 9, eight, adj = c(0,0)) ## macOS does transliteration here, badly
dev.off()

## avoid clashes with encodings tests in encodings2.R
postscript("PS-cyr.ps", width = 5, height = 5, encoding = "KOI8-R")
plot(1:10, 1:10, type = "n")
text(1.5, 7, seven, adj = c(0,0))
dev.off()

##if(!musl) {
    postscript("PS-latin2.ps", width = 5, height = 5, encoding = "ISOLatin2")
    plot(1:10, 1:10, type = "n")
    text(1.5, 9, eight, adj = c(0,0))
    dev.off()
#}

## Now with --as-cran settings in recent R, only.
if (getRversion() < "4.4.0") q("no")
Sys.setenv("_R_CHECK_MBCS_CONVERSION_FAILURE_" = "TRUE")
## These are only failures if the OS does not transliterate.
## so all work on Alpine Linux, giving '*'
if(musl) q("no")

tf <- tempfile(fileext = ".pdf")
## note that using the default on Windows (WinAnsi.enc) enables some
## transliterations that would cause output differences in these tests
## (alpha, micro and infinity from "five" will get transliterated by
## Windows)
pdf(tf, width = 5, height = 5, encoding="ISOLatin1.enc")
plot(1:11, 0:10, type = "n")
## tranliterations done in mbcsToSbcs (when they will warn) if not
## done by the OS (which will be silent).
text(0.5+seq_along(one), 1, one, adj = c(0,0))
text(0.5+seq_along(two_1), 3, two_1, adj = c(0,0))
text(0.5+seq_along(two_2), 2, two_2, adj = c(0,0))
text(0.5+seq_along(three), 4, three, adj = c(0,0))
text(0.5+seq_along(four), 5, four, adj = c(0,0))

## conversions not done in mbcsToSbcs
res5 <- try(text(0.5+seq_along(five), 6, five, adj = c(0,0)))
if(!inherits(res5, "try-error")) message("error check failed on 'five'")
res6 <- try(text(1.5, 7, six, adj = c(0,0)))
if(!inherits(res6, "try-error")) message("error check failed on 'six'")
res7 <- try(text(1.5, 8, seven, adj = c(0,0)))
if(!inherits(res7, "try-error")) message("error check failed on 'seven'")
## 'eight' may or may not fail -- does on glibc, does not on macOS.
res8 <- try(text(1.5, 9, eight, adj = c(0,0)), silent=TRUE)
## disable this check as the results are platform dependent
## if(!inherits(res8, "try-error")) message("error check failed on 'eight'")
dev.off()
unlink(tf)

