### Tests of iconv, especially 'sub'

x <- "fa\xE7ile"
Encoding(x) <- "latin1"
x
xx <- iconv(x, "latin1", "UTF-8")
xx
stopifnot(charToRaw(xx) == as.raw(c(0x66, 0x61, 0xc3, 0xa7, 0x69, 0x6c, 0x65)))

stopifnot(is.na(iconv(x, "latin1", "ASCII")))
stopifnot(iconv(x, "latin1", "ASCII", "?")  == "fa?ile")
stopifnot(iconv(x, "latin1", "ASCII", "") == "faile")
stopifnot(iconv(x, "latin1", "ASCII", "byte") == "fa<e7>ile")
stopifnot(iconv(xx, "UTF-8", "ASCII", "Unicode") == "fa<U+00E7>ile")
stopifnot(iconv(xx, "UTF-8", "ASCII", "c99") == "fa\\u00e7ile")
stopifnot(charToRaw(iconv(xx, "UTF-8", "ASCII", "c99")) ==
          sapply(c("f", "a", "\\", "u", "0", "0", "e", "7", "i", "l", "e"), charToRaw))

z <- "\U1f600"
charToRaw(z)
stopifnot(iconv(z, "UTF-8", "ASCII", "byte") == "<f0><9f><98><80>")
stopifnot(iconv(z, "UTF-8", "ASCII", "Unicode") == "<U+0001F600>")
stopifnot(iconv(z, "UTF-8", "ASCII", "c99") == "\\U0001f600")

## write out to compare with GNU libiconv's iconv on e.g. macOS
## The reading can only work in a UTF-8 locale
if(startsWith(extSoftVersion()["iconv"], 'GNU libiconv') &&
   l10n_info()[["UTF-8"]]) {
    writeLines(c(xx, z), "test.txt")
    zz <- system2("iconv", c("-f", "UTF-8", "-t", "c99", "test.txt"),
                  stdout = TRUE)
    unlink('test.txt')
    stopifnot(zz == iconv(c(xx, z), "UTF-8", "ASCII", "c99"))
    message('sub = "c99" agrees with GNU libiconv')
} else message('sub = "c99" agrees with GNU libiconv -- SKIPPED')
