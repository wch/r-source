require("tools")

## BOM stripping in .file_append_ensuring_LFs()
BOM <- as.raw(c(0xEF, 0xBB, 0xBF))

collate <- function(files, enc = NA_character_) {
    out <- tempfile(); file.create(out)
    tools:::.file_append_ensuring_LFs(out, files, enc = enc)
    readBin(out, "raw", file.info(out)$size)
}

has_bom <- function(raw) length(grepRaw(BOM, raw, fixed = TRUE)) > 0L

## File with BOM
f_bom <- tempfile(); writeBin(c(BOM, charToRaw("x <- 1\n")), f_bom)
## File without BOM
f_plain <- tempfile(); writeBin(charToRaw("y <- 2\n"), f_plain)
## BOM-only file
f_bomonly <- tempfile(); writeBin(BOM, f_bomonly)
## Short file (< 3 bytes)
f_short <- tempfile(); writeBin(charToRaw("z\n"), f_short)

stopifnot(exprs = {
    ## enc = NA preserves BOM
    has_bom(collate(f_bom))
    grepl("x <- 1", rawToChar(collate(f_bom)))

    ## enc = "UTF-8" strips BOM
    !has_bom(collate(f_bom, "UTF-8"))
    grepl("x <- 1", rawToChar(collate(f_bom, "UTF-8")))

    ## enc = "UTF-8": output is exactly 3 bytes shorter
    length(collate(f_bom, "UTF-8")) == length(collate(f_bom)) - 3L

    ## no-BOM file copied fully
    grepl("y <- 2", rawToChar(collate(f_plain, "UTF-8")))

    ## multiple files, mixed BOM
    grepl("x <- 1", rawToChar(collate(c(f_bom, f_plain), "UTF-8")))
    grepl("y <- 2", rawToChar(collate(c(f_bom, f_plain), "UTF-8")))
    !has_bom(collate(c(f_bom, f_plain), "UTF-8"))

    ## BOM-only file: stripped, only #line directive remains
    !has_bom(collate(f_bomonly, "UTF-8"))

    ## short file (< 3 bytes): fully copied
    grepl("z", rawToChar(collate(f_short, "UTF-8")))
})

unlink(c(f_bom, f_plain, f_bomonly, f_short))
