text <- function(x, ...) UseMethod("text")

text.default <-
function(x, y = NULL, labels = seq(along = x),
         adj = NULL, pos = NULL, offset = 0.5,
         vfont = NULL, cex = 1, col = NULL, font = NULL, xpd = NULL, ...) {
    if (!missing(y) && (is.character(y) || is.expression(y))) {
	labels <- y; y <- NULL
    }
    if (!is.null(vfont))
        vfont <- c(typeface = pmatch(vfont[1], Hershey$typeface) - 1,
                   fontindex= pmatch(vfont[2], Hershey$fontindex)- 1)
    .Internal(text(xy.coords(x,y, recycle=TRUE),
		   labels, adj, pos, offset, vfont,
		   cex, col, font, xpd, ...))
}

Hershey <-
    list(typeface =
         c("serif", "sans serif", "script",
           "gothic english", "gothic german", "gothic italian",
           "serif symbol", "sans serif symbol"),
         fontindex =
         c("symbol", "plain", "italic", "bold", "bold italic",
           "cyrillic", "oblique cyrillic", "EUC")
         )

