pictex <-
    function(file = "Rplots.tex", width = 5, height = 4, debug = FALSE,
	     bg = "white", fg = "black")
{
    .External("PicTeX", file, bg, fg, width, height, as.logical(debug),
              PACKAGE = "base")
    par(mar = c(5,4,2,4)+0.1)
}
