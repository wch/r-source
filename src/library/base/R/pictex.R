pictex <-
function(file="Rplots.tex", width=5, height=4, debug = FALSE,
        bg="white", fg="black")
{
        .Internal(device("pictex", as.character(c(file, bg, fg)),
                c(width, height, debug)))
        par(mar=c(5,4,2,4)+0.1)
}
