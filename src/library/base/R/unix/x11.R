X11 <- function(display="", width=7, height=7, pointsize=12,
                gamma=1, colortype = getOption("X11colortype"),
                maxcubesize = 256)
    .Internal(X11(display, width, height, pointsize, gamma, colortype, maxcubesize))

x11 <- .Alias(X11)

# Should also rename internal name from "Gnome" to "gnome" ??
gnome <- function(display="", width=7, height=7, pointsize=12)
    .Internal(Gnome(display, width, height, pointsize))

## no Gnome <- .Alias(gnome)
GNOME <- .Alias(gnome)
