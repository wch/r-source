X11 <- function(display = "", width = 7, height = 7, pointsize = 12,
                gamma = 1, colortype = getOption("X11colortype"),
                maxcubesize = 256, canvas = "white")
    .Internal(X11(display, width, height, pointsize, gamma, colortype,
                  maxcubesize, canvas))

x11 <- X11

gnome <- function(display = "", width = 7, height = 7, pointsize = 12)
    .Internal(gnome(display, width, height, pointsize))

## no Gnome <- .Alias(gnome)
GNOME <- gnome

gtk <- function(display = "", width = 7, height = 7, pointsize = 12)
    .Internal(GTK(display, width, height, pointsize))
GTK <- gtk
