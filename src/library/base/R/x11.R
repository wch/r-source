X11 <- function(display="", width=7, height=7, pointsize=12, gamma=1)
    .Internal(X11(display, width, height, pointsize, gamma))

x11 <- .Alias(X11)

Gnome <- function(display="", width=7, height=7, pointsize=12)
    .Internal(Gnome(display, width, height, pointsize))

gnome <- .Alias(Gnome)
GNOME <- .Alias(Gnome)
