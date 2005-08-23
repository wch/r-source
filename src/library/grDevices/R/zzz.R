.noGenerics <- TRUE

.onLoad <- function(libname, pkgname)
{
    op <- options()
    extras <- if(.Platform$OS.type == "windows")
        list(graphics.record = FALSE,
             windowsBuffered = TRUE,
             windowsTimeouts = as.integer(c(100,500)))
    else
        ## these must be set for x11 to be used, even non-interactively
        list(X11colortype = "true",
             X11fonts = c("-adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*",
             "-adobe-symbol-medium-r-*-*-%d-*-*-*-*-*-*-*"))
    device <- if(interactive()) {
        if(.Platform$OS.type == "windows") "windows"
        else if (.Platform$GUI == "AQUA") "quartz"
        else if (!is.null(Sys.info) && (Sys.info()["sysname"] == "Darwin")
                 && (Sys.getenv("DISPLAY") != "")) "X11"
        else if (Sys.getenv("DISPLAY") != "")
            switch(.Platform$GUI, "Tk" = "X11",
                    "X11" = "X11", "GNOME" = "X11", "postscript")
	else "postscript"
    } else "postscript"

    op.utils <- c(list(locatorBell = TRUE), extras, device=device)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}
