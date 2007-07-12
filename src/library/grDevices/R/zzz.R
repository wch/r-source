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

    op.utils <- c(list(locatorBell = TRUE, par.ask.default = FALSE),
                  extras, device=device)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}

### Used by text, mtext, strwidth, strheight, title, axis,
### L_text and L_textBounds, all of which
### coerce SYMSXPs and LANGSXPs to EXPRSXPs
### We don't want to use as.expression here as that is generic
### even though is.language no longer is

### Possibly later have
### if (is.language(x)) x
### else if(isS4(x)) methods::as(x, "character")
### else if(is.object(x)) as.character(x)
### else x

as.graphicsAnnot <- function(x)
    if(is.language(x) || !is.object(x)) x else as.character(x)
