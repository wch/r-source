segments <-
    function(x0, y0, x1, y1, col=par("fg"), lty=par("lty"), lwd=par("lwd"), ...)
    .Internal(segments(x0, y0, x1, y1, col=col, lty=lty, lwd=lwd, ...))
