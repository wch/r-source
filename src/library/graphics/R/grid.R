grid <- function (nx = NULL, ny = nx, col="lightgray", lty="dotted",
                  lwd = par("lwd"), equilogs = TRUE)
{
    if(is.null(nx) || (!is.na(nx) && nx >= 1)) {
        log <- par("xlog")
        if(is.null(nx)) { ## align to tickmarks
            ax <- par("xaxp")
            if(log && equilogs && ax[3] > 0) ax[3] <- 1
            at <- axTicks(1, axp = ax, log=log)
        } else { # equidistant, also from box borders
            U <- par("usr")
            at <- seq.int(U[1],U[2], length.out = nx+1)
            at <- (if(log) 10^at else at)[-c(1,nx+1)]
        }
        abline(v = at, col = col, lty = lty, lwd = lwd)
    }
    if(is.null(ny) || (!is.na(ny) && ny >= 1)) {
        log <- par("ylog")
        if(is.null(ny)) { ## align to tickmarks
            ax <- par("yaxp")
            if(log && equilogs && ax[3] > 0) ax[3] <- 1
            at <- axTicks(2, axp = ax, log=log)
        } else { # equidistant, also from box borders
            U <- par("usr")
            at <- seq.int(U[3],U[4], length.out = ny+1)
            at <- (if(log) 10^at else at)[-c(1,ny+1)]
        }
	abline(h = at, col = col, lty = lty, lwd = lwd)
    }
}
