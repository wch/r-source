pmin <-
function (..., na.rm = FALSE)
{
        elts <- list(...)
        minmm <- as.vector(elts[[1]])
        for (each in elts[-1]) {
            work <- cbind(minmm, as.vector(each))
            nas <- is.na(work)
            work[,1][nas[,1]] <- work[,2][nas[,1]]
            work[,2][nas[,2]] <- work[,1][nas[,2]]
            change <- work[,1] > work[,2]
            work[,1][change] <- work[,2][change]
            if (!na.rm) work[,1][nas[,1]+nas[,2] > 0] <- NA
            minmm <- work[,1]
        }
        minmm
}
