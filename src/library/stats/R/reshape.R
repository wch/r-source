reshape <-
    function(data, varying= NULL, v.names= NULL, timevar = "time", idvar = "id",
             ids = 1:NROW(data), times = seq(length = length(varying[[1]])),
             drop = NULL, direction, new.row.names = NULL,
             split = list(regexp= "\\.", include= FALSE))
{
    guess <- function(nms,re = split$regexp,drop = !split$include) {
        if (is.numeric(nms))
            nms <- names(data)[nms]
        if (drop)
            nn <- do.call("rbind",strsplit(nms,re))
        else
            nn <- cbind(substr(nms, 1, regexpr(re,nms)),
                        substr(nms, regexpr(re,nms)+1, 10000))
        v.names <- tapply(nms,nn[,1],c)
        varying <- unique(nn[,1])
        times <- sort(unique(nn[,2]))
        attr(v.names,"v.names") <- varying
        tt <- as.numeric(times)
        if (is.factor(tt)) tt <- times
        attr(v.names,"times") <- tt
        v.names
    }

    reshapeLong <-
        function(data,varying,v.names = NULL,timevar,idvar,
                 ids = 1:NROW(data), times,drop = NULL,new.row.names = NULL) {

        if (is.matrix(varying))
            varying <- tapply(varying,row(varying),list)
        ll <- unlist(lapply(varying,length))
        if (any(ll != ll[1])) stop("'varying' arguments must be the same length")
        if (ll[1] != length(times)) stop("'times' is wrong length")

        if (!is.null(drop)) {
            if (is.character(drop))
                drop <- names(data) %in% drop
            data <- data[,if (is.logical(drop)) !drop else -drop, drop = FALSE]
        }

        ## store information for back-transformation.
        undoInfo <- list(varying = varying,v.names = v.names,
                                         idvar = idvar,timevar = timevar)

        ## multiple id variables
        if (length(idvar)>1){
               repeat({
                   tempidname<-basename(tempfile("tempID"))
                   if (!(tempidname %in% names(data))) break
               })
               data[,tempidname]<-interaction(data[,idvar],drop=TRUE)
               idvar<-tempidname
               drop.idvar<-TRUE
           } else drop.idvar<-FALSE


        d <- data
        all.varying <- unlist(varying)
        d <- d[,!(names(data) %in% all.varying),drop = FALSE]
        d[,timevar] <- times[1]

        if (is.null(v.names))
            v.names <- unlist(lapply(varying,function(x) x[1]))

        for(i in 1:length(v.names))
            d[, v.names[i]] <- data[, varying[[i]][1] ]

        if (!(idvar %in% names(data)))
            d[,idvar] <- ids

        rval <- d

        if (length(times) == 1) return(rval)
        if (is.null(new.row.names))
            row.names(rval) <- paste(d[,idvar],times[1],sep = ".")
        else
            row.names(rval) <- new.row.names[1:NROW(rval)]

        for(i in 2:length(times)) {
            d[,timevar] <- times[i]
            for(j in 1:length(v.names))
                d[ ,v.names[j]] <- data[ ,varying[[j]][i]]

            if (is.null(new.row.names))
                row.names(d) <- paste(d[,idvar],times[i],sep = ".")
            else
                row.names(d) <- new.row.names[NROW(rval)+1:NROW(d)]
            rval <- rbind(rval,d)  ##inefficient. So sue me.
        }

        ## if we created a temporary id variable, drop it
        if (drop.idvar)
            rval[,idvar]<-NULL

        attr(rval,"reshapeLong") <- undoInfo
        return(rval)
    } ## re..Long()

    reshapeWide <- function(data,timevar,idvar,varying = NULL,v.names = NULL,
                            drop = NULL,new.row.names = NULL) {

        if (!is.null(drop)) {
            if (is.character(drop))
                drop <- names(data) %in% drop
            data <- data[,if (is.logical(drop)) !drop else -drop, drop = FALSE]
        }

        undoInfo <- list(v.names = v.names,  timevar = timevar,idvar = idvar)

        orig.idvar<-idvar
        if (length(idvar)>1){
            repeat({
                tempidname<-basename(tempfile("tempID"))
                if (!(tempidname %in% names(data))) break
            })
            data[,tempidname]<-interaction(data[,idvar],drop=TRUE)
            idvar<-tempidname
            drop.idvar<-TRUE
        } else drop.idvar<-FALSE

        ## times <- sort(unique(data[,timevar]))
        ## varying and times must have the same order
        times <- unique(data[,timevar])
        if (any(is.na(times)))
            warning("There are records with missing times, which will be dropped.")
        undoInfo$times<-times

        if (is.null(v.names))
            v.names <- names(data)[!(names(data) %in% c(timevar,idvar,orig.idvar))]

        if (is.null(varying))
            varying <- outer(v.names,times,paste,sep = ".")
        if (is.list(varying))
            varying <- do.call("rbind",varying)

        undoInfo$varying<-varying


        CHECK <- TRUE
        if (CHECK) {
            keep <- !(names(data) %in% c(timevar,v.names,idvar,orig.idvar))
            if(any(keep)) {
                rval <- data[keep]
                tmp <- data[,idvar]
                really.constant <-
                    unlist(lapply(rval,
                                  function(a) all(tapply(a, tmp,
                                                         function(b) length(unique(b)) == 1))))
                if (!all(really.constant))
                    warning("Some constant variables (",
                            paste(names(rval)[!really.constant],collapse = ","),
                            ") are really varying")
            }
        }


        rval <- data[!duplicated(data[,idvar]),
                     !(names(data) %in% c(timevar,v.names)), drop = FALSE]

        for(i in seq(length = length(times))) {
            thistime <- data[data[,timevar] %in% times[i],]
            rval[,varying[,i]] <- thistime[match(rval[,idvar],thistime[,idvar]),
                                           v.names]
        }

        if (!is.null(new.row.names))
            row.names(rval) <- new.row.names

        ## temporary id variable to be dropped.
        if (drop.idvar) rval[,idvar]<-NULL

        ## information for back-transformation
        attr(rval,"reshapeWide") <- undoInfo

        rval
    } ## re..Wide()

    ## Begin reshape()

    if (missing(direction)){
        undo <- c("wide","long")[c("reshapeLong","reshapeWide")%in% names(attributes(data))]
        if (length(undo)==1) direction<-undo
    }
    direction <- match.arg(direction, c("wide", "long"))
    if (!is.null(varying) && is.atomic(varying) && direction == "long")
        varying <- guess(varying)

    switch(direction,
           "wide" =
       {
           if (missing(timevar) && missing(idvar)) {
               back <- attr(data,"reshapeLong")
               if (is.null(back)) stop("No time or id specified")
               reshapeWide(data, idvar = back$idvar, timevar = back$timevar,
                           varying = back$varying, v.names = back$v.names,
                           new.row.names = new.row.names)
           } else {
               reshapeWide(data, idvar = idvar, timevar = timevar,
                           varying = varying, v.names = v.names, drop = drop,
                           new.row.names = new.row.names)
           }

       },
           "long" =
       {
           if (missing(timevar) && missing(idvar) && missing(v.names) && missing(varying)) {
               back <- attr(data,"reshapeWide")
               if (is.null(back)) stop("No time or id specified")
               reshapeLong(data, idvar = back$idvar, timevar = back$timevar,
                           varying = back$varying, v.names = back$v.names,
                           times = back$times)
           } else if (missing(v.names) && !is.null(attr(varying,"v.names"))) {
               reshapeLong(data, idvar = idvar, timevar = timevar, varying = varying,
                           v.names = attr(varying,"v.names"), drop = drop,
                           times = attr(varying,"times"), ids = ids,
                           new.row.names = new.row.names)
           } else {
               reshapeLong(data, idvar = idvar, timevar = timevar,
                           varying = varying, v.names = v.names, drop = drop,
                           times = times, ids = ids, new.row.names = new.row.names)
           }
       })
}
