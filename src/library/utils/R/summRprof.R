

summaryRprof<-function(filename = "Rprof.out", chunksize=5000){

    filename<-file(filename, "rt")
    on.exit(close(filename))
    sample.interval<-as.numeric(strsplit(readLines(filename,n=1),"=")[[1]][2])/1e6

    fnames<-NULL
    ucounts<-NULL
    fcounts<-NULL

    repeat({

       chunk<-readLines(filename,n=chunksize)
       if (length(chunk)==0)
           break

       chunk<-strsplit(chunk," ")

       newfirsts<-sapply(chunk, "[[", 1)
       newuniques<-unlist(sapply(chunk, unique))

       new.utable<-table(newuniques)
       new.ftable<-table(factor(newfirsts,levels=names(new.utable)))

       fcounts<-rowsum( c(as.vector(new.ftable),fcounts),
			c(names(new.ftable),fnames) )
       ucounts<-rowsum( c(as.vector(new.utable),ucounts),
			c(names(new.utable),fnames) )

       fnames<-sort(unique(c(fnames,names(new.utable))))

       if (length(chunk)<chunksize)
           break
    })

    if (sum(fcounts)==0)
        stop("no events were recorded")

    digits<-ifelse(sample.interval<0.01, 3,2)
    firstnum<-round(fcounts*sample.interval,digits)
    uniquenum<-round(ucounts*sample.interval,digits)

    firstpct<-round(100*firstnum/sum(firstnum),1)
    uniquepct<-round(100*uniquenum/sum(firstnum),1)

    index1<-order(-firstnum,-uniquenum)
    index2<-order(-uniquenum,-firstnum)

    rval<-data.frame(firstnum,firstpct,uniquenum,uniquepct)
    names(rval)<-c("self.time","self.pct","total.time","total.pct")
    rownames(rval)<-fnames

    list(by.self=rval[index1,],
         by.total=rval[index2,c(3,4,1,2)],
         sampling.time=sum(fcounts)*sample.interval)


}

