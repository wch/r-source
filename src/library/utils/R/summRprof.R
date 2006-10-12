

summaryRprof<-function(filename = "Rprof.out", chunksize=5000,
                       memory=c("none","both","tseries","stats"),
                       index=2,diff=TRUE,exclude=NULL){

    filename<-file(filename, "rt")
    on.exit(close(filename))
    firstline<-readLines(filename,n=1)
    sample.interval<-as.numeric(strsplit(firstline,"=")[[1]][2])/1e6
    memory.profiling<-substr(firstline,1,6)=="memory"

    memory<-match.arg(memory)
    if(memory!="none" && !memory.profiling)
        stop("profile does not contain memory information")
    if (memory=="tseries")
        return(Rprof_memory_summary(filename=filename, chunksize=chunksize,
                             label=index,diff=diff,exclude=exclude,
                                    sample.interval=sample.interval))
    else if (memory=="stats")
        return(Rprof_memory_summary(filename=filename, chunksize=chunksize,
                             aggregate=index,diff=diff,exclude=exclude,
                                    sample.interval=sample.interval))
        
    
    fnames<-NULL
    ucounts<-NULL
    fcounts<-NULL
    memcounts<-NULL
    umem<-NULL
  
    repeat({

       chunk<-readLines(filename,n=chunksize)
       if (length(chunk)==0)
           break
       if (memory.profiling){
           memprefix<-attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:",chunk),"match.length")
           if (memory=="both"){
               memstuff<-substr(chunk,2,memprefix-1)
               memcounts<-pmax(apply(sapply(strsplit(memstuff,":"),as.numeric),1,diff),0)
               memcounts<-c(0,rowSums(memcounts[,1:3]))
               rm(memstuff)
           }
           chunk<-substr(chunk,memprefix+1,nchar(chunk))
           if(any((nc<-nchar(chunk))==0)){
                chunk<-chunk[nc>0]
                memcounts<-memcounts[nc>0]
           }
       }
       
       chunk<-strsplit(chunk," ")

       newfirsts<-sapply(chunk, "[[", 1)
       newuniques<-lapply(chunk, unique)
       ulen<-sapply(newuniques,length)
       newuniques<-unlist(newuniques)
           
       new.utable<-table(newuniques)
       new.ftable<-table(factor(newfirsts,levels=names(new.utable)))
       if (memory=="both"){
           new.umem<-rowsum(memcounts[rep.int(1:length(memcounts),ulen)],newuniques)
       }
       
       fcounts<-rowsum( c(as.vector(new.ftable),fcounts),
			c(names(new.ftable),fnames) )
       ucounts<-rowsum( c(as.vector(new.utable),ucounts),
			c(names(new.utable),fnames) )
       if(memory=="both"){
           umem<-rowsum(c(new.umem,umem),
                        c(names(new.utable),fnames))
       }

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

    if (memory=="both"){
        memtotal<- round(umem/1048576,1)     ## 0.1MB
    }
    
    index1<-order(-firstnum,-uniquenum)
    index2<-order(-uniquenum,-firstnum)

    rval<-data.frame(firstnum,firstpct,uniquenum,uniquepct)
    names(rval)<-c("self.time","self.pct","total.time","total.pct")
    rownames(rval)<-fnames
    if (memory=="both"){
        rval$mem.total<-memtotal
    }

    list(by.self=rval[index1,],
             by.total=rval[index2,c(3,4, if(memory=="both") 5,1,2)],
             sampling.time=sum(fcounts)*sample.interval)    
}

Rprof_memory_summary<-function(filename, chunksize=5000,
                   label=c(1,-1), aggregate=0, diff=FALSE,
                               exclude=NULL,sample.interval){
    
    fnames<-NULL
    memcounts<-NULL
    firsts<-NULL
    labels<-vector("list",length(label))
    index<-NULL
    
    repeat({

       chunk<-readLines(filename,n=chunksize)
       if (length(chunk)==0)
           break
       memprefix<-attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:",chunk),
                       "match.length")
       memstuff<-substr(chunk,2,memprefix-1)
       memcounts<-rbind(t(sapply(strsplit(memstuff,":"),as.numeric)))
       
       chunk<-substr(chunk,memprefix+1,nchar(chunk))
       if(any((nc<-nchar(chunk))==0)){
           memcounts<-memcounts[nc>0,]
           chunk<-chunk[nc>0]
       }

       chunk<-strsplit(chunk," ")

       if (length(exclude))
           chunk<-lapply(chunk, function(l) l[!(l %in% exclude)])
       
       newfirsts<-sapply(chunk, "[[", 1)
       firsts<-c(firsts,newfirsts)
 
       if (!aggregate && length(label)){
           for(i in 1:length(label)){

               if (label[i]==1)
                   labels[[i]]<-c(labels[[i]],newfirsts)
               else if (label[i]>1){
                   labels[[i]]<-c(labels[[i]], sapply(chunk,
                                                      function(line)
                                                      paste(rev(line)[1:min(label[i],length(line))],
                                                            collapse=":")))
               } else {
                   labels[[i]]<-c(labels[[i]], sapply(chunk,
                                                      function(line)
                                                      paste(line[1:min(-label[i],length(line))],
                                                            collapse=":")))
               }
           }
       } else if (aggregate){
           if (aggregate>0){
               index<-c(index, sapply(chunk,
                                      function(line)
                                      paste(rev(line)[1:min(aggregate,length(line))],
                                            collapse=":")))
               
           } else {
               index<-c(index, sapply(chunk,
                                      function(line)
                                      paste(line[1:min(-aggregate,length(line))],
                                            collapse=":")))
           }
       }
       

       if (length(chunk)<chunksize)
           break
    })

    if (length(memcounts)==0)
        stop("no events were recorded")

    
    
    memcounts<-as.data.frame(memcounts)
    names(memcounts)<-c("vsize.small","vsize.large","nodes","duplications")
    if (!aggregate){
        rownames(memcounts)<-(1:nrow(memcounts))*sample.interval
        names(labels)<-paste("stack",label,sep=":")
        memcounts<-cbind(memcounts,labels)
    }
    
    if (diff)
        memcounts[-1,1:3]<-pmax(0,apply(memcounts[,1:3],2,diff))

    if (aggregate)
        memcounts<-by(memcounts, index,
                      function(these) with(these,
                                           round(c(vsize.small=mean(vsize.small),
                                                   max.vsize.small=max(vsize.small),
                                                   vsize.large=mean(vsize.large),
                                                   max.vsize.large=max(vsize.large),
                                                   nodes=mean(nodes),
                                                   max.nodes=max(nodes),
                                                   duplications=mean(duplications),
                                                   tot.duplications=sum(duplications),
                                                   samples=nrow(these)
                                                   ))
                                           )
                      )
    

    return(memcounts)
    
    
    
}
