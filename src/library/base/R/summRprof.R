summaryRprof <- function(filename="Rprof.out",chunksize=5000)
{
  rprof <- file(filename)
  open(rprof,"r")
  on.exit(close(rprof))
  head  <-  scan(rprof, nlines=1, what=list("",interval=0), sep="=", quiet=TRUE)

  total <- new.env(hash=TRUE)
  self <- new.env(hash=TRUE)
  inc <- function(f,e){
      if (exists(f,envir=e,inherits=FALSE))
          assign(f,get(f,envir=e)+1,envir=e)
      else
          assign(f,1,envir=e)
  }
  count <- 0
  repeat({
      chunk <- readLines(rprof,n=chunksize)
      nread <- length(chunk)
      if (nread==0)
          break
      count <- count+nread
      thelines <- strsplit(chunk," ")
      lapply(thelines, function (a.line){
          lapply(unique(a.line), inc, e=total)
          inc(a.line[[1]],e=self)
      })
      if (nread<chunksize)
          break
  })
  if(count == 0) stop("no events were recorded")
  totalt <- sapply(ls(envir=total), function(f) get(f,envir=total))
  selft <- sapply(ls(envir=self), function(f) get(f,envir=self))


  totalpct <- round(totalt*100/count,1)
  selfpct <- round(selft*100/sum(selft),1)
  totalt <- round(totalt*head$interval/1e6,2)
  selft <- round(selft*head$interval/1e6,2)

  combine <- merge(data.frame(self.time=selft,self.pct=selfpct),
                   data.frame(total.time=totalt,total.pct=totalpct),
                   by=0, all=TRUE)
  row.names(combine) <- combine[,"Row.names"]
  combine <- combine[,-1]
  combine$self.time[is.na(combine$self.time)] <- 0
  combine$self.pct[is.na(combine$self.pct)] <- 0
  list(by.self=combine[order(-combine$self.time),],
       by.total=combine[order(-combine$total.time),c(3,4,1,2)],
       sampling.time=count*head$interval/1e6)
}

