warnings<-function(...)
{
        n<-length(last.warning)
        names<-names(last.warning)
        if( n == 1 )
                cat("Warning message:\n")
        else
                cat("Warning messages:\n")
        for(i in 1:n) {
                if( n == 1 )
                        out<-names[i]
                else
                        out<-paste(i,": ",names[i],sep="")
                if(length(last.warning[[i]])) {
                        temp<-deparse(last.warning[[i]])
                        if(length(temp)>1)
                                out<-paste(out, "in:", temp[1]," ...")
                        else
                                out<-paste(out, "in:", temp[1])
                }
                cat(out, ..., fill = T)
        }
}
