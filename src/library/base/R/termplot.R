termplot<-function(model,data=model.frame(model),partial.resid=FALSE,rug=FALSE,terms=NULL,se=FALSE,xlabs=NULL,ylab=NULL,...)
{
    ##coxph and glm have different default terms=
    if (is.null(terms))
        terms<-predict(model,type="terms",se=se)
    else
        terms<-predict(model,type="terms",se=se,terms=terms)
    
    mf<-model.frame(model)
    if (se)
        tms<-terms$fit
    else
        tms<-terms

    pf<-parent.frame()
    carrier<-function(term){
        if (length(term)>1)
            carrier(term[[2]])
        else
            eval(term,data,pf)
    }

    if (partial.resid)
        pres<-resid(model,"partial")

    cn<-parse(text=colnames(tms))
    factors<-sapply(colnames(tms),function(v) is.factor(mf[,v]))
    if (is.null(xlabs))
        xlabs<-colnames(tms)
    if (is.null(ylab))
        ylab<-substitute(link(foo),list(foo=formula(model)[[2]]))
    for (i in 1:NCOL(tms)){
        ylims<-range(tms[,i],na.rm=TRUE)
        if (se)
            ylims<-range(c(ylims,
                           tms[,i]+2*terms$se.fit[,i],
                           tms[,i]-2*terms$se.fit[,i]),na.rm=TRUE)
        if (partial.resid){
            ylims<-range(c(ylims,pres[,i]),na.rm=TRUE)
        }
        if (rug)
            ylims[1]<-ylims[1]-0.07*diff(ylims)

        if (factors[i]){
            ff<-mf[,colnames(tms)[i]]
            ll<-levels(ff)
            xlims<-range(seq(along=ll))
            xlims[1]<-xlims[1]-0.5
            xlims[2]<-xlims[2]+0.5
            if(rug){
                xlims[1]<-xlims[1]-0.07*diff(xlims)
                xlims[2]<-xlims[2]+0.03*diff(xlims)
                xx<-codes(ff)
            }
            plot(1,0,type="n",ylim=ylims,xlab=xlabs[i],
                 ylab=ylab,xlim=xlims,...)
            for(j in seq(along=ll)){
                ww<-which(ff==ll[j])[c(1,1)]
                lines(j+c(-0.4,0.4),tms[ww,i],...)
                if (se){
                    lines(j+c(-0.4,0.4),tms[ww,i]+terms$se.fit[ww,i]*1.96,
                          lty=2)
                    lines(j+c(-0.4,0.4),tms[ww,i]-terms$se.fit[ww,i]*1.96,
                          lty=2)
                }
            }
        } else {
            xx<-carrier(cn[[i]])
            xlims<-range(xx,na.rm=TRUE)
            if(rug)
                xlims[1]<-xlims[1]-0.07*diff(xlims)
            oo<-order(xx)
            plot(xx[oo],tms[oo,i],type="l",ylim=ylims,xlab=xlabs[i],
                 ylab=ylab,xlim=xlims,...)
            if (se){
                lines(xx[oo],tms[oo,i]+terms$se.fit[oo,i]*1.96,lty=2)
                lines(xx[oo],tms[oo,i]-terms$se.fit[oo,i]*1.96,lty=2)
            }
        }
        if (partial.resid)
            points(xx,pres[,i])
        if (rug){
            y0<-ylims[1]+c(0,0.05,NA)*diff(ylims)
            lines(rep(jitter(xx),rep(3,length(xx))),rep(y0,length(xx)))
            if (partial.resid){
                x0<-xlims[1]+c(0,0.05,NA)*diff(xlims)
                lines(rep(x0,length(xx)),rep(pres[,i],rep(3,length(xx))))
            }
        }

    }
}


