expand.model.frame<-function(model,extras, enclos=sys.frame(sys.parent()),na.expand=FALSE){
    ## don't use model$call$formula -- it might be a variable name
    f<-formula(model)
    data<-eval(model$call$data,enclos)
    
    # new formula (there must be a better way...)
    ff<-foo~bar+baz
    if (is.call(extras))
        gg<-extras
    else
        gg<-parse(text=paste("~",paste(extras,collapse="+")))[[1]]
    ff[[2]]<-f[[2]]
    ff[[3]][[2]]<-f[[3]]
    ff[[3]][[3]]<-gg[[2]]

    if (!na.expand){
        naa<-model$call$na.action
        subset<-model$call$subset
        rval<-model.frame(ff,data=data,subset=subset,na.action=naa)
    } else {
        subset<-model$call$subset
        rval<-model.frame(ff,data=data,subset=subset,na.action=I)
        oldmf<-model.frame(model)
        keep<-match(rownames(oldmf),rownames(rval))
        rval<-rval[keep,]
    }
    
    return(rval)
}
