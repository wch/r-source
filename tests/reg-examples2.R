## For examples skipped in testing because they are 'random'

## some should still be skipped when  --with-recommended-packages=no :
base.and.rec <- .packages(all.available=TRUE, lib=.Library)

set.seed(1)
if(.Platform$OS.type == "windows") options(pager = "console")

pdf("reg-examples-2.pdf", encoding = "ISOLatin1.enc")


## stats
example(SSasympOrig, run.dontcheck=TRUE)
example(SSlogis, run.dontcheck=TRUE)
example(constrOptim, run.dontcheck=TRUE)
example(cancor, run.dontcheck=TRUE)
example(aov, run.dontcheck=TRUE)
# signs for promax rotation are arbitrary
example(factanal, run.dontcheck=TRUE)
example(family, run.dontcheck=TRUE)
example(glm, run.dontcheck = any("MASS" == base.and.rec))
example(glm.control, run.dontcheck=TRUE)
# from extractAIC
extractAIC(glm.D93, run.dontcheck=TRUE)
example(influence.measures, run.dontcheck=TRUE)
example(lm, run.dontcheck=TRUE)
example(ls.diag, run.dontcheck=TRUE)
example(model.tables, run.dontcheck=TRUE)
example(nlminb, run.dontcheck=TRUE)
example(optim, run.dontcheck=TRUE)
example(prcomp, run.dontcheck=TRUE)
example(step, run.dontcheck=TRUE)
example(summary.manova, run.dontcheck=TRUE)
example(uniroot, run.dontcheck=TRUE)

proc.time()
