## For examples skipped in testing because they are 'random'

set.seed(1)
if(.Platform$OS.type == "windows") options(pager = "console")

pdf("reg-examples-2.pdf", encoding = "ISOLatin1.enc")


## stats
example(SSasympOrig)
example(SSlogis)
example(constrOptim)
example(cancor)
example(aov)
# signs for promax rotation are arbitrary
example(factanal)
example(family)
example(glm)
example(glm.control)
# from extractAIC
extractAIC(glm.D93)
example(influence.measures)
example(lm)
example(ls.diag)
example(model.tables)
example(nlminb)
example(optim)
example(prcomp)
example(step)
example(summary.manova)
example(uniroot)

proc.time()
