## Static (i.e., not determined by configure) Make variables to be
## shared between 'Makeconf' and 'etc/Makeconf'.

R_PKGS_BASE = base tools utils graphics stats methods grid splines stats4 tcltk
R_PKGS_STUBS = ctest eda lqs mle modreg mva nls stepfun ts
# mgcv links to nlme, boot used to link to survival
R_PKGS_RECOMMENDED_SOURCES = survival boot cluster foreign KernSmooth lattice nlme mgcv rpart VR
R_PKGS_RECOMMENDED = survival boot cluster foreign KernSmooth lattice nlme mgcv rpart MASS class nnet spatial

