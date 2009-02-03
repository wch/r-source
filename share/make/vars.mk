## Static (i.e., not determined by configure) Make variables to be
## shared between '../../Makeconf' and '../../etc/Makeconf'
## *and* grepped from '../../m4/R.m4'

R_PKGS_BASE = base tools utils grDevices graphics stats datasets methods grid splines stats4 tcltk
R_PKGS_STUBS = ctest eda lqs mle modreg mva nls stepfun ts

# FIXME: cannot use common make variable,
# R_PKGS_RECOMMENDED_common = boot cluster codetools foreign KernSmooth lattice Matrix nlme mgcv rpart survival
# R_PKGS_RECOMMENDED_SOURCES = VR $(R_PKGS_RECOMMENDED_common)
# R_PKGS_RECOMMENDED = $(R_PKGS_RECOMMENDED_common) MASS class nnet spatial
#  as ../../m4/R.m4  greps the *_SOURCES line
R_PKGS_RECOMMENDED_SOURCES = VR boot cluster codetools foreign KernSmooth lattice nlme mgcv rpart survival Matrix
R_PKGS_RECOMMENDED =            boot cluster codetools foreign KernSmooth lattice nlme mgcv rpart survival MASS class nnet spatial Matrix
# mgcv requires nlme and MASS
