#ifndef  R_FOREIGN_H
#define  R_FOREIGN_H

/* 
 These are very similar to those in  unix/dynload.c
 but we maintain them separately to give us more freedom to do
 some computations on the internal versions that are derived from
 these definitions.
*/
typedef struct {
  const char *name;
  void       *fun;
  int         numArgs;
 
} R_CMethodDef;

typedef struct {
  const char *name;
  void       *fun;
  int         numArgs;
 
} R_FortranMethodDef;

typedef struct {
  const char *name;
  void       *fun;
  int         numArgs;
 
} R_CallMethodDef;


typedef struct _DllInfo DllInfo;

int
R_registerRoutines(DllInfo *info, R_CMethodDef *croutines,
                     R_CallMethodDef *callRoutines, 
  		     R_FortranMethodDef *fortranRoutines);

DllInfo *R_getDllInfo(const char *name);

#endif /* End ifdef R_FOREIGN_H */
