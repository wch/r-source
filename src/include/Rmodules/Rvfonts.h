#ifndef R_VFONTS_MODULE_H
#define R_VFONTS_MODULE_H

typedef void (*R_GE_VTextRoutine)(double x, double y, const char * const s, 
				  double x_justify, double y_justify, 
				  double rotation,
				  R_GE_gcontext *gc, GEDevDesc *dd);

typedef double (*R_GE_VStrWidthRoutine)(const char *s, 
					R_GE_gcontext *gc, GEDevDesc *dd);

typedef double (*R_GE_VStrHeightRoutine)(const char *s, 
					 R_GE_gcontext *gc, GEDevDesc *dd);

typedef struct {
    R_GE_VTextRoutine GEVText;
    R_GE_VStrWidthRoutine GEVStrWidth;
    R_GE_VStrHeightRoutine GEVStrHeight;
} VfontRoutines;

void R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth, 
			   R_GE_VStrHeightRoutine vheight, 
			   R_GE_VTextRoutine vtext);


#endif /* ifndef R_VFONTS_MODULE_H */
