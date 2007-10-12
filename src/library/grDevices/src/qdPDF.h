#include <R.h>

Rboolean QuartzPDF_DeviceCreate(void *dd,const char *type,const char *file,double width,double height,double pointsize,const char *family,
                                   Rboolean antialias,Rboolean smooth,Rboolean autorefresh,int quartzpos,int bg, const char *title, double *dpi);

