/* These are the currently supported device "models" */
typedef enum {
    MONOCHROME = 0,
    GRAYSCALE,
    PSEUDOCOLOR1,
    PSEUDOCOLOR2,
    TRUECOLOR
} X_COLORTYPE;

typedef enum {
    WINDOW, /* NB: have "type > WINDOW" below ... */
    PNG,
    JPEG,
    XIMAGE
} X_GTYPE;

Rboolean X11DeviceDriver(DevDesc*, char*, double, double, double, double, 
			 X_COLORTYPE, int);
