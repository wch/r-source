/**************************
 * A first hack at a possible new device API
 **************************
 */

/* New device driver structure
 * NOTES:
 * 1. All locations and dimensions are in device coordinates.
 * 2. I found this comment in the doc for dev_Open -- looks nasty
 *    Any known instances of such a thing happening?  Should be
 *    replaced by a function to query the device for preferred gpars
 *    settings? (to be called when the device is initialised)
         *
         * NOTE that it is perfectly acceptable for this	
         * function to set generic graphics parameters too	
         * (i.e., override the generic parameter settings	
         * which GInit sets up) all at the author's own risk	
         * of course :)						
	 *
 * 3. Do we really need dev_StrWidth as well as dev_MetricInfo?
 *    I can see the difference between the two -- its just a 
 *    question of whether dev_MetricInfo should just return
 *    what dev_StrWidth would give if font metric information is
 *    not available.  I guess having both allows the developer
 *    to decide when to ask for which sort of value, and to decide
 *    what to do when font metric information is not available.
 *    And why not a dev_StrHeight?
 * 4. Replaced dev_Resize with dev_Size
 * 5. Do I make "cex" and "ps" part of the graphics engine or
 *    part of R base graphics?  If not part of the graphics engine,
 *    then how should the graphics engine specify text size?
 *    For now, they are part of the graphics engine.
 *    Similar comments apply for "font".
 * 6. Should "ipr", "asp", and "cra" be in the device description?
 *    If not, then where?
 *    I guess they don't need to be if no device makes use of them.
 *    On the other hand, they would need to be replaced by a device
 *    call that R base graphics could use to get enough information
 *    to figure them out.  (e.g., some sort of dpi() function to
 *    complement the size() function.)
 * 7. Is dev_Dot used anywhere?
 */
typedef struct {
    /* The first element is a boolean indicating whether this is 
     * a new device driver (always 1) -- the old device driver structure
     * has had a similar element added (which will always be 0)
     */
    int newDevStruct; 
    /********************************************************
     * Device state variables.
     ********************************************************/
    double left;	        /* left raster coordinate */
    double right;	        /* right raster coordinate */
    double bottom;	        /* bottom raster coordinate */
    double top;		        /* top raster coordinate */
    double clipLeft;
    double clipRight;
    double clipBottom;
    double clipTop;
    double gamma;	        /* Device Gamma Correction */
    double xCharOffset;	        /* x character addressing offset */
    double yCharOffset;	        /* y character addressing offset */
    double yLineBias;	        /* 1/2 interline space as frac of line hght */
    Rboolean canResizePlot;	/* can the graphics surface be resized */
    Rboolean canChangeFont;	/* device has multiple fonts */
    Rboolean canRotateText;	/* text can be rotated */
    Rboolean canResizeText;	/* text can be resized */
    Rboolean canClip;		/* Hardware clipping */
    Rboolean canChangeGamma;    /* can the gamma factor be modified */
    int canHAdj;	        /* Can do at least some horiz adjust of text
			           0 = none, 1 = {0,0.5, 1}, 2 = [0,1] */
    double ipr[2];	        /* Inches per raster; [0]=x, [1]=y */
    double asp;		        /* Pixel aspect ratio = ipr[1]/ipr[0] */
    double cra[2];	        /* Character size in rasters; [0]=x, [1]=y */
    double startps;             
    int startcol;
    int startfill;
    int startlty;
    int startfont;
    double startgamma;
    void *deviceSpecific;	/* pointer to device specific parameters */
    Rboolean displayListOn;     /* toggle for display list status */
    SEXP displayList;           /* display list */
    Rboolean ask;	        /* User confirmation of ``page eject'' */
    /********************************************************
     * Device procedures.
     ********************************************************/
    /*
     * device_Activate is called when a device becomes the	
     * active device.  For example, it can be used to change the
     * title of a window to indicate the active status of	
     * the device to the user.  Not all device types will	
     * do anything.			
     * The only parameter is a device driver structure.
     * An example is ...
     *
     * static void   X11_Activate(NewDevDesc *dd);
     *
     */
    void (*activate)();
    /*
     * device_Circle should have the side-effect that a	
     * circle is drawn, centred at the given location, with 
     * the given radius.  The border of the circle should be
     * drawn in the given "col", and the circle should be	
     * filled with the given "fill" colour.		
     * If "col" is NA_INTEGER then no border should be drawn
     * If "fill" is NA_INTEGER then the circle should not 
     * be filled.
     * An example is ...
     *
     * static void X11_Circle(double x, double y, double r,
     *		              int col, int fill, double gamma,
     *                        int lty, double lwd,
     *                        NewDevDesc *dd);
     *
     */
    void (*circle)();
    /*
     * device_Clip is given the left, right, bottom, and	
     * top of a rectangle (in DEVICE coordinates).	
     * It should have the side-effect that subsequent output	
     * is clipped to the given rectangle.
     * NOTE that R's graphics engine already clips to the 
     * extent of the device.
     * NOTE also that this will probably only be called if
     * the flag canClip is true.
     * An example is ...
     *
     * static void X11_Clip(double x0, double x1, double y0, double y1, 
     *                      NewDevDesc *dd)
     */
    void (*clip)();
    /*
     * device_Close is called when the device is killed.
     * This function is responsible for destroying any	
     * device-specific resources that were created in	
     * device_Open and for FREEing the device-specific	
     * parameters structure.	
     * An example is ...
     *
     * static void X11_Close(NewDevDesc *dd)
     *
     */
    void (*close)();
    /*
     * device_Deactivate is called when a device becomes	
     * inactive.  
     * This allows the device to undo anything it did in 
     * dev_Activate.
     * Not all device types will do anything.
     * An example is ...
     *
     * static void X11_Deactivate(NewDevDesc *dd)
     *
     */
    void (*deactivate)();
    void (*dot)();
    /*
     * I don't know what this is for and i can't find it	
     * being used anywhere, but i'm loath to kill it in	
     * case i'm missing something important			
     * An example is ...
     *
     * static void X11_Hold(NewDevDesc *dd)
     *
     */
    void (*hold)();
    /*
     * device_Locator should return the location of the next
     * mouse click (in DEVICE coordinates)	
     * Not all devices will do anything (e.g., postscript)	
     * An example is ...
     *
     * static Rboolean X11_Locator(double *x, double *y, NewDevDesc *dd)
     *
     */
    Rboolean (*locator)();
    /*
     * device_Line should have the side-effect that a single
     * line is drawn (from x1,y1 to x2,y2)			
     * An example is ...
     *
     * static void X11_Line(double x1, double y1, double x2, double y2,
     *		            int col, double gamma, int lty, double lwd,
     *                      NewDevDesc *dd);
     *
     */
    void (*line)();
    /*
     * device_MetricInfo should return height, depth, and	
     * width information for the given character in DEVICE	
     * units.
     * This is used for formatting mathematical expressions	
     * and for exact centering of text (see GText)		
     * If the device cannot provide metric information then 
     * it MUST return 0.0 for ascent, descent, and width.
     * An example is ...
     *
     * static void X11_MetricInfo(int c, int font, double cex, double ps,
     *                            double* ascent, double* descent,
     *                            double* width, NewDevDesc *dd);
     */
    void (*metricInfo)();
    /*
     * device_Mode is called whenever the graphics engine	
     * starts drawing (mode=1) or stops drawing (mode=0)	
     * The device is not required to do anything		
     * An example is ...
     *
     * static void X11_Mode(int mode, NewDevDesc *dd);
     *
     */
    void (*mode)();
    /*
     * device_NewPage is called whenever a new plot requires
     * a new page.	
     * A new page might mean just clearing the	
     * device (e.g., X11) or moving to a new page	
     * (e.g., postscript)					
     * An example is ...
     *
     *
     * static void X11_NewPage(int fill, double gamma, NewDevDesc *dd);
     *
     */
    void (*newPage)();
    /*
     * device_Open is not usually called directly by the	
     * graphics engine;  it is usually only called from	
     * the device-driver entry point.			
     * This function should set up all of the device-	
     * specific resources for a new device			
     * This function is given a newly-allocated NewDevDesc structure 
     * and it must FREE the structure if anything goes seriously wrong.
     * NOTE that different devices will accept different parameter
     * lists, corresponding to different device-specific preferences.
     * An example is ...
     *
     * Rboolean X11_Open(NewDevDesc *dd, char *dsp, double w, double h, 
     *                   double gamma_fac, X_COLORTYPE colormodel, 
     *                   int maxcube, int canvascolor);
     *
     */
    Rboolean (*open)();
    /*
     * device_Polygon should have the side-effect that a	
     * polygon is drawn using the given x and y values	
     * the polygon border should be drawn in the "col"	
     * colour and filled with the "fill" colour.
     * If "col" is NA_INTEGER don't draw the border		
     * If "fill" is NA_INTEGER don't fill the polygon		
     * An example is ...
     *
     * static void X11_Polygon(int n, double *x, double *y, 
     *		               int col, int fill, double gamma,
     *                         int lty, double lwd,
     *                         NewDevDesc *dd);
     *
     */
    void (*polygon)();
    /*
     * device_Polyline should have the side-effect that a	
     * series of line segments are drawn using the given x	
     * and y values.
     * An example is ...
     *
     * static void X11_Polyline(int n, double *x, double *y, 
     *		               int col, double gamma,
     *                         int lty, double lwd,
     *                          NewDevDesc *dd);
     *
     */
    void (*polyline)();
    /*
     * device_Rect should have the side-effect that a	
     * rectangle is drawn with the given locations for its	
     * opposite corners.  The border of the rectangle	
     * should be in the given "col" colour and the rectangle	
     * should be filled with the given "fill" colour.
     * If "col" is NA_INTEGER then no border should be drawn 
     * If "fill" is NA_INTEGER then the rectangle should not	
     * be filled.						
     * An example is ...
     *
     * static void X11_Rect(double x0, double y0, double x1, double y1,
     *		            int col, int fill, double gamma,
     *                      int lty, double lwd,
     *                      NewDevDesc *dd);
     *
     */
    void (*rect)();
    /*
     * device_Size is called whenever the device is	
     * resized.  
     * The function returns (left, right, bottom, and top) for the	
     * new device size.					
     * This is not usually called directly by the graphics	
     * engine because the detection of device resizes	
     * (e.g., a window resize) are usually detected by	
     * device-specific code.
     * An example is ...
     *
     * static void X11_Size(double *left, double *right,
     *                      double *bottom, double *top,
     *                      NewDevDesc *dd);
     *
     */
    void (*size)();
    /*
     * device_StrWidth should return the width of the given 
     * string in DEVICE units.				
     * An example is ...
     *
     * static double X11_StrWidth(char *str, int font, double cex, double ps,
     *                            NewDevDesc *dd)
     *
     */
    double (*strWidth)();
    /*
     * device_Text should have the side-effect that the	
     * given text is drawn at the given location.
     * The text should be rotated according to rot (degrees)
     * An example is ...
     *
     * static void X11_Text(double x, double y, char *str, 
     *                      double rot, double hadj, 
     *		            int col, double gamma,
     *                      int font, double cex, double ps,
     * 	                    NewDevDesc *dd);
     *
     */
    void (*text)();
} NewDevDesc;

	/********************************************************/
	/* the device-driver entry point is given a device	*/
	/* description structure that it must set up.  this	*/
	/* involves several important jobs ...			*/
	/* (1) it must ALLOCATE a new device-specific parameters*/
	/* structure and FREE that structure if anything goes	*/
	/* wrong (i.e., it won't report a successful setup to	*/
	/* the graphics engine (the graphics engine is NOT	*/
	/* responsible for allocating or freeing device-specific*/
	/* resources or parameters)				*/
	/* (2) it must initialise the device-specific resources */
	/* and parameters (mostly done by calling device_Open)	*/
	/* (3) it must initialise the generic graphical		*/
	/* parameters that are not initialised by GInit (because*/
	/* only the device knows what values they should have)	*/
	/* see Graphics.h for the official list of these	*/
	/* (4) it may reset generic graphics parameters that	*/
	/* have already been initialised by GInit (although you	*/
	/* should know what you are doing if you do this)	*/
	/* (5) it must attach the device-specific parameters	*/
	/* structure to the device description structure	*/
	/* e.g., dd->deviceSpecfic = (void *) xd;		*/
	/* (6) it must FREE the overall device description if	*/
	/* it wants to bail out to the top-level		*/
	/* the graphics engine is responsible for allocating	*/
	/* the device description and freeing it in most cases	*/
	/* but if the device driver freaks out it needs to do	*/
	/* the clean-up itself					*/
	/********************************************************/

