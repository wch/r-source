/*
 * Source code from Xfig 3.2.4 modified to work with arrays of doubles
 * instead linked lists of F_points and to remove some globals(!)
 * See copyright etc below.
 * 
 * #included from engine.c
 */

/*
 * From w_drawprim.h
 */

#define         MAXNUMPTS       25000

/*
 * From u_draw.c
 */ 

/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985-1988 by Supoj Sutanthavibul
 * Parts Copyright (c) 1989-2002 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 * Parts Copyright (c) 1992 by James Tough
 * Parts Copyright (c) 1998 by Georg Stemmer
 * Parts Copyright (c) 1995 by C. Blanc and C. Schlick
 *
 * Any party obtaining a copy of these files is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish and/or distribute copies of
 * the Software, and to permit persons who receive copies from any such 
 * party to do so, with the only requirement being that this copyright 
 * notice remain intact.
 *
 */

/************** POLYGON/CURVE DRAWING FACILITIES ****************/

static int npoints;
static int max_points;
static double *xpoints;
static double *ypoints;

/************* Code begins here *************/

static Rboolean
add_point(double x, double y, GEDevDesc *dd)
{
    if (npoints >= max_points) {
	int tmp_n;
	double *tmp_px;
	double *tmp_py;
	tmp_n = max_points + 200;
	/* too many points, return false */
	if (tmp_n > MAXNUMPTS) {
	    error(_("add_point - reached MAXNUMPTS (%d)"),tmp_n);
	}
	if (max_points == 0) {
	    tmp_px = (double *) R_alloc(tmp_n, sizeof(double));
	    tmp_py = (double *) R_alloc(tmp_n, sizeof(double));
	} else {
	    tmp_px = (double *) S_realloc((char *) xpoints, 
					  tmp_n, max_points, 
					  sizeof(double));
	    tmp_py = (double *) S_realloc((char *) ypoints, 
					  tmp_n, max_points, 
					  sizeof(double));
	}
	if (tmp_px == NULL || tmp_py == NULL) {
	    error(_("insufficient memory to allocate point array"));
	}
	xpoints = tmp_px;
	ypoints = tmp_py;
	max_points = tmp_n;
    }
    /* ignore identical points */
    if (npoints > 0 && xpoints[npoints-1] == x && ypoints[npoints-1] == y)
	return TRUE;
    /*
     * Convert back from 1200ppi to DEVICE coordinates
     */
    xpoints[npoints] = toDeviceX(x / 1200, GE_INCHES, dd);
    ypoints[npoints] = toDeviceY(y / 1200, GE_INCHES, dd);
    npoints = npoints + 1;
    return TRUE;
}

/*
 * From u_draw_spline.c
 */

/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985-1988 by Supoj Sutanthavibul
 * Parts Copyright (c) 1989-2002 by Brian V. Smith
 * Parts Copyright (c) 1991 by Paul King
 *
 * Any party obtaining a copy of these files is granted, free of charge, a
 * full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 * nonexclusive right and license to deal in this software and
 * documentation files (the "Software"), including without limitation the
 * rights to use, copy, modify, merge, publish and/or distribute copies of
 * the Software, and to permit persons who receive copies from any such 
 * party to do so, with the only requirement being that this copyright 
 * notice remain intact.
 *
 */

/* THIS FILE IS #included FROM u_draw.c and u_geom.c */

/********************* CURVES FOR SPLINES *****************************

 The following spline drawing routines are from

    "X-splines : A Spline Model Designed for the End User"

    by Carole BLANC and Christophe SCHLICK,
    in Proceedings of SIGGRAPH ' 95

***********************************************************************/

#define         HIGH_PRECISION    0.5
#define         LOW_PRECISION     1.0
#define         ZOOM_PRECISION    5.0
#define         ARROW_START       4
#define         MAX_SPLINE_STEP   0.2

/***********************************************************************/

#define Q(s)  (-(s))
#define EQN_NUMERATOR(dim) \
  (A_blend[0]*dim[0]+A_blend[1]*dim[1]+A_blend[2]*dim[2]+A_blend[3]*dim[3])

static double
f_blend(double numerator, double denominator)
{
  double p = 2 * denominator * denominator;
  double u = numerator / denominator;
  double u2 = u * u;

  return (u * u2 * (10 - p + (2*p - 15)*u + (6 - p)*u2));
}

static double 
g_blend(double u, double q)             /* p equals 2 */
{
  return(u*(q + u*(2*q + u*(8 - 12*q + u*(14*q - 11 + u*(4 - 5*q))))));
}

static double
h_blend(double u, double q)
{
    double u2=u*u;
    return (u * (q + u * (2 * q + u2 * (-2*q - u*q))));
}

static void
negative_s1_influence(double t, double s1, double *A0, double *A2)
{
  *A0 = h_blend(-t, Q(s1));
  *A2 = g_blend(t, Q(s1));
}

static void
negative_s2_influence(double t, double s2, double *A1, double *A3)
{
  *A1 = g_blend(1-t, Q(s2));
  *A3 = h_blend(t-1, Q(s2));
}

static void
positive_s1_influence(double k, double t, double s1, double *A0, double *A2)
{
  double Tk;
  
  Tk = k+1+s1;
  *A0 = (t+k+1<Tk) ? f_blend(t+k+1-Tk, k-Tk) : 0.0;
  
  Tk = k+1-s1;
  *A2 = f_blend(t+k+1-Tk, k+2-Tk);
}

static void
positive_s2_influence(double k, double t, double s2, double *A1, double *A3)
{
  double Tk;

  Tk = k+2+s2; 
  *A1 = f_blend(t+k+1-Tk, k+1-Tk);
  
  Tk = k+2-s2;
  *A3 = (t+k+1>Tk) ? f_blend(t+k+1-Tk, k+3-Tk) : 0.0;
}

static void
point_adding(double *A_blend, double *px, double *py, 
	     GEDevDesc *dd) 
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];
  add_point(EQN_NUMERATOR(px) / (weights_sum),
	    EQN_NUMERATOR(py) / (weights_sum),
	    dd);
}

static void
point_computing(double *A_blend, 
		double *px, double *py,
		int *x, int *y)
{
  double weights_sum;

  weights_sum = A_blend[0] + A_blend[1] + A_blend[2] + A_blend[3];

  *x = EQN_NUMERATOR(px) / (weights_sum);
  *y = EQN_NUMERATOR(py) / (weights_sum);
}

static float
step_computing(int k,
	       double *px, double *py,
	       double s1, double s2, 
	       float precision)
{
  double A_blend[4];
  int    xstart, ystart, xend, yend, xmid, ymid, xlength, ylength;
  int    start_to_end_dist, number_of_steps;
  float  step, angle_cos, scal_prod, xv1, xv2, yv1, yv2, sides_length_prod;
  
  /* This function computes the step used to draw the segment (p1, p2)
     (xv1, yv1) : coordinates of the vector from middle to origin
     (xv2, yv2) : coordinates of the vector from middle to extremity */

  if ((s1 == 0) && (s2 == 0))
    return(1.0);              /* only one step in case of linear segment */

  /* compute coordinates of the origin */
  if (s1>0) {
      if (s2<0) {
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  negative_s2_influence(0.0, s2, &A_blend[1], &A_blend[3]); 
      } else {
	  positive_s1_influence(k, 0.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.0, s2, &A_blend[1], &A_blend[3]); 
      }
      point_computing(A_blend, px, py, &xstart, &ystart);
  } else {
      xstart = px[1];
      ystart = py[1];
  }
  
  /* compute coordinates  of the extremity */
  if (s2>0) {
      if (s1<0) {
	  negative_s1_influence(1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]);
      } else {
	  positive_s1_influence(k, 1.0, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 1.0, s2, &A_blend[1], &A_blend[3]); 
      }
      point_computing(A_blend, px, py, &xend, &yend);
  } else {
      xend = px[2];
      yend = py[2];
  }

  /* compute coordinates  of the middle */
  if (s2>0) {
      if (s1<0) {
	  negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]);
      } else {
	  positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, 0.5, s2, &A_blend[1], &A_blend[3]); 
	}
  } else if (s1<0) {
      negative_s1_influence(0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
  } else {
      positive_s1_influence(k, 0.5, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(0.5, s2, &A_blend[1], &A_blend[3]);
  }
  point_computing(A_blend, px, py, &xmid, &ymid);

  xv1 = xstart - xmid;
  yv1 = ystart - ymid;
  xv2 = xend - xmid;
  yv2 = yend - ymid;

  scal_prod = xv1*xv2 + yv1*yv2;
  
  sides_length_prod = sqrt((xv1*xv1 + yv1*yv1)*(xv2*xv2 + yv2*yv2));

  /* compute cosinus of origin-middle-extremity angle, which approximates the
     curve of the spline segment */
  if (sides_length_prod == 0.0)
    angle_cos = 0.0;
  else
    angle_cos = scal_prod/sides_length_prod; 

  xlength = xend - xstart;
  ylength = yend - ystart;

  start_to_end_dist = sqrt(xlength*xlength + ylength*ylength);

  /* more steps if segment's origin and extremity are remote */
  number_of_steps = sqrt(start_to_end_dist)/2;

  /* more steps if the curve is high */
  number_of_steps += (int)((1 + angle_cos)*10);

  if (number_of_steps == 0)
    step = 1;
  else
    step = precision/number_of_steps;
  
  if ((step > MAX_SPLINE_STEP) || (step == 0))
    step = MAX_SPLINE_STEP;
  return (step);
}

static void
spline_segment_computing(float step, int k, 
			 double *px, double *py,
			 double s1, double s2, 
			 GEDevDesc *dd)
{
  double A_blend[4];
  double t;
  
  if (s1<0) {  
     if (s2<0) {
	 for (t=0.0 ; t<1 ; t+=step) {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, px, py, dd);
	 }
     } else {
	 for (t=0.0 ; t<1 ; t+=step) {
	     negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, px, py, dd);
	 }
     }
  } else if (s2<0) {
      for (t=0.0 ; t<1 ; t+=step) {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, px, py, dd);
      }
  } else {
      for (t=0.0 ; t<1 ; t+=step) {
	     positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
	     positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);

	     point_adding(A_blend, px, py, dd);
      } 
  }
}

/*
 * For adding last line segment when computing open spline
 * WITHOUT end control points repeated 
 * (i.e., can't just connect to last control point)
 */ 
static void
spline_last_segment_computing(float step, int k, 
			      double *px, double *py,
			      double s1, double s2, 
			      GEDevDesc *dd)
{
  double A_blend[4];
  double t = 1;
  
  if (s1<0) {  
      if (s2<0) {
	  negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	  negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);
	  
	  point_adding(A_blend, px, py, dd);
      } else {
	  negative_s1_influence(t, s1, &A_blend[0], &A_blend[2]);
	  positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);
	  
	  point_adding(A_blend, px, py, dd);
      }
  } else if (s2<0) {
      positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
      negative_s2_influence(t, s2, &A_blend[1], &A_blend[3]);
       
      point_adding(A_blend, px, py, dd);
  } else {
      positive_s1_influence(k, t, s1, &A_blend[0], &A_blend[2]);
      positive_s2_influence(k, t, s2, &A_blend[1], &A_blend[3]);
      
      point_adding(A_blend, px, py, dd);
  } 
}

/********************* MAIN METHODS *************************************/

/*
 * x and y are in DEVICE coordinates
 * xfig works in 1200ppi 
 *   (http://www.csit.fsu.edu/~burkardt/data/fig/fig_format.html)
 * so convert to 1200ppi so that step calculations are correct
 */
#define COPY_CONTROL_POINT(PI, I, N) \
      px[PI] = fromDeviceX(x[(I) % N], GE_INCHES, dd) * 1200; \
      py[PI] = fromDeviceY(y[(I) % N], GE_INCHES, dd) * 1200; \
      ps[PI] = s[(I) % N]

#define NEXT_CONTROL_POINTS(K, N) \
      COPY_CONTROL_POINT(0, K, N); \
      COPY_CONTROL_POINT(1, K + 1, N); \
      COPY_CONTROL_POINT(2, K + 2, N); \
      COPY_CONTROL_POINT(3, K + 3, N)

#define INIT_CONTROL_POINTS(N) \
      COPY_CONTROL_POINT(0, N - 1, N); \
      COPY_CONTROL_POINT(1, 0, N); \
      COPY_CONTROL_POINT(2, 1, N); \
      COPY_CONTROL_POINT(3, 2, N)

#define SPLINE_SEGMENT_LOOP(K, PX, PY, S1, S2, PREC) \
      step = step_computing(K, PX, PY, S1, S2, PREC);    \
      spline_segment_computing(step, K, PX, PY, S1, S2, dd)

static Rboolean
compute_open_spline(int n, double *x, double *y, double *s,
		    Rboolean repEnds,
		    float precision, 
		    GEDevDesc *dd)
{
  int       k;
  float     step = 0.0 /* -Wall */;
  double px[4];
  double py[4];
  double ps[4]={0.,0.,0.,0.};

  max_points = 0;
  npoints = 0;
  xpoints = NULL;
  ypoints = NULL;

  if (repEnds && n < 2)
      error(_("There must be at least two control points"));
  if (!repEnds && n < 4)
      error(_("There must be at least four control points"));

  if (repEnds) {
      /* first control point is needed twice for the first segment */
      COPY_CONTROL_POINT(0, 0, n);
      COPY_CONTROL_POINT(1, 0, n);
      COPY_CONTROL_POINT(2, 1, n);

      if (n == 2) {
	COPY_CONTROL_POINT(3, 1, n);
      } else {
	COPY_CONTROL_POINT(3, 2, n);
      }

      for (k = 0 ; ; k++) {
	  SPLINE_SEGMENT_LOOP(k, px, py, ps[1], ps[2], precision);
	  if (k + 3 == n)
	      break;
	  NEXT_CONTROL_POINTS(k, n);
      }

      /* last control point is needed twice for the last segment */
      COPY_CONTROL_POINT(0, n - 3, n);
      COPY_CONTROL_POINT(1, n - 2, n);
      COPY_CONTROL_POINT(2, n - 1, n);
      COPY_CONTROL_POINT(3, n - 1, n);
      SPLINE_SEGMENT_LOOP(k, px, py, ps[1], ps[2], precision);

      add_point(px[3], py[3], dd);
  } else {
      for (k = 0 ; k + 3 < n ; k++) {
	  NEXT_CONTROL_POINTS(k, n);
	  SPLINE_SEGMENT_LOOP(k, px, py, ps[1], ps[2], precision);
      }    
      spline_last_segment_computing(step, n - 4, px, py, ps[1], ps[2], dd);
  }
  
  return TRUE;
}

static Rboolean
compute_closed_spline(int n, double *x, double *y, double *s,
		      float precision, 
		      GEDevDesc *dd)
{
  int k;
  float     step;
  double px[4];
  double py[4];
  double ps[4];
  
  max_points = 0;
  npoints = 0;
  xpoints = NULL;
  ypoints = NULL;

  if (n < 3)
      error(_("There must be at least three control points"));

  INIT_CONTROL_POINTS(n);

  for (k = 0 ; k < n ; k++) {
      SPLINE_SEGMENT_LOOP(k, px, py, ps[1], ps[2], precision);
      NEXT_CONTROL_POINTS(k, n);
  }

  return TRUE;
}




