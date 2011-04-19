/* PAUL MURRELL
   This is from the GNU plotutils libplot-2.3 distribution
   Several modifications have been made to use the R graphics engine
   for output.
   All references to HAVE_PROTOS removed
   All references to "plotter" replaced with references to "GEDevDesc"
*/

/* This file contains the internal method _falabel_hershey(), which plots a
   label using Hershey fonts.  Each character in a Hershey font is a
   sequence of pen motions, so this function simply calls fmoverel() and
   fcontrel() to `stroke' each character in the argument string.

   The width of the string in user units is returned.  The internal method
   _flabelwidth_hershey() is similar, but does not actually plot the label.
*/

/* PAUL MURRELL
   sys-defines.h not used
*/
/* #include "sys-defines.h" */

/* PAUL MURRELL
   extern.h renamed g_extern.h
*/
#include "g_extern.h"
#include "g_control.h"
#include "g_her_metr.h"
#include <Rmodules/Rvfonts.h>

/* Shearing factor for oblique fonts, new_x = x + SHEAR * y  */

#define SHEAR (2.0/7.0)

/* Relative size of subscripts/superscripts (i.e. `indexical' size) */

#define SCRIPTSIZE (0.6)

/* Positioning of subscripts/superscripts */

#define SUBSCRIPT_DX 0.0
#define SUBSCRIPT_DY (-0.25)
#define SUPERSCRIPT_DX 0.0
#define SUPERSCRIPT_DY 0.4

/* Positioning of accents (in Hershey units).  UP_SHIFT is amount by which
   accents are raised when placed over upper-case letters.  RIGHT_SHIFT is
   applied as well, if the upper-case letter is italic. */

#define ACCENT_UP_SHIFT 7.0
#define ACCENT_RIGHT_SHIFT 2.0

/* Relative size of small Japanese Kana */
#define SMALL_KANA_SIZE 0.725

/* Hershey glyph arrays */

#define OCCIDENTAL 0
#define ORIENTAL 1

/* Location of first Kana in the occidental glyph arrays.  (Kana, unlike
   Kanji, are placed in the occidental array, at the very end.) */
#define BEGINNING_OF_KANA 4195

/* PAUL MURRELL
   Structure to contain vfont current position
*/
typedef struct {
    double currX;
    double currY;
    double angle;
} vfontContext;

/* forward references */
static bool _composite_char (unsigned char *composite,
			     unsigned char *character,
			     unsigned char *accent);
static void _draw_stroke (vfontContext *vc, const pGEcontext gc,
			  pGEDevDesc dd,
			  bool pendown, double deltax, double deltay);
static double _label_width_hershey (const pGEcontext gc, pGEDevDesc dd,
				    const unsigned short *label);
static void _draw_hershey_string (vfontContext *vc, const pGEcontext gc,
				  pGEDevDesc dd,
				  const unsigned short *string);

/* _draw_hershey_stroke() draws a stroke, taking into account the
   transformation from Hershey units to user units, and also the current
   transformation matrix (as set by the user).  _draw_stroke is similar,
   but takes arguments in user units. */
/* PAUL MURRELL
   _draw_stroke now takes arguments in INCHES;  it needs to work in
   an absolute coordinate system because it does the rotation
*/

static
void _draw_hershey_stroke (vfontContext *vc, const pGEcontext gc, pGEDevDesc dd,
			   bool pendown, double deltax, double deltay)
{
    _draw_stroke(vc, gc, dd, pendown,
		 fromDeviceWidth(HERSHEY_X_UNITS_TO_USER_UNITS (deltax),
				 GE_INCHES, dd),
		 fromDeviceHeight(HERSHEY_Y_UNITS_TO_USER_UNITS (deltay),
				  GE_INCHES, dd));
}

static void moverel(double dx, double dy, vfontContext *vc) {
    vc->currX += dx;
    vc->currY += dy;
}

static void linerel(double dx, double dy,
		    vfontContext *vc, const pGEcontext gc,
		    pGEDevDesc dd) {
    GELine(toDeviceX(vc->currX, GE_INCHES, dd),
	   toDeviceY(vc->currY, GE_INCHES, dd),
	   toDeviceX(vc->currX+dx, GE_INCHES, dd),
	   toDeviceY(vc->currY+dy, GE_INCHES, dd),
	   gc, dd);
    vc->currX += dx;
    vc->currY += dy;
}

/* e.g. for some Windows headers */
#ifndef M_PI
#define M_PI		3.141592653589793238462643383279502884197169399375
#endif

static void _draw_stroke (vfontContext *vc, const pGEcontext gc, pGEDevDesc dd,
			  bool pendown, double deltax, double deltay)
{
  double dx, dy;
  double theta;

  theta = M_PI * vc->angle / 180.0;

  dx = cos(theta) * deltax - sin(theta) * deltay;
  dy = sin(theta) * deltax + cos(theta) * deltay;

  if (pendown)
      linerel(dx, dy, vc, gc, dd);
  else
      moverel(dx, dy, vc);
}

/* PAUL MURRELL
   Renamed from _g_flabelwidth_hershey to GVStrWidth
   ... and added unit argument
   ... and rearranged order of arguments
*/

/* this is the version of the flabelwidth() method that is specific to the
   case when the current Plotter font is a Hershey font; called in
   g_flabelwidth () */
static double R_VF_VStrWidth (const char *s,
			      const pGEcontext gc, pGEDevDesc dd)
{
  double label_width;
  unsigned short *codestring;

  /* PAUL MURRELL
     _controlify using R_alloc instead of xmalloc so need to do
     vmaxget() ... vmaxset() instead of free()
  */

  const void *vmax = vmaxget();

  /* convert string to a codestring, including annotations */
  codestring = _controlify (dd, (const unsigned char *) s,
			    gc->fontfamily[3] - 1, gc->fontface);

  label_width = _label_width_hershey (gc, dd, codestring);

  vmaxset(vmax);
  /*  free (codestring); */

  return label_width;
}

/* PAUL MURRELL
   Added _label_height_hershey and GVStrHeight function
*/

static double _label_height_hershey (const pGEcontext gc,
				     pGEDevDesc dd,
				     const unsigned short *label)
{
    return( HERSHEY_Y_UNITS_TO_USER_UNITS(HERSHEY_LARGE_CAPHEIGHT) );
}

static double R_VF_VStrHeight (const char *s, const pGEcontext gc, pGEDevDesc dd)
{
  double label_height;
  unsigned short *codestring;

  const void *vmax = vmaxget();

  /* convert string to a codestring, including annotations */
  codestring = _controlify (dd, (const unsigned char *) s,
			    gc->fontfamily[3] - 1, gc->fontface);

  label_height = _label_height_hershey (gc, dd, codestring);

  vmaxset(vmax);

  return label_height;
}

/* PAUL MURRELL
   This guy is the entry point to the GNU code
   Renamed from _g_falabel_hershey to GVText
   Reordered arguments
   Added x, y, rotation, and font specification arguments
   Re-typed x/y_justify from int to double
   Re-typed s from "unsigned char *" to char *
   Changed return value from double to void
*/

/* this is the version of the falabel() method that is specific
   to the case when the current Plotter font is a Hershey font */
static void R_VF_VText (double x, double y, const char *s,
			double x_justify, double y_justify, double rotation,
			const pGEcontext gc,
			pGEDevDesc dd)
{
  unsigned short *codestring;
  double label_width, label_height;
  double x_offset, y_offset;
/*  double x_displacement; */

  /* PAUL MURRELL
     _controlify using R_alloc instead of xmalloc so need to do
     vmaxget() ... vmaxset() instead of free()
  */
  const void *vmax = vmaxget();

  /* PAUL MURRELL
     initialise the local currX and currY
     work in INCHES because that is what moverel and linerel work in
  */
  vfontContext vc;
  vc.currX = fromDeviceX(x, GE_INCHES, dd);
  vc.currY = fromDeviceY(y, GE_INCHES, dd);
  vc.angle = rotation;

  /* PAUL MURRELL
   * Override gc settings for lty and lwd
   */
  gc->lty = LTY_SOLID;
  gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
  gc->lend = GE_ROUND_CAP;
  gc->ljoin = GE_ROUND_JOIN;

  /* convert string to a codestring, including annotations */
  codestring = _controlify (dd, (const unsigned char *)s,
			    gc->fontfamily[3] - 1, gc->fontface);


  /* PAUL MURRELL
     Justification changed from char (e.g., 'l', 'c', 'r') to
     double (e.g., 0, .5, 1)
     Also added handling of NaN x_justify and y_justify
     ... and replaced HERSHEY_Y_UNITS_TO_USER_UNITS(HERSHEY_HEIGHT)
     with HERSHEY_Y_UNITS_TO_USER_UNITS(HERSHEY_LARGE_CAPHEIGHT)
     ... and replaced ((double)HERSHEY_ASCENT / (double)HERSHEY_HEIGHT)
     with 1.0
  */

  /* dimensions of the string, in user units */
  label_width = _label_width_hershey (gc, dd, codestring);
  label_height = _label_height_hershey(gc, dd, codestring);

  if (!R_FINITE(x_justify))
      x_justify = 0.5;
  if (!R_FINITE(y_justify))
      y_justify = 0.5;

  x_offset = 0 - x_justify;
  /* x_displacement = 1 - 2 * x_justify; */

  y_offset = 0 - y_justify * 1.0;

  /* save relevant drawing attributes, and restore them later */

  {
/*
    char *old_line_mode, *old_cap_mode, *old_join_mode;
    int old_fill_type;
    double oldposx, oldposy;
    bool old_dash_array_in_effect;

    old_line_mode = (char *)_plot_xmalloc (strlen (_plotter->drawstate->line_mode) + 1);
    old_cap_mode = (char *)_plot_xmalloc (strlen (_plotter->drawstate->cap_mode) + 1);
    old_join_mode = (char *)_plot_xmalloc (strlen (_plotter->drawstate->join_mode) + 1);
    oldposx = (_plotter->drawstate->pos).x;
    oldposy = (_plotter->drawstate->pos).y;

    strcpy (old_line_mode, _plotter->drawstate->line_mode);
    strcpy (old_cap_mode, _plotter->drawstate->cap_mode);
    strcpy (old_join_mode, _plotter->drawstate->join_mode);
    old_fill_type = _plotter->drawstate->fill_type;
    old_dash_array_in_effect = _plotter->drawstate->dash_array_in_effect;
*/
    /* Our choices for rendering: solid lines, rounded capitals and joins,
       a line width equal to slightly more than 1 Hershey unit, and
       transparent filling. */
/*
    _plotter->linemod (R___(_plotter) "solid");
    _plotter->capmod (R___(_plotter) "round");
    _plotter->joinmod (R___(_plotter) "round");
    _plotter->filltype (R___(_plotter) 0);
*/

    /* move to take horizontal and vertical justification into account;
       arguments here are in user units */
      /* PAUL MURRELL
	 arguments now in INCHES because _draw_stroke does rotation
	 so it needs to use absolute coordinates
      */
      _draw_stroke (&vc, gc, dd,
		    false,
		    fromDeviceWidth(x_offset * label_width, GE_INCHES, dd),
		    fromDeviceHeight(y_offset * label_height, GE_INCHES, dd));
    /* call stroker on the sequence of strokes obtained from each char (the
       stroker may manipulate the line width) */
      /* _draw_hershey_stroke (dd, true, 0, HERSHEY_EM);     */
      _draw_hershey_string (&vc, gc, dd, codestring);

    /* Restore original values of relevant drawing attributes, free
       storage.  endpath() will be invoked in here automatically, flushing
       the created polyline object comprising the stroked text. */
/*
    _plotter->linemod (R___(_plotter) old_line_mode);
    _plotter->capmod (R___(_plotter) old_cap_mode);
    _plotter->joinmod (R___(_plotter) old_join_mode);
    _plotter->filltype (R___(_plotter) old_fill_type);
    _plotter->drawstate->dash_array_in_effect = old_dash_array_in_effect;

    free (old_line_mode);
    free (old_cap_mode);
    free (old_join_mode);
*/
    /* return to original position */
      /*    _plotter->fmove (R___(_plotter) oldposx, oldposy); */
  }

  /* amount by which to shift after printing label (user units) */
/*
  postdx = x_displacement * label_width;
  theta = M_PI * rotation / 180.0;
  dx = cos (theta) * postdx
    - sin (theta) * 0;
  dy = sin (theta) * postdx
    + cos (theta) * 0;

  moverel(dx, dy);
*/

  vmaxset(vmax);
  /*  free (codestring); */

  /* PAUL MURRELL
     No return value
  */
  /* return label_width;		 user units */
}

/* In addition to scaling the character sizes and the `width', we perform
   the following (dx, dy):

   enter subscript	(dx, dy) = (-1/9, -1/2) * width
   exit subscript	(dx, dy) = (+1/6, +1/2) * width

   enter superscript	(dx, dy) = (-1/9, +1/2) * width
   exit superscript	(dx, dy) = (+1/6, -1/2) * width

   For clarity here, `width' refers to the width _before_ it is
   multiplied by a factor 2/3.

   [N.B. In Bob Beach's original UGS character stroke generator,
   the +1/6's here were +2/9 instead.  Better?] */

/* _label_width_hershey() computes the width (total delta x) of a
   controlified character string to be rendered in a vector font, in user
   units */
static double _label_width_hershey (const pGEcontext gc, pGEDevDesc dd,
				    const unsigned short *label)
{
  const unsigned short *ptr = label;
  unsigned short c;
  double charsize = 1.0;	/* relative char size, 1.0 means full size */
  double saved_charsize = 1.0;
  double width = 0.0;		/* label width */
  double saved_width = 0.0;

  /* loop through unsigned shorts in label */
  while ((c = (*ptr)) != (unsigned short)'\0')
    {
      int glyphnum;		/* glyph in Hershey array */
      const unsigned char *glyph;

      if (c & RAW_HERSHEY_GLYPH)
	/* glyph was spec'd via an escape, not as a char in a font */
	{
	  glyphnum = c & GLYPH_SPEC;
	  glyph = (const unsigned char *)(_occidental_hershey_glyphs[glyphnum]);

	  if (*glyph != '\0')	/* nonempty glyph */
	    /* 1st two chars are bounds */
	    width += charsize * ((int)glyph[1] - (int)glyph[0]);
	}
      else if (c & RAW_ORIENTAL_HERSHEY_GLYPH)
	/* glyph was spec'd via an escape, not as a char in a font */
	{
	  glyphnum = c & GLYPH_SPEC;
	  glyph = (const unsigned char *)_oriental_hershey_glyphs[glyphnum];

	  if (*glyph != '\0')	/* nonempty glyph */
	    /* 1st two chars are bounds */
	    width += charsize * ((int)glyph[1] - (int)glyph[0]);
	}
      else if (c & CONTROL_CODE)	/* parse control code */
	{
	  switch (c & ~CONTROL_CODE)
	    {
	    case C_BEGIN_SUBSCRIPT:
	    case C_BEGIN_SUPERSCRIPT :
	      charsize *= SCRIPTSIZE;
	      break;

	    case C_END_SUBSCRIPT:
	    case C_END_SUPERSCRIPT:
	      charsize /= SCRIPTSIZE;
	      break;

	    case C_PUSH_LOCATION:
	      saved_width = width;
	      saved_charsize = charsize;
	      break;

	    case C_POP_LOCATION:
	      width = saved_width;
	      charsize = saved_charsize;
	      break;

	    case C_RIGHT_ONE_EM:
	      width += charsize * HERSHEY_EM;
	      break;

	    case C_RIGHT_HALF_EM:
	      width += charsize * HERSHEY_EM / 2.0;
	      break;

	    case C_RIGHT_QUARTER_EM:
	      width += charsize * HERSHEY_EM / 4.0;
	      break;

	    case C_RIGHT_SIXTH_EM:
	      width += charsize * HERSHEY_EM / 6.0;
	      break;

	    case C_RIGHT_EIGHTH_EM:
	      width += charsize * HERSHEY_EM / 8.0;
	      break;

	    case C_RIGHT_TWELFTH_EM:
	      width += charsize * HERSHEY_EM / 12.0;
	      break;

	    case C_LEFT_ONE_EM:
	      width -= charsize * HERSHEY_EM;
	      break;

	    case C_LEFT_HALF_EM:
	      width -= charsize * HERSHEY_EM / 2.0;
	      break;

	    case C_LEFT_QUARTER_EM:
	      width -= charsize * HERSHEY_EM / 4.0;
	      break;

	    case C_LEFT_SIXTH_EM:
	      width -= charsize * HERSHEY_EM / 6.0;
	      break;

	    case C_LEFT_EIGHTH_EM:
	      width -= charsize * HERSHEY_EM / 8.0;
	      break;

	    case C_LEFT_TWELFTH_EM:
	      width -= charsize * HERSHEY_EM / 12.0;
	      break;

	      /* unrecognized control code */
	    default:
	      break;
	    }
	}
      else			/* yow, an actual character */
	{
	  int raw_fontnum;

	  /* compute index of font, in table in g_fontdb.c */
	  raw_fontnum = (c >> FONT_SHIFT) & ONE_BYTE;

	  c &= ~FONT_SPEC;	/* extract character proper */
	  glyphnum = (_hershey_font_info[raw_fontnum].chars)[c];

	  /* could be a pseudo glyph number, e.g. an indication that
	     character is composite */
	  if (glyphnum == ACC0 || glyphnum == ACC1 || glyphnum == ACC2)
	    {
		unsigned char composite, character = '\0' /* -Wall */, accent;

	      /* if so, use 1st element of composite character */
	      composite = (unsigned char)c;
	      if (_composite_char (&composite, &character, &accent))
		glyphnum = (_hershey_font_info[raw_fontnum].chars)[character];
	      else
		glyphnum = UNDE; /* hope this won't happen */
	    }

	  /* could also be a glyph number displaced by KS, to indicate
	     that this is a small kana */
	  if (glyphnum & KS)
	    glyphnum -= KS;

	  glyph = (const unsigned char *)(_occidental_hershey_glyphs[glyphnum]);
	  if (*glyph != '\0')	/* nonempty glyph */
	    /* 1st two chars are bounds */
	    width += charsize * ((int)glyph[1] - (int)glyph[0]);
	}

      ptr++;			/* bump pointer in string */
    }

  return HERSHEY_X_UNITS_TO_USER_UNITS (width);
}

/* _draw_hershey_penup_stroke() draws a penup stroke, along a vector
   specified in Hershey units.  Size scaling and obliquing (true/false) are
   specified.  This is used for repositioning during rendering of
   composite (accented) characters. */
static
void _draw_hershey_penup_stroke(vfontContext *vc, const pGEcontext gc,
				pGEDevDesc dd,
				double dx, double dy,
				double charsize, bool oblique)
{
  double shear;

  shear = oblique ? (SHEAR) : 0.0;
  _draw_hershey_stroke (vc, gc, dd,
			false,	/* pen up */
			charsize * (dx + shear * dy),
			charsize * dy);
}

/* _draw_hershey_glyph() invokes move() and cont() to draw a raw Hershey
   glyph, specified by index in the occidental or oriental glyph arrays.
   Size scaling and obliquing (true/false) are specified. */
static
void _draw_hershey_glyph (vfontContext *vc, const pGEcontext gc, pGEDevDesc dd,
			  int glyphnum,
			  double charsize, int type, bool oblique)
{
  double xcurr, ycurr;
  double xfinal, yfinal;
  bool pendown = false;
  const unsigned char *glyph;
  double dx, dy;
  double shear;

  shear = oblique ? (SHEAR) : 0.0;
  switch (type)
    {
    case OCCIDENTAL:
    default:
      glyph = (const unsigned char *)(_occidental_hershey_glyphs[glyphnum]);
      break;
    case ORIENTAL:
      glyph = (const unsigned char *)(_oriental_hershey_glyphs[glyphnum]);
      break;
    }

  if (*glyph != '\0')	/* nonempty glyph */
    {
      xcurr = charsize * (double)glyph[0];
      xfinal = charsize * (double)glyph[1];
      ycurr = yfinal = 0.0;
      glyph += 2;
      while (*glyph)
	{
	  int xnewint;

	  xnewint = (int)glyph[0];

	  if (xnewint == (int)' ')
	    pendown = false;
	  else
	    {
	      double xnew, ynew;

	      xnew = (double)charsize * xnewint;
	      ynew = (double)charsize
		* ((int)'R'
		   - ((int)glyph[1] + (double)HERSHEY_BASELINE));
	      dx = xnew - xcurr;
	      dy = ynew - ycurr;
	      _draw_hershey_stroke (vc, gc, dd,
				    pendown, dx + shear * dy, dy);
	      xcurr = xnew, ycurr = ynew;
	      pendown = true;
	    }

	  glyph +=2;	/* on to next pair */
	}

      /* final penup stroke, to end where we should */
      dx = xfinal - xcurr;
      dy = yfinal - ycurr;
      _draw_hershey_stroke (vc, gc, dd, false, dx + shear * dy, dy);
    }
}

/* _draw_hershey_string() strokes a string beginning at present location,
   which is taken to be on the string's baseline.  Besides invoking move()
   and cont(), it invokes linewidth(). */
static
void _draw_hershey_string (vfontContext *vc, const pGEcontext gc, pGEDevDesc dd,
			   const unsigned short *string)
{
  unsigned short c;
  const unsigned short *ptr = string;
  double charsize = 1.0;
  int line_width_type = 0;	/* 0,1,2 = unset,occidental,oriental */

  while ((c = (*ptr++)) != '\0')
    {
      /* Check for the four possibilities: (1) a Hershey glyph specified by
	 glyph number, (2) an oriental Hershey glyph specified by glyph
	 number, (3) a control code, and (4) an ordinary font character,
	 which will be mapped to a Hershey glyph by one of the tables in
	 g_fontdb.c. */

      if (c & RAW_HERSHEY_GLYPH)
	{
	  if (line_width_type != 1)
	    {
	      gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
	      line_width_type = 1;
	    }
	  _draw_hershey_glyph (vc, gc, dd,
			       c & GLYPH_SPEC, charsize, OCCIDENTAL, false);
	}

      else if (c & RAW_ORIENTAL_HERSHEY_GLYPH)
	{
	  if (line_width_type != 2)
	    {
	      gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
	      line_width_type = 2;
	    }
	  _draw_hershey_glyph (vc, gc, dd,
			       c & GLYPH_SPEC, charsize, ORIENTAL, false);
	}

      else if (c & CONTROL_CODE)
	switch (c & ~CONTROL_CODE) /* parse control codes */
	  {
	  case C_BEGIN_SUPERSCRIPT :
	    _draw_hershey_stroke (vc, gc, dd,
				  false,
				  SUPERSCRIPT_DX * charsize * HERSHEY_EM,
				  SUPERSCRIPT_DY * charsize * HERSHEY_EM);
	    charsize *= SCRIPTSIZE;
	    break;

	  case C_END_SUPERSCRIPT:
	    charsize /= SCRIPTSIZE;
	    _draw_hershey_stroke (vc, gc, dd,
				  false,
				  - SUPERSCRIPT_DX * charsize * HERSHEY_EM,
				  - SUPERSCRIPT_DY * charsize * HERSHEY_EM);
	    break;

	  case C_BEGIN_SUBSCRIPT:
	    _draw_hershey_stroke (vc, gc, dd,
				  false,
				  SUBSCRIPT_DX * charsize * HERSHEY_EM,
				  SUBSCRIPT_DY * charsize * HERSHEY_EM);
	    charsize *= SCRIPTSIZE;
	    break;

	  case C_END_SUBSCRIPT:
	    charsize /= SCRIPTSIZE;
	    _draw_hershey_stroke (vc, gc, dd,
				  false,
				  - SUBSCRIPT_DX * charsize * HERSHEY_EM,
				  - SUBSCRIPT_DY * charsize * HERSHEY_EM);
	    break;

	  case C_PUSH_LOCATION:
	      /* saved_charsize = charsize;
		 saved_position_x = _plotter->drawstate->pos.x;
		 saved_position_y = _plotter->drawstate->pos.y; */
	    break;

	  case C_POP_LOCATION:
	      /* charsize = saved_charsize;
		 _plotter->fmove (R___(_plotter)
		 saved_position_x, saved_position_y); */
	    break;

	  case C_RIGHT_ONE_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, charsize * HERSHEY_EM, 0.0);
	    break;

	  case C_RIGHT_HALF_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, charsize * HERSHEY_EM / 2.0, 0.0);
	    break;

	  case C_RIGHT_QUARTER_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, charsize * HERSHEY_EM / 4.0, 0.0);
	    break;

	  case C_RIGHT_SIXTH_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, charsize * HERSHEY_EM / 6.0, 0.0);
	    break;

	  case C_RIGHT_EIGHTH_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, charsize * HERSHEY_EM / 8.0, 0.0);
	    break;

	  case C_LEFT_ONE_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, - charsize * HERSHEY_EM, 0.0);
	    break;

	  case C_LEFT_HALF_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, - charsize * HERSHEY_EM / 2.0, 0.0);
	    break;

	  case C_LEFT_QUARTER_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, - charsize * HERSHEY_EM / 4.0, 0.0);
	    break;

	  case C_LEFT_SIXTH_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, - charsize * HERSHEY_EM / 6.0, 0.0);
	    break;

	  case C_LEFT_EIGHTH_EM:
	    _draw_hershey_stroke (vc, gc, dd,
				  false, - charsize * HERSHEY_EM / 8.0, 0.0);
	    break;

	    /* unrecognized control code, punt */
	  default:
	    break;
	  }

      else
	/* yow, an actual font character!  Several possibilities: could be
	   a composite (accented) character, could be a small Kana, or
	   could be a garden-variety character. */
	{
	  int raw_fontnum;
	  int glyphnum;		/* glyph in Hershey array */
	  int char_glyphnum, accent_glyphnum; /* for composite chars */
	  int char_width, accent_width; /* for composite chars */
	  const unsigned char *char_glyph, *accent_glyph;
	  unsigned char composite, character = '\0', accent = '\0' /* -Wall */;
	  bool oblique, small_kana = false;

	  /* compute index of font, in font table in g_fontdb.c */
	  raw_fontnum = (c >> FONT_SHIFT) & ONE_BYTE;
	  /* shear font?  (for HersheySans-Oblique, etc.) */
	  oblique = _hershey_font_info[raw_fontnum].obliquing;

	  c &= ~FONT_SPEC;	/* extract character proper */
	  glyphnum = (_hershey_font_info[raw_fontnum].chars)[c];

	  if (glyphnum & KS) /* a small kana? */
	    {
	      glyphnum -= KS;
	      small_kana = true;
	    }

	  switch (glyphnum)
	    {
	      /* special case: this is a composite (accented) character;
		 search font table in g_fontdb.c for it */
	    case ACC0:
	    case ACC1:
	    case ACC2:
	      composite = (unsigned char)c;
	      if (_composite_char (&composite, &character, &accent))
		{
		  char_glyphnum =
		    (_hershey_font_info[raw_fontnum].chars)[character];
		  accent_glyphnum =
		    (_hershey_font_info[raw_fontnum].chars)[accent];
		}
	      else
		{		/* hope this won't happen */
		  char_glyphnum = UNDE;
		  accent_glyphnum = 0;
		}
	      char_glyph =
		(const unsigned char *)_occidental_hershey_glyphs[char_glyphnum];
	      accent_glyph =
		(const unsigned char *)_occidental_hershey_glyphs[accent_glyphnum];

	      if (*char_glyph != '\0') /* nonempty glyph */
		/* 1st two chars are bounds, in Hershey units */
		char_width = (int)char_glyph[1] - (int)char_glyph[0];
	      else
		char_width = 0;

	      if (*accent_glyph != '\0') /* nonempty glyph */
		/* 1st two chars are bounds, in Hershey units */
		accent_width = (int)accent_glyph[1] - (int)accent_glyph[0];
	      else
		accent_width = 0;

	      /* draw the character */
	      if (line_width_type != 1)
	      {
		  gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
		  line_width_type = 1;
		}
	      _draw_hershey_glyph (vc, gc, dd,
				   char_glyphnum, charsize,
				   OCCIDENTAL, oblique);
	      /* back up to draw accent */
	      _draw_hershey_penup_stroke (vc, gc, dd,
					  -0.5 * (double)char_width
					  -0.5 * (double)accent_width,
					  0.0, charsize, oblique);

	      /* repositioning for uppercase and uppercase italic */
	      if (glyphnum == ACC1)
		_draw_hershey_penup_stroke (vc, gc, dd,
					    0.0,
					    (double)(ACCENT_UP_SHIFT),
					    charsize, oblique);
	      else if (glyphnum == ACC2)
		_draw_hershey_penup_stroke (vc, gc, dd,
					    (double)(ACCENT_RIGHT_SHIFT),
					    (double)(ACCENT_UP_SHIFT),
					    charsize, oblique);

	      /* draw the accent */
	      _draw_hershey_glyph (vc, gc, dd,
				   accent_glyphnum, charsize,
				   OCCIDENTAL, oblique);

	      /* undo special repositioning if any */
	      if (glyphnum == ACC1)
		_draw_hershey_penup_stroke (vc, gc, dd,
					    0.0,
					    -(double)(ACCENT_UP_SHIFT),
					    charsize, oblique);
	      else if (glyphnum == ACC2)
		_draw_hershey_penup_stroke (vc, gc, dd,
					    -(double)(ACCENT_RIGHT_SHIFT),
					    -(double)(ACCENT_UP_SHIFT),
					    charsize, oblique);

	      /* move forward, to end composite char where we should */
	      _draw_hershey_penup_stroke (vc, gc, dd,
					  0.5 * (double)char_width
					  -0.5 * (double)accent_width,
					  0.0, charsize, oblique);
	      break;

	      /* not a composite (accented) character; just an ordinary
		 glyph from occidental+Kana array (could be a Kana, in
		 particular, could be a small Kana) */
	    default:
	      if (small_kana)
		{
		  int kana_width;
		  const unsigned char *kana_glyph;
		  double shift = 0.5 * (1.0 - (SMALL_KANA_SIZE));

		  kana_glyph =
		    (const unsigned char *)_occidental_hershey_glyphs[glyphnum];
		  kana_width = (int)kana_glyph[1] - (int)kana_glyph[0];

		  /* draw small Kana, preceded and followed by a penup
		     stroke in order to traverse the full width of an
		     ordinary Kana */
		  _draw_hershey_penup_stroke (vc, gc, dd,
					      shift * (double)kana_width,
					      0.0, charsize, oblique);
		  if (line_width_type != 2)
		    {
			gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
		      line_width_type = 2;
		    }
		  _draw_hershey_glyph (vc, gc, dd,
				       glyphnum,
				       (SMALL_KANA_SIZE) * charsize,
				       OCCIDENTAL, oblique);
		  _draw_hershey_penup_stroke (vc, gc, dd,
					      shift * (double)kana_width,
					      0.0, charsize, oblique);
		}
	      else
		/* whew! just an ordinary glyph from the occidental array
		   (could be a Kana however, since they're confusingly
		   placed in that array, at the end) */
		{
		  if (glyphnum >= BEGINNING_OF_KANA)
		    {
		      if (line_width_type != 2)
			{
			gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_ORIENTAL_STROKE_WIDTH);
			  line_width_type = 2;
			}
		    }
		  else
		      if (line_width_type != 1)
			{
			gc->lwd = HERSHEY_LINE_WIDTH_TO_LWD (HERSHEY_STROKE_WIDTH);
			  line_width_type = 1;
			}
		_draw_hershey_glyph (vc, gc, dd,
				     glyphnum, charsize,
				     OCCIDENTAL, oblique);
		}
	      break;
	    } /* end of case statement that switches based on glyphnum */

	} /* end of font character case */

    } /* end of loop through unsigned shorts in the codestring */

  return;
}

/* retrieve the two elements of a composite character from the table in
   g_fontdb.c */
static bool _composite_char (unsigned char *composite,
			     unsigned char *character,
			     unsigned char *accent)
{
  const struct plHersheyAccentedCharInfoStruct *compchar = _hershey_accented_char_info;
  bool found = false;
  unsigned char given = *composite;

  while (compchar->composite)
    {
      if (compchar->composite == given)
	{
	  found = true;
	  /* return char and accent via pointers */
	  *character = compchar->character;
	  *accent = compchar->accent;
	}
      compchar++;
    }

  return found;
}

#include <R_ext/Rdynload.h>
void R_init_vfonts(DllInfo *dll)
{
    R_GE_setVFontRoutines(R_VF_VStrWidth, R_VF_VStrHeight, R_VF_VText);
}
