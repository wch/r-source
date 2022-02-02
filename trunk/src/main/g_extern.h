/* PAUL MURRELL
   This is from the GNU plotutils libplot-2.3 distribution
   Lots has been taken out, to leave just the hershey font stuff
   Renamed g_extern.h from extern.h so that I can easily recognise
   it as part of the stuff I took from GNU plotutils to put into R
*/

/* This is the chief include file for GNU libplot/libplotter.  It
   supplements the include files ../include/sys-defines.h,
   ../include/plot.h and ../include/plotter.h.  plot.h is libplot-specific,
   but plotter.h is included both in libplot and libplotter.  plotter.h
   defines what a Plotter object is (a struct for libplot, but a class for
   libplotter). */

/* This file contains many #defines and declarations of data structures.
   More importantly, it contains declarations of all the Plotter methods.
   They are declared differently, depending on whether we are compiling
   libplot (signalled if NOT_LIBPLOTTER is #defined) or libplotter.

   In libplot, the plotter operations are implemented as global functions
   that are members of the Plotter struct.  They are set up differently for
   the different types of Plotter; for example, the `openpl' slot in the
   struct contains the method _g_openpl for generic [i.e. base] Plotters,
   the method _m_openpl for MetaPlotters, etc.  The files ?_defplot.c
   contain the initializations that are used for the different types of
   Plotter.  In this file, if NOT_LIBPLOTTER is defined then each of these
   many methods is declared as a global function.

   In libplotter, the different types of Plotter are implemented as
   distinct classes, which are derived from the generic [i.e. base] Plotter
   class.  This file contains a great many #defines that are appropriate to
   that situation.  For example, _m_openpl is #defined to be
   MetaPlotter::openpl if NOT_LIBPLOTTER is not defined.  The MetaPlotter
   class, like all other Plotter classes, is defined in plotter.h. */


/* PAUL MURRELL
   Removed all include files stuff
*/

/* PAUL MURRELL
   Added the following (to allow R declarations and definitions)
*/
#include <config.h>
#include <Defn.h>
#include <R_ext/GraphicsEngine.h>

/* PAUL MURRELL
   This type def taken from sys-defines.h
*/
typedef int bool;
#define false 0
#define true 1

/*************************************************************************/
/* DEFINITIONS RELATED TO OUR FONT DATABASE (g_fontdb.c and g_fontd2.c)  */
/*************************************************************************/

/* HERSHEY FONTS */

/* our information about each of the 22 Hershey vector fonts in g_fontdb.c,
   and the typefaces they belong to */
struct plHersheyFontInfoStruct
{
  const char *name;		/* font name */
  const char *othername;	/* an alias (for backward compatibility) */
  const char *orig_name;	/* Allen Hershey's original name for it */
  short chars[256];		/* array of vector glyphs */
  int typeface_index;		/* default typeface for the font */
  int font_index;		/* which font within typeface this is */
  bool obliquing;		/* whether to apply obliquing */
  bool iso8859_1;		/* whether font encoding is iso8859-1 */
  bool visible;			/* whether font is visible, i.e. not internal*/
};

extern const struct plHersheyFontInfoStruct _hershey_font_info[];

/* This numbering should agree with the numbering of Hershey fonts in the
   _hershey_font_info[] array in g_fontdb.c. */
#define HERSHEY_SERIF 0
#define HERSHEY_SERIF_ITALIC 1
#define HERSHEY_SERIF_BOLD 2
#define HERSHEY_CYRILLIC 4
#define HERSHEY_HIRAGANA 6	/* hidden font */
#define HERSHEY_KATAKANA 7	/* hidden font */
#define HERSHEY_EUC 8
#define HERSHEY_GOTHIC_GERMAN 16
#define HERSHEY_SERIF_SYMBOL 18

/* accented character information (used in constructing Hershey ISO-Latin-1
   accented characters, see table in g_fontdb.c) */
struct plHersheyAccentedCharInfoStruct
{
  unsigned char composite, character, accent;
};

extern const struct plHersheyAccentedCharInfoStruct _hershey_accented_char_info[];

/* types of accent, for a composite character in a Hershey font */
#define ACC0 (16384 + 0)	/* superimpose on character */
#define ACC1 (16384 + 1)	/* elevate by 7 Hershey units */
#define ACC2 (16384 + 2)	/* same, also shift right by 2 units */

/* a flag in a Hershey glyph number indicating a `small Kana' */
#define KS 8192			/* i.e. 0x200 */

/* HERSHEY VECTOR GLYPHS */

/* arrays of Hershey vector glyphs in g_her_glyph.c */
extern const char * const _occidental_hershey_glyphs[];
extern const char * const _oriental_hershey_glyphs[];

/* position of `undefined character' symbol (a bundle of horizontal lines)
   in the Hershey _occidental_hershey_glyphs[] array */
#define UNDE 4023

/* TYPEFACES */

/* typeface information, applicable to all four sorts of font in our font
   database (Hershey, PS, PCL, Stick) */

#define FONTS_PER_TYPEFACE 10   /* maximum */

struct plTypefaceInfoStruct
{
  int numfonts;
  int fonts[FONTS_PER_TYPEFACE];
};

extern const struct plTypefaceInfoStruct _hershey_typeface_info[];

/* PAUL MURRELL
   Modified this declaration
*/
extern unsigned short * _controlify (pGEDevDesc, const unsigned char *,
				     int, int);

/* PAUL MURRELL
   Removed heaps below here
*/
