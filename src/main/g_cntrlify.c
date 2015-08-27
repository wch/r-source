/* PAUL MURRELL
   This is from the GNU plotutils libplot-2.3 distribution
   All references to HAVE_PROTOS removed
   All references to "plotter" replaced with references to "GEDevDesc"
*/

/* <UTF8-FIXME> This assumes single-byte encoding */

/* _controlify() converts a "label" (i.e. a character string), which may
   contain troff-like escape sequences, into a string of unsigned shorts.
   The possible troff-like escape sequences are listed in g_cntrlify.h.

   This conversion is to facilitate rendering.  _controlify() is called by
   alabel(), and the converted label is rendered by _alabel_standard(),
   _alabel_stroke(), or _alabel_device(), depending on what sort of font is
   currently selected.  See g_alabel.c (_controlify() is called by
   labelwidth() too).

   If the currently selected font is a font with ISO-Latin-1 encoding, the
   valid escape sequences include escape sequences for the non-ASCII
   ISO-Latin-1 characters.  Also allowed are such escape sequences as \f0,
   \f1, \f2, \f3, \f4, etc., which switch among the various fonts in the
   current typeface:

	\f1	Switch to font #1, basic
	\f2	Switch to font #2, italic
	\f3	Switch to font #3, bold
	\f4	Switch to font #4, bold italic
	\f0	Switch to font #0, symbol (including Greek characters)

   \fP switches to the previously used font (there is a depth-1 stack,
   i.e. only one previous font is remembered, as in troff).

   All typefaces include at least two fonts: fonts #0 and #1.  Some may
   include more than the above five.  Each unsigned short in the converted
   string is really an annotated character: the low byte is the character,
   and the high byte is the font number.

   Besides font annotations, the controlified string may include control
   codes (unsigned shorts with particular bit patterns in their high
   bytes), which are produced by the escape sequences:

	\sp  start superscript
	\ep  end superscript

	\sb  start subscript
	\eb  end subscript

	\mk  mark location
	\rt  return to mark
	     [useful e.g. for underlining, and filling square roots]

    There are also control codes for horizontal shifts.  \r1, \r2, \r4,
    \r6, \r8, \r^ will produce control codes that will shift right by 1 em,
    1/2 em, 1/4 em, 1/6 em, 1/8 em, 1/12 em, respectively.  \l1, \l2, \l4,
    \l6, \l8, \l^ are similar.

    The string of unsigned shorts, which is returned, is allocated with
    malloc and may be freed later. */

/* PAUL MURRELL
   sys-defines.h not used
*/
/* #include "sys-defines.h" */

/* PAUL MURRELL
   extern.h renamed g_extern.h
*/
#include "g_extern.h"
#include "g_control.h"
#include "g_cntrlify.h"
#include "g_jis.h"

/* these two array lengths must agree with values in file g_her_glyph.c */
#define NUM_OCCIDENTAL_HERSHEY_GLYPHS 4400
#define NUM_ORIENTAL_HERSHEY_GLYPHS 5500

/* PAUL MURRELL
   Added typeface and fontindex arguments
*/

attribute_hidden
unsigned short * _controlify (pGEDevDesc dd, const unsigned char *src,
			      int typeface, int fontindex)
{
  unsigned short *dest;
  unsigned char c, d;
  unsigned char esc[3];
  int j = 0;
  int raw_fontnum, raw_symbol_fontnum;
  unsigned short fontword, symbol_fontword;

  /* note: string length can grow by a factor of 6, because a single
     printable character can be mapped to a sequence of unsigned shorts, of
     length up to 6 (see comment below) */
  /* PAUL MURRELL
     replace _plot_xmalloc with R_alloc
  */
  dest = (unsigned short *) R_alloc (6 * strlen ((char *)src) + 1,
				     sizeof(unsigned short));

  /* PAUL MURRELL
     only for Hershey fonts so removed switch ...
  */
      raw_fontnum = _hershey_typeface_info[typeface].fonts[fontindex];
      raw_symbol_fontnum = _hershey_typeface_info[typeface].fonts[0];
  /* Of the following two words, `fontword' is updated whenever an escape
     sequence like \f0, \f1, \f2 etc. is seen, since raw_fontnum is itself
     updated.  But `symbol_fontword' is fixed */
      fontword = (unsigned short)(raw_fontnum << FONT_SHIFT);
      symbol_fontword = (unsigned short)(raw_symbol_fontnum << FONT_SHIFT);

  while (*src != (unsigned char)'\0')
    {
      /* If EUC, check first for high bit and process two-byte characters
	 separately.  This approach is awkward (we duplicate a lot of code
	 here, which appears elsewhere below). */

      if ((raw_fontnum == HERSHEY_EUC)
	  && (*src & 0x80) && (*(src + 1) & 0x80))
	{
	  unsigned char jis_row = *src & ~(0x80);
	  unsigned char jis_col = *(src + 1) & ~(0x80);

	  if (GOOD_JIS_INDEX(jis_row, jis_col))
	    {
	      int jis_glyphindex = 256 * jis_row + jis_col;

	      if (jis_glyphindex >= BEGINNING_OF_KANJI)
		/* in Kanji range, so check if we have it */
		{
#ifndef NO_KANJI
		  const struct kanjipair *kanji = _builtin_kanji_glyphs;
		  bool matched = false;

		  while (kanji->jis != 0)
		    {
		      if (jis_glyphindex == kanji->jis)
			{
			  matched = true;
			  break;
			}
		      kanji++;
		    }
		  if (matched)
		    {
		      dest[j++] = (unsigned short)( RAW_ORIENTAL_HERSHEY_GLYPH | (kanji->nelson));
		      src += 2;
		      continue;	/* back to top of while loop */
		    }
		  else		/* a Kanji we don't have */
		    {
		      /* render as standard `undefined character' glyph */
		      dest[j++] = RAW_HERSHEY_GLYPH | UNDE;
		      src += 2;
		      continue;	/* back to top of while loop */
		    }
#endif /* not NO_KANJI */
		}
	      else
		/* not in Kanji range, so look for it in char table */
		{
		  const struct jis_entry *char_mapping = _builtin_jis_chars;
		  bool matched = false;

		  while (char_mapping->jis != 0)
		    {
		      if (jis_glyphindex == char_mapping->jis)
			{
			  matched = true;
			  break;
			}
		      char_mapping++;
		    }
		  if (matched)
		    /* the entry in the character table maps the JIS
		       character to a character (in 0..255 range) in
		       one of the fonts in the master table in g_fontdb.c */
		    {
		      int fontnum = char_mapping->font;
		      unsigned short charnum = char_mapping->charnum;

		      if (charnum & RAW_HERSHEY_GLYPH)
			/* a raw Hershey glyph, not in any font */
			dest[j++] = RAW_HERSHEY_GLYPH | charnum;
		      else
			/* a character in one of the fonts in g_fontdb.c */
			  dest[j++] = (unsigned short)((fontnum << FONT_SHIFT) | charnum);
		      src += 2;
		      continue; /* back to top of while loop */
		    }
		  else	/* a character we don't have */
		    {
		      /* render as standard `undefined character' glyph */
		      dest[j++] = RAW_HERSHEY_GLYPH | UNDE;
		      src += 2;
		      continue;	/* back to top of while loop */
		    }
		}
	    }
	  else
	    /* JIS index is OOB */
	    {
	      src += 2;
	      continue;		/* back to top of while loop */
	    }
	}

      /* if current font is Hershey, first try to match each ligature
	 pattern (no ligatures supported in non-Hershey fonts) */
      if (1) /* _plotter->drawstate->font_type == F_HERSHEY) */
	{
	  int i;
	  bool matched = false;

	  for (i = 0; i < NUM_LIGATURES; i++)
	    if ((_ligature_tbl[i].font == raw_fontnum)
		&& (strncmp ((char *)src, _ligature_tbl[i].from,
			     strlen (_ligature_tbl[i].from)) == 0))
	      {
		matched = true;
		break;
	      }

	  if (matched)
	    {
	      dest[j++] = fontword | (unsigned short)_ligature_tbl[i].byte;
	      src += strlen (_ligature_tbl[i].from);
	      continue;		/* back to top of while loop */
	    }
	}

      c = *(src++);		/* no ligature, so get single new char */
      if (c != (unsigned char)'\\') /* ordinary char, may pass through */
	{
	  /* if current font is an ISO-Latin-1 Hershey font ... */
	    if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && _hershey_font_info[raw_fontnum].iso8859_1)
	    {
	      int i;
	      bool matched = false;

	      /* check if this is a `raised' ISO-Latin-1 character */
	      for (i = 0; i < NUM_RAISED_CHARS; i++)
		if (c == _raised_char_tbl[i].from)
		  {
		    matched = true;
		    break;
		  }
	      if (matched)	/* it's a raised character */
		{
		  /* map to string of unsigned shorts, length 3 or 6:
		     `begin superscript' control code, [`mark'
		     control code,] replacement char, [`return'
		     control code, underline,] `end superscript' */
		  dest[j++] =
		    (unsigned short) (CONTROL_CODE | C_BEGIN_SUPERSCRIPT);
		  if (_raised_char_tbl[i].underscored) /* also underline */
		    {
		      dest[j++] =
			(unsigned short) (CONTROL_CODE | C_PUSH_LOCATION);
		      dest[j++] =
			fontword | (unsigned short)_raised_char_tbl[i].to;
		      dest[j++] =
			(unsigned short) (CONTROL_CODE | C_POP_LOCATION);
		      /* select appropriate HersheySymbol font */
		      dest[j++] =
			symbol_fontword | (unsigned short)VECTOR_SYMBOL_FONT_UNDERSCORE;
		    }
		  else	/* just print raised char, no underline */
		    dest[j++] =
		      fontword | (unsigned short)_raised_char_tbl[i].to;

		  dest[j++] =
		    (unsigned short) (CONTROL_CODE | C_END_SUPERSCRIPT);

		  continue; /* back to top of while loop */
		}

	      /* since current font is an ISO-Latin-1 Hershey font, also
		 check if this char should be deligatured */
	      for (i = 0; i < NUM_DELIGATURED_CHARS; i++)
		if (c == _deligature_char_tbl[i].from)
		  {
		    matched = true;
		    break;
		  }
	      if (matched)
		{
		  if (_deligature_char_tbl[i].except_font != raw_fontnum)
		    {
		      dest[j++] = fontword
			| (unsigned short)_deligature_char_tbl[i].to[0];
		      dest[j++] = fontword
			| (unsigned short)_deligature_char_tbl[i].to[1];
		      continue;	/* back to top of while loop */
		    }
		}
	    }

	  /* didn't do anything special, so just pass the character thru */
	  dest[j++] = fontword | (unsigned short)c;
	  continue;		/* back to top of while loop */
	}
      else			/* character is a backslash */
	{
	  int i;

	  c = *(src++);		/* grab next character */
	  if (c == (unsigned char)'\0')	/* ASCII NUL ? */
	    {
	      dest[j++] = fontword | (unsigned short)'\\';
	      break;		/* string terminated with a backslash */
	    }

	  if (c == (unsigned char)'\\')
	    {
	      dest[j++] = fontword | (unsigned short)'\\';
	      dest[j++] = fontword | (unsigned short)'\\';
	      continue;		/* saw \\, leave as is */
	    }

	  d = *(src++);
	  if (d == (unsigned char)'\0')
	    {
	      dest[j++] = fontword | (unsigned short)'\\';
	      dest[j++] = fontword | (unsigned short)c;
	      break;		/* string terminated with \c */
	    }

	  esc[0] = c;
	  esc[1] = d;
	  esc[2] = (unsigned char)'\0';	/* have an escape sequence */

	  /* is this an escape seq. (e.g. \#H0001) for a raw Hershey glyph? */
	  if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && esc[0] == '#' && esc[1] == 'H'
	      && src[0] >= '0' && src[0] <= '9'
	      && src[1] >= '0' && src[1] <= '9'
	      && src[2] >= '0' && src[2] <= '9'
	      && src[3] >= '0' && src[3] <= '9')
	    {
	      int glyphindex;

	      glyphindex = (src[3] - '0') + 10 * (src[2] - '0')
		+ 100 * (src[1] - '0') + 1000 * (src[0] - '0');
	      if (glyphindex < NUM_OCCIDENTAL_HERSHEY_GLYPHS)
		{
		  dest[j++] = (unsigned short)(RAW_HERSHEY_GLYPH | glyphindex);
		  src += 4;
		  continue;	/* back to top of while loop */
		}
	    }

#ifndef NO_KANJI
	  /* is this an escape seq. (e.g. \#N0001) for a raw Japanese
	     Hershey glyph (Kanji), as numbered in Nelson's dictionary? */
	  if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && esc[0] == '#' && esc[1] == 'N'
	      && src[0] >= '0' && src[0] <= '9'
	      && src[1] >= '0' && src[1] <= '9'
	      && src[2] >= '0' && src[2] <= '9'
	      && src[3] >= '0' && src[3] <= '9')
	    {
	      int glyphindex;

	      glyphindex = (src[3] - '0') + 10 * (src[2] - '0')
		+ 100 * (src[1] - '0') + 1000 * (src[0] - '0');
	      if (glyphindex < NUM_ORIENTAL_HERSHEY_GLYPHS)
		{
		  dest[j++] = (unsigned short)(RAW_ORIENTAL_HERSHEY_GLYPH | glyphindex);
		  src += 4;
		  continue;	/* back to top of while loop */
		}
	    }
#endif /* not NO_KANJI */

	  /* is this an escape seq. (e.g. \#J0001) for a raw Japanese
	     Hershey glyph (JIS numbering, in hex)? */
	  if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && esc[0] == '#' && esc[1] == 'J'
	      && ((src[0] >= '0' && src[0] <= '9')
		  || (src[0] >= 'a' && src[0] <= 'f')
		  || (src[0] >= 'A' && src[0] <= 'F'))
	      && ((src[1] >= '0' && src[1] <= '9')
		  || (src[1] >= 'a' && src[1] <= 'f')
		  || (src[1] >= 'A' && src[1] <= 'F'))
	      && ((src[2] >= '0' && src[2] <= '9')
		  || (src[2] >= 'a' && src[2] <= 'f')
		  || (src[2] >= 'A' && src[2] <= 'F'))
	      && ((src[3] >= '0' && src[3] <= '9')
		  || (src[3] >= 'a' && src[3] <= 'f')
		  || (src[3] >= 'A' && src[3] <= 'F')))
	    {
	      int jis_glyphindex;
	      int i, hexnum[4];
	      int jis_row, jis_col;

	      for (i = 0; i < 4; i++)
		if (src[i] >= 'a' && src[i] <= 'f')
		  hexnum[i] = 10 + src[i] - 'a';
		else if (src[i] >= 'A' && src[i] <= 'F')
		  hexnum[i] = 10 + src[i] - 'A';
		else /* a decimal digit */
		  hexnum[i] = src[i] - '0';

	      jis_glyphindex = (hexnum[3] + 16 * hexnum[2]
				+ 256 * hexnum[1] + 4096 * hexnum[0]);
	      jis_row = hexnum[1] + 16 * hexnum[0];
	      jis_col = hexnum[3] + 16 * hexnum[2];

	      if (GOOD_JIS_INDEX(jis_row, jis_col))
		{
		  if (jis_glyphindex >= BEGINNING_OF_KANJI)
		    /* in Kanji range, so check if we have it */
		    {
#ifndef NO_KANJI
		      const struct kanjipair *kanji = _builtin_kanji_glyphs;
		      bool matched = false;

		      while (kanji->jis != 0)
			{
			  if (jis_glyphindex == kanji->jis)
			    {
			      matched = true;
			      break;
			    }
			  kanji++;
			}
		      if (matched)
			{
			  dest[j++] = (unsigned short)(RAW_ORIENTAL_HERSHEY_GLYPH | (kanji->nelson));
			  src += 4;
			  continue;	/* back to top of while loop */
			}
		      else		/* a Kanji we don't have */
			{
			  /* render as standard `undefined character' glyph */
			  dest[j++] = RAW_HERSHEY_GLYPH | UNDE;
			  src += 4;
			  continue;	/* back to top of while loop */
			}
#endif /* not NO_KANJI */
		    }
		  else
		    /* not in Kanji range, so look for it in char table */
		    {
		      const struct jis_entry *char_mapping = _builtin_jis_chars;
		      bool matched = false;

		      while (char_mapping->jis != 0)
			{
			  if (jis_glyphindex == char_mapping->jis)
			    {
			      matched = true;
			      break;
			    }
			  char_mapping++;
			}
		      if (matched)
			/* the entry in the character table maps the JIS
			   character to a character (in 0..255 range) in
			   one of the fonts in the master table in g_fontdb.c*/
			{
			  int fontnum = char_mapping->font;
			  unsigned short charnum = char_mapping->charnum;

			  if (charnum & RAW_HERSHEY_GLYPH)
			    /* a raw Hershey glyph, not in any font */
			    dest[j++] = RAW_HERSHEY_GLYPH | charnum;
			  else
			    /* a character in one of the fonts in g_fontdb.c */
			    dest[j++] = (unsigned short)((fontnum << FONT_SHIFT) | charnum);
			  src += 4;
			  continue; /* back to top of while loop */
			}
		      else	/* a character we don't have */
			{
			  /* render as standard `undefined character' glyph */
			  dest[j++] = RAW_HERSHEY_GLYPH | UNDE;
			  src += 4;
			  continue;	/* back to top of while loop */
			}
		    }
		}
	    }

	  {
	    bool matched = false;

	    /* is this an escape seq. for a control code? */
	    for (i = 0; i < NUM_CONTROLS; i++)
	      if (strcmp ((char *)esc, _control_tbl[i]) == 0)
		{
		  matched = true;
		  break;
		}
	    if (matched)		/* it's a control code */
	      {
		dest[j++] = (unsigned short)(CONTROL_CODE | i);
		continue;	/* back to top of while loop */
	      }
	  }

	  /* if current font is an ISO-Latin-1 Hershey font, is this an
	     escape sequence for an 8-bit (non-ASCII) char, which due to
	     nonexistence should be deligatured? */
	  if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && _hershey_font_info[raw_fontnum].iso8859_1)
	    {
	      int i;
	      bool matched = false;

	      for (i = 0; i < NUM_DELIGATURED_ESCAPES; i++)
		if (strcmp ((char *)esc, _deligature_escape_tbl[i].from) == 0)
		  {
		    matched = true;
		    break;
		  }
	      if (matched)
		{
		  if (_deligature_escape_tbl[i].except_font != raw_fontnum)
		    {
		      dest[j++] = fontword
			| (unsigned short)_deligature_escape_tbl[i].to[0];
		      dest[j++] = fontword
			| (unsigned short)_deligature_escape_tbl[i].to[1];

		      continue;	/* back to top of while loop */
		    }
		}
	    }

	  /* if the current font is an ISO-Latin-1 font (no matter whether
	     font is a a Hershey font, a PS or PCL/Stick font, or a
	     device-specific font for which we have no table entry), is
	     this an escape seq. for an 8-bit (non-ASCII) ISO8859-1 char?  */

	  /* PAUL MURRELL
	     Only concerned with Hershey fonts
	  */
/*
	  if ((_plotter->drawstate->font_type == F_POSTSCRIPT
	       && _ps_font_info[raw_fontnum].iso8859_1)
	      || (_plotter->drawstate->font_type == F_HERSHEY
		  && _hershey_font_info[raw_fontnum].iso8859_1)
	      || (_plotter->drawstate->font_type == F_PCL
		  && _pcl_font_info[raw_fontnum].iso8859_1)
	      || (_plotter->drawstate->font_type == F_STICK
		  && _stick_font_info[raw_fontnum].iso8859_1)
	      || (_plotter->drawstate->font_type == F_OTHER
		  && _plotter->drawstate->font_is_iso8859_1
		  && raw_fontnum == 1))
*/
	  if (1 /* _plotter->drawstate->font_type == F_HERSHEY */
	      && _hershey_font_info[raw_fontnum].iso8859_1)
	    {
	      bool matched = false;

	      for (i = 0; i < NUM_ISO_ESCAPES; i++)
		if (strcmp ((char *)esc, _iso_escape_tbl[i].string) == 0)
		  {
		    matched = true;
		    break;
		  }
	      if (matched)	/* it's an 8-bit ISO8859-1 character */
		{
		  /* certain such characters are drawn in the Hershey fonts
		     as superscripts */
		    if (1) /* _plotter->drawstate->font_type == F_HERSHEY) */
		    {
		      int k;
		      bool matched2 = false;

		      /* check if this is a `raised' ISO-Latin-1 character */
		      for (k = 0; k < NUM_RAISED_CHARS; k++)
			if (_iso_escape_tbl[i].byte == _raised_char_tbl[k].from)
			  {
			    matched2 = true;
			    break;
			  }
		      if (matched2)	/* it's a raised character */
			{
			  /* map to string of unsigned shorts, length 3 or 6:
			     `begin superscript' control code, [`mark'
			     control code,] replacement char, [`return'
			     control code, underline,] `end superscript' */
			  dest[j++] =
			    (unsigned short) (CONTROL_CODE | C_BEGIN_SUPERSCRIPT);
			  if (_raised_char_tbl[k].underscored) /* also underline */
			    {
			      dest[j++] =
				(unsigned short) (CONTROL_CODE | C_PUSH_LOCATION);
			      dest[j++] =
				fontword | (unsigned short)_raised_char_tbl[k].to;
			      dest[j++] =
				(unsigned short) (CONTROL_CODE | C_POP_LOCATION);
			      /* select appropriate HersheySymbol font */
			      dest[j++] =
				symbol_fontword | (unsigned short)VECTOR_SYMBOL_FONT_UNDERSCORE;
			    }
			  else	/* just print raised char, no underline */
			    {
			      dest[j++] =
				fontword | (unsigned short)_raised_char_tbl[k].to;
			    }

			  dest[j++] =
			    (unsigned short) (CONTROL_CODE | C_END_SUPERSCRIPT);

			  continue; /* back to top of while loop */
			}
		    }

		  /* won't draw this char as a superscript; just pass thru */
		  dest[j++] = fontword | (unsigned short)(_iso_escape_tbl[i].byte);
		  continue;	/* back to top of while loop */
		}
	    }

	  /* is this an escape seq. for a `special' (non-ISO, non-Symbol)
	     Hershey glyph?  Such glyphs include astronomical signs, and
	     `final s'. */
	  if (1) /* _plotter->drawstate->font_type == F_HERSHEY) */
	    {
	      bool matched = false;

	      for (i = 0; i < NUM_SPECIAL_ESCAPES; i++)
		if (strcmp ((char *)esc, _special_escape_tbl[i].string) == 0)
		  {
		    matched = true;
		    break;
		  }
	      if (matched)	/* it's a special character */
		{
		  /* "\s-" is special; yields character in current font */
		  if (_special_escape_tbl[i].byte == FINAL_LOWERCASE_S)
		  dest[j++] =
		    fontword | (unsigned short)(_special_escape_tbl[i].byte);
		  else
		  /* we select symbol font of typeface, in which we've
		     stored all other special characters */
		    dest[j++] = symbol_fontword | (unsigned short)(_special_escape_tbl[i].byte);
		  continue;	/* back to top of while loop */
		}
	    }

	  {
	    bool matched = false;

	    /* Irrespective of font type, is this an escape seq. for a char
	       in the font's corresponding symbol font? */
	    for (i = 0; i < NUM_SYMBOL_ESCAPES; i++)
	      if (strcmp (_symbol_escape_tbl[i].string, "NO_ABBREV") != 0
		  && strcmp ((char *)esc, _symbol_escape_tbl[i].string) == 0)
		{
		  matched = true;
		  break;
		}
	    if (matched)	/* it's a character in the symbol font */
	      {
		/* select symbol font by OR'ing in the symbol fontword */
		dest[j++] = symbol_fontword | (unsigned short)(_symbol_escape_tbl[i].byte);
		continue;	/* back to top of while loop */
	      }
	  }

	  /* Gross kludge.  In the non-Hershey fonts we handle the "\rn"
	     control sequence in a painful way.  For a PS font we map it
	     into (1) a left shift, (2) the `radicalex' character in the PS
	     Symbol font, and (3) a right shift.  Shift distances are taken
	     from the bbox of the radicalex char, and are slightly larger
	     than 0.5 em.  For a PCL font it's similar, but the shifts are
	     much smaller.  The reason it's different for PCL is that the
	     PCL radicalex character is different from the PS radicalex
	     character: the overbar is not displaced.  Possibly someone at
	     HP made a mistake while reimplementing the Adobe Symbol font
	     for PCL 5?

	     We don't implement \rn for Stick fonts, because they have
	     no associated symbol font. */

	  /* couldn't match; unknown escape seq., so pass through unchanged */
	  dest[j++] = fontword | (unsigned short)'\\';
	  dest[j++] = fontword | (unsigned short)c;
	  dest[j++] = fontword | (unsigned short)d;
	}
    }

  dest[j] = (unsigned short)'\0';   /* terminate string */

  return dest;
}

#ifdef UNUSED
int
#ifdef _HAVE_PROTOS
_codestring_len (const unsigned short *codestring)
#else
_codestring_len (codestring)
     const unsigned short *codestring;
#endif
{
  int i = 0;

  while (*codestring)
    {
      i++;
      codestring++;
    }

  return i;
}
#endif
