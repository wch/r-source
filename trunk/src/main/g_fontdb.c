/* PAUL MURRELL
   This is from the GNU plotutils libplot-2.3 distribution, with PS font
   stuff removed
*/

/* THIS FILE IS NOW SPLIT INTO TWO PIECES: g_fontdb.c and g_fontd2.c, TO
   FACILITATE COMPILING.  This is the first half, containing PS fonts and
   Hershey vector fonts. */

/*----------------------------------------------------------------------*/

/* This file contains information about PS fonts, Hershey vector fonts, PCL
   fonts, and Stick fonts (i.e., device-resident vector fonts), and about
   the way in which they are divided into typefaces.  For the non-Hershey
   fonts, the most important information is the width tables.  For the
   Hershey fonts, it is the mapping from the character position in the
   font, to the position of the corresponding glyph in the glyph array in
   g_her_glyph.c.  Accented characters are formed as composites. */

/* PAUL MURRELL
   sys-defines.h not used
*/
/* #include "sys-defines.h" */

/* PAUL MURRELL
   extern.h renamed g_extern.h
*/
#include "g_extern.h"

/* PAUL MURRELL
   I have deleted the PS fonts stuff to leave just the Hershey font stuff
*/

/* The Hershey vector fonts we support.	 Each character in a Hershey font
   is an index into the glyph array in g_her_glyphs.c.	Each
   plHersheyFontInfoStruct includes these elements:

   (1) PS-style name for the font
   (2) an alias for the font (for backward compatibility)
   (3) Allen Hershey's original name for the font
   (4) the characters in the font (an array of glyph indices, size 256)
   (5) a typeface id (an index into the _ps_typeface_info[] array below)
   (6) a font index (which font within the typeface this is)
   (7) an `obliquing requested' flag (set if glyphs should be sheared)
   (8) an `iso8859-1' flag
   (9) a `visible' flag (false for the two Kana fonts,
	which are only used internally) */

/* Each Hershey font below may contain up to 256 Hershey glyphs, each of
   which is specified by a number that indexes into the array in
   g_her_glyph.c.  Only the ranges 0x20..0x7e and 0xa0..0xff are directly
   accessible to the user.

   There are several sorts of hole in the Hershey fonts, i.e., entries in
   the character arrays in these printable ranges which are not proper
   glyphs.  They are:

   0. undefined characters: thorn, eth, the guillemets, the mysterious
   `currency' symbol, and the European 'euro' currency symbol.	These are
   written as UNDE, which is a glyph consisting of several horizontal
   lines.

   1. the characters onesuperior, twosuperior, threesuperior, ordmasculine,
   ordfeminine.	 These are written as 0, which means an empty glyph.  That
   is because they not implemented as conventional glyphs at all: the
   corresponding entries in this table are never accessed.  In the file
   g_cntrlify.c these characters are mapped to sequences of control
   sequences, which implement them as superscripts.

   2. the characters ae, AE, and germandbls (i.e. eszet), which (except in
   Gothic-German) are also written as 0.  That is because they are
   `deligatured' in g_cntrlify.c, via a translation table in g_cntrlify.h.
   Deligaturization maps them to the two-characters sequences "ae", "AE",
   and "ss".  The corresponding entries in this table are never accessed.

   3. the accented ISO-Latin-1 characters.  These are written as ACC0,
   ACC1, or ACC2, any of which signifies that they should be looked up in
   the following table of accented characters, yielding an accent and a
   character.  They signify respectively that the accent should be
   superimposed on the character, that it should be raised by 7 Hershey
   units (for capitals), and that it should be raised by 7 units and also
   displaced rightward by 2 units (for italic capitals).

   4. the small Katakana in the HersheyEUC font.  These are written as KS
   (i.e. Katakana small) + true glyph number.  This means that they should
   be isotropically compressed, with the baseline preserved.


   In many Hershey fonts, the `nonprintable' range 0x80..0x9f is
   nonprintable only in the sense that it is not directly accessible to the
   user; it may contain ligatures, macros, and variant characters.  In fact
   the just-mentioned accents, used in the construction of accented
   ISO-Latin-1 characters, are stored there.  Our convention for the
   0x80..0x9f range, for ISO-Latin-1 Hershey fonts, is:

   0200: ff
   0201: fi
   0202: fl
   0203: ffi
   0204: ffl
   0205--0207: other ligatures (font-specific)
   0210: acute accent
   0211: grave accent
   0212: dieresis
   0213: circumflex accent
   0214: circumflex accent for `i'
   0215: tilde accent
   0216: ring accent for `a'
   0217: cedilla	[NOT YET IMPLEMENTED]
   0230: variant [final] form for `s'
   0231: dotless i

   In the symbol fonts, we store miscellaneous symbols and astronomical
   symbols in this range.  In fact, in the symbol fonts the astronomical
   symbols overflow to the other nonprintable range 0x00..0x1f
   (the twelve zodiacal signs are stored there). */

#define CEDILLA UNDE   /* currently undefined, to be implemented someday */

/* IMPORTANT: The fonts in this array may be referred to elsewhere in the
   code by number.  If you change the numbering of Hershey fonts, i.e., the
   order in which they appear in this array, be sure to update, e.g., the
   definitions DEFAULT_HERSHEY_FONT_INDEX, HERSHEY_SERIF, HERSHEY_EUC
   etc. in ./g_extern.h. */

/* LIST them by  (occur "^ *\"Hershey.*#" nil)  <<C-x C-e in Emacs */

const struct plHersheyFontInfoStruct _hershey_font_info[] =
{
  {
    "HersheySerif",		/* #0 */
    NULL,
    "Complex Roman",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 2217, 2275, 2274, 2271, 2272, 2251,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
      2273, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
      2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
      2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2024, 2025, 2026, 2223, 4002, 2224, 4110, 4013,
      2252, 2101, 2102, 2103, 2104, 2105, 2106, 2107,
      2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115,
      2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123,
      2124, 2125, 2126, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      2177, 2178, 2179, 2180, 2181, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      /* PAUL MURRELL
	 Added accents for "Oslash" and "oslash"
      */
      /*      0, 0, 0, 0, 0, 0, 0, 0, */
      802, 220, 0, 0, 0, 0, 0, 0,
      2119, 2182, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      2199, 4113,  910,	 272, UNDE, 4125, 4106, 2276,
      4182,  274,    0, UNDE, 4080, 4104,  273, 4187,
      2218, 2233,    0,	   0, 4180, 2138, UNDE, 729,
      CEDILLA, 0,    0, UNDE, 270,  261,  271, 4114,
      ACC1, ACC1, ACC1, ACC1, ACC1, 2078,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      /* PAUL MURRELL
	 Changed coding for "Oslash"
      */
      /*      2015, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0, */
      ACC0, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      /* PAUL MURRELL
	 Changed coding for "oslash"
      */
      /*      2115, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0, */
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    0, 1, false, true, true
  },
  {
    "HersheySerif-Italic",	/* #1 */
    NULL,
    "Complex Italic",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 2217, 2275, 2274, 2271, 2272, 2251,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2770,
      2750, 2751, 2752, 2753, 2754, 2755, 2756, 2757,
      2758, 2759, 2212, 2213, 2241, 2238, 2242, 2215,
      2273, 2051, 2052, 2053, 2054, 2055, 2056, 2057,
      2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065,
      2066, 2067, 2068, 2069, 2070, 2071, 2072, 2073,
      2074, 2075, 2076, 2223, 4002, 2224, 4110, 4013,
      2252, 2151, 2152, 2153, 2154, 2155, 2156, 2157,
      2158, 2159, 2160, 2161, 2162, 2163, 2164, 2165,
      2166, 2167, 2168, 2169, 2170, 2171, 2172, 2173,
      2174, 2175, 2176, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      2191, 2192, 2193, 2194, 2195, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2169, 2196, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      2199, 4113,  910,	 272, UNDE, 4129, 4106, 2276,
      4182,  274,    0, UNDE, 4080, 4104,  273, 4187,
      2218, 2233,    0,	   0, 4180, 2138, UNDE,	 729,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4114,
      ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,    0, ACC0,
      ACC2, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,
      UNDE, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,	 727,
      2065, ACC2, ACC2, ACC2, ACC2, ACC2, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      2165, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    0, 2, false, true, true
  },
  {
    "HersheySerif-Bold",		/* #2 */
    NULL,
    "Triplex Roman",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3249, 3214, 3228, 3232, 3219, 3233, 3218, 3217,
      3221, 3222, 3223, 3225, 3211, 3224, 3210, 3220,
      3200, 3201, 3202, 3203, 3204, 3205, 3206, 3207,
      3208, 3209, 3212, 3213, 3230, 3226, 3231, 3215,
      3234, 3001, 3002, 3003, 3004, 3005, 3006, 3007,
      3008, 3009, 3010, 3011, 3012, 3013, 3014, 3015,
      3016, 3017, 3018, 3019, 3020, 3021, 3022, 3023,
      3024, 3025, 3026, 2223, 4178, 2224, 4110, 4013,
      3216, 3101, 3102, 3103, 3104, 3105, 3106, 3107,
      3108, 3109, 3110, 3111, 3112, 3113, 3114, 3115,
      3116, 3117, 3118, 3119, 3120, 3121, 3122, 3123,
      3124, 3125, 3126, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3119, 4160, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      3249, 4119,  910,	 272, UNDE, 4126, 4107, 2276,
      4182,  274,    0, UNDE, 4080, 4105,  273, 4187,
      3229, 2233,    0,	   0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4120,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      3015, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      3115, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    0, 3, false, true, true
  },
  {
    "HersheySerif-BoldItalic",	/* #3 */
    NULL,
    "Triplex Italic",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3249, 3264, 3278, 3282, 3269, 3283, 3268, 3267,
      3271, 3272, 3273, 3275, 3261, 3274, 3260, 3270,
      3250, 3251, 3252, 3253, 3254, 3255, 3256, 3257,
      3258, 3259, 3262, 3263, 3280, 3276, 3281, 3265,
      3284, 3051, 3052, 3053, 3054, 3055, 3056, 3057,
      3058, 3059, 3060, 3061, 3062, 3063, 3064, 3065,
      3066, 3067, 3068, 3069, 3070, 3071, 3072, 3073,
      3074, 3075, 3076, 2223, 4178, 2224, 4110, 4013,
      3266, 3151, 3152, 3153, 3154, 3155, 3156, 3157,
      3158, 3159, 3160, 3161, 3162, 3163, 3164, 3165,
      3166, 3167, 3168, 3169, 3170, 3171, 3172, 3173,
      3174, 3175, 3176, 2225, 4108, 2226, 2246,	   0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3169, 4161, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      3249, 4121,  910,	 272, UNDE, 4130, 4107, 2276,
      4182,  274,    0, UNDE, 4080, 4105,  273, 4187,
      3279, 2233,    0,	   0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4122,
      ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,    0, ACC0,
      ACC2, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,
      UNDE, ACC2, ACC2, ACC2, ACC2, ACC2, ACC2,	 727,
      3065, ACC2, ACC2, ACC2, ACC2, ACC2, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      3165, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    0, 4, false, true, true
  },
  {
    "HersheyCyrillic",		/* #4 */
    NULL,
    "Complex Cyrillic",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 2217, 2275, 2274, 2271, 2272, 2251,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
      2273, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
      2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
      2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2024, 2025, 2026, 2223, 4002, 2224, 4110, 4013,
      2252, 2101, 2102, 2103, 2104, 2105, 2106, 2107,
      2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115,
      2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123,
      2124, 2125, 2126, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      2177, 2178, 2179, 2180, 2181, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2119, 0, 0, 0, 0, 0, 0, 0,
      /* begin bogus region */
      0, 0, 0, ACC0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, ACC1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 274,
      /* end bogus region */
      2931, 2901, 2902, 2923, 2905, 2906, 2921, 2904,
      2922, 2909, 2910, 2911, 2912, 2913, 2914, 2915,
      2916, 2932, 2917, 2918, 2919, 2920, 2907, 2903,
      2929, 2928, 2908, 2925, 2930, 2926, 2924, 2927,
      2831, 2801, 2802, 2823, 2805, 2806, 2821, 2804,
      2822, 2809, 2810, 2811, 2812, 2813, 2814, 2815,
      2816, 2832, 2817, 2818, 2819, 2820, 2807, 2803,
      2829, 2828, 2808, 2825, 2830, 2826, 2824, 2827,
    },
    0, 5, false, false, true
  },
  {
    "HersheyCyrillic-Oblique",	/* #5 */
    NULL,
    "Complex Cyrillic (obliqued)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 2217, 2275, 2274, 2271, 2272, 2251,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
      2273, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
      2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
      2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2024, 2025, 2026, 2223, 4002, 2224, 4110, 4013,
      2252, 2101, 2102, 2103, 2104, 2105, 2106, 2107,
      2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115,
      2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123,
      2124, 2125, 2126, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      2177, 2178, 2179, 2180, 2181, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2119, 0, 0, 0, 0, 0, 0, 0,
      /* begin bogus region */
      0, 0, 0, ACC0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, ACC1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 274,
      /* end bogus region */
      2931, 2901, 2902, 2923, 2905, 2906, 2921, 2904,
      2922, 2909, 2910, 2911, 2912, 2913, 2914, 2915,
      2916, 2932, 2917, 2918, 2919, 2920, 2907, 2903,
      2929, 2928, 2908, 2925, 2930, 2926, 2924, 2927,
      2831, 2801, 2802, 2823, 2805, 2806, 2821, 2804,
      2822, 2809, 2810, 2811, 2812, 2813, 2814, 2815,
      2816, 2832, 2817, 2818, 2819, 2820, 2807, 2803,
      2829, 2828, 2808, 2825, 2830, 2826, 2824, 2827,
    },
    0, 6, true, false, true
  },
  {
    "HersheyHiragana",		/* #6 */
    NULL,
    "Hiragana (from oriental glyph database)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      4399, 4200+KS, 4200, 4201+KS, 4201, 4202+KS, 4202, 4203+KS,
      4203, 4204+KS, 4204, 4205, 4255, 4206, 4256, 4207,
      4257, 4208, 4258, 4209, 4259, 4210, 4260, 4211,
      4261, 4212, 4262, 4213, 4263, 4214, 4264, 4215,
      4265, 4216, 4266, 4217+KS, 4217, 4267, 4218, 4268,
      4219, 4269, 4220, 4221, 4222, 4223, 4224, 4225,
      4270, 4275, 4226, 4271, 4276, 4227, 4272, 4277,
      4228, 4273, 4278, 4229, 4274, 4279, 4230, 4231,
      4232, 4233, 4234, 4235+KS, 4235, 4237+KS, 4237, 4239+KS,
      4239, 4240, 4241, 4242, 4243, 4244, 4245+KS, 4245,
      4246, 4248, 4249, 4250,	 0,    0,    0,	   0,
      4197, 4196, 4195,	   0,	 0,    0,    0,	   0,
      /********************/
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
    },
    0, 6, false, false, false
  },
  {
    "HersheyKatakana",		/* #7 */
    NULL,
    "Katakana (from oriental glyph database)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      4399, 4300+KS, 4300, 4301+KS, 4301, 4302+KS, 4302, 4303+KS,
      4303, 4304+KS, 4304, 4305, 4355, 4306, 4356, 4307,
      4357, 4308, 4358, 4309, 4359, 4310, 4360, 4311,
      4361, 4312, 4362, 4313, 4363, 4314, 4364, 4315,
      4365, 4316, 4366, 4317+KS, 4317, 4367, 4318, 4368,
      4319, 4369, 4320, 4321, 4322, 4323, 4324, 4325,
      4370, 4375, 4326, 4371, 4376, 4327, 4372, 4377,
      4328, 4373, 4378, 4329, 4374, 4379, 4330, 4331,
      4332, 4333, 4334, 4335+KS, 4335, 4337+KS, 4337, 4339+KS,
      4339, 4340, 4341, 4342, 4343, 4344, 4345+KS, 4345,
      4346, 4348, 4349, 4350, 4398, 4305+KS, 4308+KS,  0,
      4197, 4196, 4195,	   0,	 0,    0,    0,	   0,
      /********************/
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
    },
    0, 7, false, false, false
  },
  {
    "HersheyEUC",			/* #8 */
    NULL,
    "Composite Japanese (from oriental glyph database)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 2217, 2275, 2274, 2271, 2272, 2251,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
      2273, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
      2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
      2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
      2024, 2025, 2026, 2223, 4125, 2224, 4110, 4013,
      2252, 2101, 2102, 2103, 2104, 2105, 2106, 2107,
      2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115,
      2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123,
      2124, 2125, 2126, 2225, 2229, 2226, 4008, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      2177, 2178, 2179, 2180, 2181, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2119, 2182, 0, 0, 0, 0, 0, 0,
      /* printable single-byte 8-bit characters (none for this font) */
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
	 0,    0,    0,	   0,	 0,    0,    0,	   0,
    },
    0, 7, false, false, true
  },
  {
    "HersheySans",		/* #9 */
    NULL,
    "Simplex Roman",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      699, 714, 717, 733, 719, 697, 734, 731,
      721, 722, 728, 725, 711, 724, 710, 720,
      700, 701, 702, 703, 704, 705, 706, 707,
      708, 709, 712, 713, 691, 726, 692, 715,
      690, 501, 502, 503, 504, 505, 506, 507,
      508, 509, 510, 511, 512, 513, 514, 515,
      516, 517, 518, 519, 520, 521, 522, 523,
      524, 525, 526, 693, 4002, 694, 4110, 4013,
      730, 601, 602, 603, 604, 605, 606, 607,
      608, 609, 610, 611, 612, 613, 614, 615,
      616, 617, 618, 619, 620, 621, 622, 623,
      624, 625, 626, 695, 723, 696, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4188, 4189, 4182, 4190, 4191, 4185, 4186, 0,
      /* PAUL MURRELL
	 Added accents for "Oslash" and "oslash"
      */
      /*      0, 0, 0, 0, 0, 0, 0, 0, */
      802, 220, 0, 0, 0, 0, 0, 0,
      619, 4162, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      699, 4111,  910,	272, UNDE, 4127, 4106, 2276,
      4182,  274, 0, UNDE, 4080, 4104,	273, 4187,
      718, 2233, 0, 0, 4188, 638, UNDE, 729,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4112,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      /* PAUL MURRELL
	 Changed coding for "Oslash"
      */
      /*      515, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	  0, */
      ACC0, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      /* PAUL MURRELL
	 Changed coding for "oslash"
      */
      /*      615, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0, */
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    1, 1, false, true, true
  },
  {
    "HersheySans-Oblique",	/* #10 */
    NULL,
    "Simplex Roman (obliqued)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      699, 714, 717, 733, 719, 697, 734, 731,
      721, 722, 728, 725, 711, 724, 710, 720,
      700, 701, 702, 703, 704, 705, 706, 707,
      708, 709, 712, 713, 691, 726, 692, 715,
      690, 501, 502, 503, 504, 505, 506, 507,
      508, 509, 510, 511, 512, 513, 514, 515,
      516, 517, 518, 519, 520, 521, 522, 523,
      524, 525, 526, 693, 4002, 694, 4110, 4013,
      730, 601, 602, 603, 604, 605, 606, 607,
      608, 609, 610, 611, 612, 613, 614, 615,
      616, 617, 618, 619, 620, 621, 622, 623,
      624, 625, 626, 695, 723, 696, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4188, 4189, 4182, 4190, 4191, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      619, 4162, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      699, 4111,  910,	272, UNDE, 4127, 4106, 2276,
      4182,  274, 0, UNDE, 4080, 4104,	273, 4187,
      718, 2233, 0, 0, 4188, 638, UNDE, 729,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4112,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
       515, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
       615, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    1, 2, true, true, true
  },
  {
    "HersheySans-Bold",		/* #11 */
    NULL,
    "Duplex Roman",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2699, 2714, 2728, 2732, 2719, 2733, 2718, 2717,
      2721, 2722, 2723, 2725, 2711, 2724, 2710, 2720,
      2700, 2701, 2702, 2703, 2704, 2705, 2706, 2707,
      2708, 2709, 2712, 2713, 2730, 2726, 2731, 2715,
      2734, 2501, 2502, 2503, 2504, 2505, 2506, 2507,
      2508, 2509, 2510, 2511, 2512, 2513, 2514, 2515,
      2516, 2517, 2518, 2519, 2520, 2521, 2522, 2523,
      2524, 2525, 2526, 2223, 4178, 2224, 4110, 4013,
      2716, 2601, 2602, 2603, 2604, 2605, 2606, 2607,
      2608, 2609, 2610, 2611, 2612, 2613, 2614, 2615,
      2616, 2617, 2618, 2619, 2620, 2621, 2622, 2623,
      2624, 2625, 2626, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2619, 4163, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      2699, 4115,  910,	 272, UNDE, 4128, 4107, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      2729, 2233, 0, 0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4116,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      2515, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      2615, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    1, 3, false, true, true
  },
  {
    "HersheySans-BoldOblique",	/* #12 */
    NULL,
    "Duplex Roman (obliqued)",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2699, 2714, 2728, 2732, 2719, 2733, 2718, 2717,
      2721, 2722, 2723, 2725, 2711, 2724, 2710, 2720,
      2700, 2701, 2702, 2703, 2704, 2705, 2706, 2707,
      2708, 2709, 2712, 2713, 2730, 2726, 2731, 2715,
      2734, 2501, 2502, 2503, 2504, 2505, 2506, 2507,
      2508, 2509, 2510, 2511, 2512, 2513, 2514, 2515,
      2516, 2517, 2518, 2519, 2520, 2521, 2522, 2523,
      2524, 2525, 2526, 2223, 4178, 2224, 4110, 4013,
      2716, 2601, 2602, 2603, 2604, 2605, 2606, 2607,
      2608, 2609, 2610, 2611, 2612, 2613, 2614, 2615,
      2616, 2617, 2618, 2619, 2620, 2621, 2622, 2623,
      2624, 2625, 2626, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2619, 4163, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      2699, 4115,  910,	 272, UNDE, 4128, 4107, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      2729, 2233, 0, 0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4116,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      2515, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      2615, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    1, 4, true, true, true
  },
  {
    "HersheyScript",		/* #13 */
    NULL,
    "Simplex Script",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      699, 714, 717, 733, 719, 697, 734, 731,
      721, 722, 728, 725, 711, 724, 710, 720,
      700, 701, 702, 703, 704, 705, 706, 707,
      708, 709, 712, 713, 691, 726, 692, 715,
      690, 551, 552, 553, 554, 555, 556, 557,
      558, 559, 560, 561, 562, 563, 564, 565,
      566, 567, 568, 569, 570, 571, 572, 573,
      574, 575, 576, 693, 4002, 694, 4110, 4013,
      730, 651, 652, 653, 654, 655, 656, 657,
      658, 659, 660, 661, 662, 663, 664, 665,
      666, 667, 668, 669, 670, 671, 672, 673,
      674, 675, 676, 695, 723, 696, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      669, 0, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      699, 4111,  910,	272, UNDE, 4127, 4106, 2276,
      4182,  274, 0, UNDE, 4080, 4104,	273, 4187,
      718, 2233, 0, 0, 4180, 638, UNDE, 729,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4112,
      551, 551, 551, 551, 551, 551, UNDE, 553,
      555, 555, 555, 555, 559, 559, 559, 559,
      UNDE, 564, 565, 565, 565, 565, 565, 727,
      565, 571, 571, 571, 571, 575, UNDE, 0,
      651, 651, 651, 651, 651, 651, 0, 653,
      655, 655, 655, 655, 659, 659, 659, 659,
      UNDE, 664, 665, 665, 665, 665, 665, 2237,
      665, 671, 671, 671, 671, 675, UNDE, 675
    },
    2, 1, false, true, true
  },
  {
    "HersheyScript-Bold",		/* #14 */
    NULL,
    "Complex Script",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2749, 2764, 2778, 2782, 2769, 2783, 2768, 2767,
      2771, 2772, 2773, 2775, 2761, 2774, 2760, 2770,
      2750, 2751, 2752, 2753, 2754, 2755, 2756, 2757,
      2758, 2759, 2762, 2763, 2780, 2776, 2781, 2765,
      2784, 2551, 2552, 2553, 2554, 2555, 2556, 2557,
      2558, 2559, 2560, 2561, 2562, 2563, 2564, 2565,
      2566, 2567, 2568, 2569, 2570, 2571, 2572, 2573,
      2574, 2575, 2576, 2223, 4002, 2224, 4110, 4013,
      2766, 2651, 2652, 2653, 2654, 2655, 2656, 2657,
      2658, 2659, 2660, 2661, 2662, 2663, 2664, 2665,
      2666, 2667, 2668, 2669, 2670, 2671, 2672, 2673,
      2674, 2675, 2676, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2669, 0, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      2749, 4117,  910,	 272, UNDE, 4128, 4106, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      2779, 2233, 0, 0, 4180, 638, UNDE, 729,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4118,
      2551, 2551, 2551, 2551, 2551, 2551, 0, 2553,
      2555, 2555, 2555, 2555, 2559, 2559, 2559, 2559,
      UNDE, 2554, 2555, 2555, 2555, 2555, 2555, 727,
      2555, 2571, 2571, 2571, 2571, 2575, UNDE, 0,
      2651, 2651, 2651, 2651, 2651, 2651, 0, 2653,
      2655, 2655, 2655, 2655, 2659, 2659, 2659, 2659,
      UNDE, 2664, 2665, 2665, 2665, 2665, 2665, 2237,
      2665, 2671, 2671, 2671, 2671, 2675, UNDE, 2675
    },
    2, 3, false, true, true
  },
  {
    "HersheyGothicEnglish",	/* #15 */
    "HersheyGothic-English",
    "Gothic English",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3699, 3714, 3728, 3732, 3719, 3733, 3718, 3717,
      3721, 3722, 3723, 3725, 3711, 3724, 3710, 3720,
      3700, 3701, 3702, 3703, 3704, 3705, 3706, 3707,
      3708, 3709, 3712, 3713, 3730, 3726, 3731, 3715,
      3734, 3501, 3502, 3503, 3504, 3505, 3506, 3507,
      3508, 3509, 3510, 3511, 3512, 3513, 3514, 3515,
      3516, 3517, 3518, 3519, 3520, 3521, 3522, 3523,
      3524, 3525, 3526, 2223, 4178, 2224, 4110, 4013,
      3716, 3601, 3602, 3603, 3604, 3605, 3606, 3607,
      3608, 3609, 3610, 3611, 3612, 3613, 3614, 3615,
      3616, 3617, 3618, 3619, 3620, 3621, 3622, 3623,
      3624, 3625, 3626, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3619, 4165, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      3699, 4123,  910,	 272, UNDE, 4126, 4107, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      3729, 2233, 0, 0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4124,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      3515, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      3615, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    3, 1, false, true, true
  },
  {
    "HersheyGothicGerman",	/* #16 */
    "HersheyGothic-German",
    "Gothic German",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3249, 3214, 3228, 3232, 3219, 3233, 3218, 3217,
      3221, 3222, 3223, 3225, 3211, 3224, 3210, 3220,
      3200, 3201, 3202, 3203, 3204, 3205, 3206, 3207,
      3208, 3209, 3212, 3213, 3230, 3226, 3231, 3215,
      3234, 3301, 3302, 3303, 3304, 3305, 3306, 3307,
      3308, 3309, 3310, 3311, 3312, 3313, 3314, 3315,
      3316, 3317, 3318, 3319, 3320, 3321, 3322, 3323,
      3324, 3325, 3326, 2223, 4178, 2224, 4110, 4013,
      3216, 3401, 3402, 3403, 3404, 3405, 3406, 3407,
      3408, 3409, 3410, 3411, 3412, 3413, 3414, 3415,
      3416, 3417, 3418, 3419, 3420, 3421, 3422, 3423,
      3424, 3425, 3426, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 4179, 3429,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3427, 4164, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      3249, 4119,  910,	 272, UNDE, 4126, 4107, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      3229, 2233, 0, 0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4120,
      ACC1, ACC1, ACC1, ACC1, 3330, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, 3331,	 727,
      3515, ACC1, ACC1, ACC1, 3332, ACC1, UNDE, 3428,
      ACC0, ACC0, ACC0, ACC0, 3430, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, 3431, 2237,
      3615, ACC0, ACC0, ACC0, 3432, ACC0, UNDE, ACC0,
    },
    4, 1, false, true, true
  },
  {
    "HersheyGothicItalian",	/* #17 */
    "HersheyGothic-Italian",
    "Gothic Italian",
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3249, 3214, 3228, 3232, 3219, 3233, 3218, 3217,
      3221, 3222, 3223, 3225, 3211, 3224, 3210, 3220,
      3200, 3201, 3202, 3203, 3204, 3205, 3206, 3207,
      3208, 3209, 3212, 3213, 3230, 3226, 3231, 3215,
      3234, 3801, 3802, 3803, 3804, 3805, 3806, 3807,
      3808, 3809, 3810, 3811, 3812, 3813, 3814, 3815,
      3816, 3817, 3818, 3819, 3820, 3821, 3822, 3823,
      3824, 3825, 3826, 2223, 4178, 2224, 4110, 4013,
      3216, 3901, 3902, 3903, 3904, 3905, 3906, 3907,
      3908, 3909, 3910, 3911, 3912, 3913, 3914, 3915,
      3916, 3917, 3918, 3919, 3920, 3921, 3922, 3923,
      3924, 3925, 3926, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible to the user.  We store
	 ligatures, accents, and variant glyphs there. */
      0, 0, 0, 0, 0, 0, 0, 0,
      4180, 4181, 4182, 4183, 4184, 4185, 4186, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      3919, 4166, 0, 0, 0, 0, 0, 0,
      /* ISO-Latin-1 encoding resumes */
      3249, 4119,  910,	 272, UNDE, 4126, 4107, 2276,
      4182,  274, 0, UNDE, 4080, 4105,	273, 4187,
      3229, 2233, 0, 0, 4180, 3138, UNDE, 4131,
      CEDILLA, 0, 0, UNDE, 270,	 261,  271, 4120,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,    0, ACC0,
      ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,
      UNDE, ACC1, ACC1, ACC1, ACC1, ACC1, ACC1,	 727,
      3815, ACC1, ACC1, ACC1, ACC1, ACC1, UNDE,	   0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,    0, ACC0,
      ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0,
      UNDE, ACC0, ACC0, ACC0, ACC0, ACC0, ACC0, 2237,
      3915, ACC0, ACC0, ACC0, ACC0, ACC0, UNDE, ACC0,
    },
    5, 1, false, true, true
  },
  {
    "HersheySerifSymbol",		/* #18 */
    "HersheySerif-Symbol",
    "Complex Greek",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 4014, 2275, 2279, 2271, 2272,	 282,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
       250, 2027, 2028, 2048, 2030, 2031, 2047, 2029,
      2033, 2035, 2134, 2036, 2037, 2038, 2039, 2041,
      2042, 2034, 2043, 2044, 2045, 2025, 2187, 2050,
      2040, 2049, 2032, 2223,  740, 2224,  738, 4013,
      4009, 2127, 2128, 2148, 2130, 2131, 2186, 2129,
      2133, 2135, 2147, 2136, 2137, 2138, 2139, 2141,
      2142, 2185, 2143, 2144, 2145, 2146, 4083, 2150,
      2140, 2149, 2132, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
      2187, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199, 2046, 2216, 2243, 2220, 2270, 2156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
      2218, 2233, 2217, 2244, 2235, 2245, 2265,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739, 2266,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4176, UNDE, 4174, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4177, UNDE, 4175, UNDE, UNDE, UNDE,	   0
    },
    6, 1, false, false, true
  },
  {
    "HersheySerifSymbol-Oblique",	/* #19 */
    NULL,
    "Complex Greek (obliqued)",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 2214, 4014, 2275, 2279, 2271, 2272,	 282,
      2221, 2222, 2219, 2232, 2211, 2231, 2210, 2220,
      2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207,
      2208, 2209, 2212, 2213, 2241, 2238, 2242, 2215,
       250, 2027, 2028, 2048, 2030, 2031, 2047, 2029,
      2033, 2035, 2134, 2036, 2037, 2038, 2039, 2041,
      2042, 2034, 2043, 2044, 2045, 2025, 2187, 2050,
      2040, 2049, 2032, 2223,  740, 2224,  738, 4013,
      4009, 2127, 2128, 2148, 2130, 2131, 2186, 2129,
      2133, 2135, 2147, 2136, 2137, 2138, 2139, 2141,
      2142, 2185, 2143, 2144, 2145, 2146, 4083, 2150,
      2140, 2149, 2132, 2225, 2229, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
      2187, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199, 2046, 2216, 2243, 2220, 2270, 2156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
      2218, 2233, 2217, 2244, 2235, 2245, 2265,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739, 2266,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4176, UNDE, 4174, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4177, UNDE, 4175, UNDE, UNDE, UNDE,	   0
    },
    6, 2, true, false, true
  },
  {
    "HersheySerifSymbol-Bold",	/* #20 */
    NULL,
    "Triplex Greek",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 3214, 4014, 3232, 2279, 3233, 3219,	 282,
      3221, 3222, 3223, 3225, 3211, 3224, 3210, 3220,
      3200, 3201, 3202, 3203, 3204, 3205, 3206, 3207,
      3208, 3209, 3212, 3213, 3230, 3226, 3231, 3215,
       250, 3027, 3028, 3048, 3030, 3031, 3047, 3029,
      3033, 3035, 3134, 3036, 3037, 3038, 3039, 3041,
      3042, 3034, 3043, 3044, 3045, 3025, 3187, 3050,
      3040, 3049, 3032, 2223,  740, 2224,  738, 4013,
      4009, 3127, 3128, 3148, 3130, 3131, 3186, 3129,
      3133, 3135, 3147, 3136, 3137, 3138, 3139, 3141,
      3142, 3185, 3143, 3144, 3145, 3146, 3150, 3150,
      3140, 3149, 3132, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
      3187, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199, 3046, 3227, 2243, 3220, 2270, 3156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
      3218, 2233, 3228, 2244, 2235, 2245, 3183,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739, 3083,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4176, UNDE, 4174, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4177, UNDE, 4175, UNDE, UNDE, UNDE,	   0
    },
    6, 3, false, false, true
  },
  {
    "HersheySerifSymbol-BoldOblique", /* #21 */
    NULL,
    "Triplex Greek (obliqued)",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199, 3214, 4014, 3232, 2279, 3233, 3219,	 282,
      3221, 3222, 3223, 3225, 3211, 3224, 3210, 3220,
      3200, 3201, 3202, 3203, 3204, 3205, 3206, 3207,
      3208, 3209, 3212, 3213, 3230, 3226, 3231, 3215,
       250, 3027, 3028, 3048, 3030, 3031, 3047, 3029,
      3033, 3035, 3134, 3036, 3037, 3038, 3039, 3041,
      3042, 3034, 3043, 3044, 3045, 3025, 3187, 3050,
      3040, 3049, 3032, 2223,  740, 2224,  738, 4013,
      4009, 3127, 3128, 3148, 3130, 3131, 3186, 3129,
      3133, 3135, 3147, 3136, 3137, 3138, 3139, 3141,
      3142, 3185, 3143, 3144, 3145, 3146, 3150, 3150,
      3140, 3149, 3132, 2225, 4108, 2226, 2246, 0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
      3187, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199, 3046, 3227, 2243, 3220, 2270, 3156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
      3218, 2233, 3228, 2244, 2235, 2245, 3183,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739, 3083,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4176, UNDE, 4174, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4177, UNDE, 4175, UNDE, UNDE, UNDE,	   0
    },
    6, 4, true, false, true
  },
  {
    "HersheySansSymbol",		/* #22 */
    "HersheySans-Symbol",
    "Simplex Greek",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199,  714, 4014,	 733, 2279,  697,  734,	 282,
       721,  722,  728,	 725,  711,  724,  710,	 720,
       700,  701,  702,	 703,  704,  705,  706,	 707,
       708,  709,  712,	 713, 2241,  726, 2242,	 715,
       250,  527,  528,	 548,  530,  531,  547,	 529,
       533,  535,  534,	 536,  537,  538,  539,	 541,
       542,  534,  543,	 544,  545,  525,  687,	 550,
       540,  549,  532,	 693,  740,  694,  738, 4013,
      4009,  627,  628,	 648,  630,  631,  686,	 629,
       633,  635,  647,	 636,  637,  638,  639,	 641,
       642,  685,  643,	 644,  645,  646,  650,	 650,
       640,  649,  632,	 695, 2229,  696, 2246,	   0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
       687, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199,  546,  716, 2243,  720,  736, 2156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
       718, 2233,  717, 2244, 2235, 2245,  683,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739,  583,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4172, UNDE, 4170, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4173, UNDE, 4171, UNDE, UNDE, UNDE,	   0
    },
    7, 1, false, false, true
  },
  {
    "HersheySansSymbol-Oblique",	/* #23 */
    NULL,
    "Simplex Greek (obliqued)",
    {
      /* The range 00..037 isn't accessible except through macros, since in
	 the standard symbol encoding, it's empty.  We store misc. non-font
	 characters (e.g., Hershey zodiacal signs) in this range. */
	 0, 2301, 2302, 2303, 2304, 2305, 2306, 2307,
      2308, 2309, 2310, 2311, 2312,    0,    0,	   0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      2199,  714, 4014,	 733, 2279,  697,  734,	 282,
       721,  722,  728,	 725,  711,  724,  710,	 720,
       700,  701,  702,	 703,  704,  705,  706,	 707,
       708,  709,  712,	 713, 2241,  726, 2242,	 715,
       250,  527,  528,	 548,  530,  531,  547,	 529,
       533,  535,  534,	 536,  537,  538,  539,	 541,
       542,  534,  543,	 544,  545,  525,  687,	 550,
       540,  549,  532,	 693,  740,  694,  738, 4013,
      4009,  627,  628,	 648,  630,  631,  686,	 629,
       633,  635,  647,	 636,  637,  638,  639,	 641,
       642,  685,  643,	 644,  645,  646,  650,	 650,
       640,  649,  632,	 695, 2229,  696, 2246,	   0,
      /* The range 0200--0237 isn't accessible except through macros, since
	 in the standard symbol encoding, it's empty.  We store
	 misc. non-font characters (e.g., Hershey astronomical symbols) in
	 this range. */
	 0,    0,    0,	   0, 4019, 4020, 4021, 4022,
      UNDE, 2281, 2282, 2283, 2284, 2285, 2286, 2287,
      2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
       687, 2277, 2278, 2269, 2234, 2230, 4008, 4012,
      /* symbol encoding resumes... */
      2199,  546,  716, 2243,  720,  736, 2156,	 744,
       743,  742,  741, 4045, 2263, 2262, 2261, 2264,
       718, 2233,  717, 2244, 2235, 2245,  683,	 828,
      2237, 2239, 2240,	 248, 4070, 4044, 4051, 4065,
      2077, 3309, 3318, 2190, 4003, 4004, 4071, 2259,
      2257, 2258, 4011, 4082, 2256, 4010, 2260, 4007,
       739,  583,  273,	 274, 4098, 2401, 2267, 2236,
      4080, 4078, 4081, 4046, 4048, 4050, 4049, 4047,
       743, 2227,  273,	 274, 4097, 2402, UNDE, UNDE,
      UNDE, 4172, UNDE, 4170, UNDE, UNDE, UNDE, UNDE,
      UNDE, 2228, 2268, UNDE, UNDE, UNDE, UNDE, UNDE,
      UNDE, 4173, UNDE, 4171, UNDE, UNDE, UNDE,	   0
    },
    7, 2, true, false, true
  },
  {
    NULL,			/* DUMMY */
    NULL,
    NULL,
    {
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    },
    0, 0, false, false, false
  }
};

/* Accented character table, used (1) by ISO-Latin-1 Hershey fonts, and (2)
   by HersheyCyrillic and HersheyCyrillic-Oblique.  (Really the union of
   three non-conflicting tables.)  This table maps each accented character
   to a base character and an accent.

   The indices of the accent glyphs, in the glyph table in g_her_glyph.c,
   are stored in the inaccessible 0x80--0x9f region (i.e., \0200--\0237
   region) of each font. */

const struct plHersheyAccentedCharInfoStruct _hershey_accented_char_info[] =
{
  /* for HersheyCyrillic[-Oblique] (KOI8-R encoding) accented characters */
  {0243, 0305, 0212},		/* edieresis */
  {0263, 0345, 0212},		/* Edieresis */
  /* for ISO-Latin-1 accented characters */
  {0300, 'A', 0211},		/* Agrave */
  {0301, 'A', 0210},		/* Aacute */
  {0302, 'A', 0213},		/* Acircumflex */
  {0303, 'A', 0215},		/* Atilde */
  {0304, 'A', 0212},		/* Adieresis */
  {0305, 'A', 0216},		/* Aring */
  {0307, 'C', 0217},		/* Ccedilla */
  {0310, 'E', 0211},		/* Egrave */
  {0311, 'E', 0210},		/* Eacute */
  {0312, 'E', 0213},		/* Ecircumflex */
  {0313, 'E', 0212},		/* Edieresis */
  {0314, 'I', 0210},		/* Igrave */
  {0315, 'I', 0211},		/* Iacute */
  {0316, 'I', 0214},		/* Icircumflex (note narrow circ.) */
  {0317, 'I', 0212},		/* Idieresis */
  {0321, 'N', 0215},		/* Ntilde */
  {0322, 'O', 0211},		/* Ograve */
  {0323, 'O', 0210},		/* Oacute */
  {0324, 'O', 0213},		/* Ocircumflex */
  {0325, 'O', 0215},		/* Otilde */
  {0326, 'O', 0212},		/* Odieresis */
  /* PAUL MURRELL
     Added "Oslash"
  */
  {0330, 'O', 0220},
  {0331, 'U', 0211},		/* Ugrave */
  {0332, 'U', 0210},		/* Uacute */
  {0333, 'U', 0213},		/* Ucircumflex */
  {0334, 'U', 0212},		/* Udieresis */
  {0335, 'Y', 0210},		/* Yacute */
  {0340, 'a', 0211},		/* agrave */
  {0341, 'a', 0210},		/* aacute */
  {0342, 'a', 0214},		/* acircumflex */
  {0343, 'a', 0215},		/* atilde */
  {0344, 'a', 0212},		/* adieresis */
  {0345, 'a', 0216},		/* aring */
  {0347, 'c', 0217},		/* ccedilla */
  {0350, 'e', 0211},		/* egrave */
  {0351, 'e', 0210},		/* eacute */
  {0352, 'e', 0214},		/* ecircumflex */
  {0353, 'e', 0212},		/* edieresis */
  {0354, 0231, 0210},		/* igrave */
  {0355, 0231, 0211},		/* iacute */
  {0356, 0231, 0214},		/* icircumflex (note narrow circ.) */
  {0357, 0231, 0212},		/* idieresis */
  {0361, 'n', 0215},		/* ntilde */
  {0362, 'o', 0211},		/* ograve */
  {0363, 'o', 0210},		/* oacute */
  {0364, 'o', 0214},		/* ocircumflex */
  {0365, 'o', 0215},		/* otilde */
  {0366, 'o', 0212},		/* odieresis */
  /* PAUL MURRELL
     Added "oslash"
  */
  {0370, 'o', 0221},
  {0371, 'u', 0211},		/* ugrave */
  {0372, 'u', 0210},		/* uacute */
  {0373, 'u', 0214},		/* ucircumflex */
  {0374, 'u', 0212},		/* udieresis */
  {0375, 'y', 0210},		/* yacute */
  {0377, 'y', 0212},		/* ydieresis */
  {0, 0, 0}
};

/* known Hershey vector font typefaces.	 Each plTypefaceInfoStruct contains
   the following information:

   (1) number of valid fonts [should be >= 2, since every typeface
       should include a symbol font (the zeroth font, the 1st listed)]
   (2) a list of fonts (each number is an index into
       the _hershey_font_info[] array above).

   The number of valid fonts should be <= FONTS_PER_TYPEFACE; the
   initializers are filled out with dummy fonts to get arrays of length
   FONTS_PER_TYPEFACE. */

const struct plTypefaceInfoStruct _hershey_typeface_info[] =
{
  /* Hershey Serif [including Cyrillic, Cyrillic-Obl., and EUC], typeface #0 */
  { 8, { 18, 0, 1, 2, 3, 4, 5, 8, 999, 999 } },
  /* Hershey Sans, typeface #1 */
  { 5, { 22, 9, 10, 11, 12, 999, 999, 999, 999, 999 } },
  /* Hershey Script [note duplicates], typeface #2 */
  { 5, { 18, 13, 13, 14, 14, 999, 999, 999, 999, 999 } },
  /* Hershey Gothic English, typeface #3 */
  { 2, { 18, 15, 999, 999, 999, 999, 999, 999, 999, 999 } },
  /* Hershey Gothic German, typeface #4 */
  { 2, { 18, 16, 999, 999, 999, 999, 999, 999, 999, 999 } },
  /* Hershey Gothic Italian, typeface #5 */
  { 2, { 18, 17, 999, 999, 999, 999, 999, 999, 999, 999 } },
  /* Hershey Serif Symbol, typeface #6 */
  { 5, { 18, 18, 19, 20, 21, 999, 999, 999, 999, 999 } },
  /* Hershey Sans Symbol, typeface #7 */
  { 3, { 22, 22, 23, 999, 999, 999, 999, 999, 999, 999 } },
};
