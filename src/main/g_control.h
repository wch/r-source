/* Control codes (used internally when rendering a label, i.e. a
   user-specified text string).  The header file is #include'd by
   g_cntrlify.c, and g_alabel_her.c (which renders labels in Hershey
   fonts), and by the generic renderer g_alabel.c. */

/* the order of these must agree with the order in g_cntrlify.h */
#define C_BEGIN_SUPERSCRIPT 0
#define C_END_SUPERSCRIPT 1
#define C_BEGIN_SUBSCRIPT 2
#define C_END_SUBSCRIPT 3
#define C_PUSH_LOCATION 4
#define C_POP_LOCATION 5
#define C_RIGHT_ONE_EM 6
#define C_RIGHT_HALF_EM 7
#define C_RIGHT_QUARTER_EM 8
#define C_RIGHT_SIXTH_EM 9
#define C_RIGHT_EIGHTH_EM 10
#define C_RIGHT_TWELFTH_EM 11
#define C_LEFT_ONE_EM 12
#define C_LEFT_HALF_EM 13
#define C_LEFT_QUARTER_EM 14
#define C_LEFT_SIXTH_EM 15
#define C_LEFT_EIGHTH_EM 16
#define C_LEFT_TWELFTH_EM 17

#define C_RIGHT_RADICAL_SHIFT 254	/* for \rn in PS and PCL fonts */
#define C_LEFT_RADICAL_SHIFT 255
#define PS_RADICAL_WIDTH 0.515	/* shifts to make radical, radicalex line up */
#define PCL_RADICAL_WIDTH 0.080
#define RADICALEX 96

/* flags in each unsigned short in a `controlified' text string (mutually
   exclusive) */
#define CONTROL_CODE 0x8000
#define RAW_HERSHEY_GLYPH 0x4000
#define RAW_ORIENTAL_HERSHEY_GLYPH 0x2000

/* masks for extracting, from an unsigned short in a controlified text string,
   (1) the font, if none of the above flags is set, or (2) the raw glyph
   number, if either of the latter two flags is set */
#define ONE_BYTE 0xff
#define FONT_SHIFT 8
#define FONT_SPEC (ONE_BYTE << FONT_SHIFT)
#define GLYPH_SPEC 0x1fff
