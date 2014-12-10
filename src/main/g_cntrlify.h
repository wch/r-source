/* This header file is #include'd by g_cntrlify.c.  It is a database rather
   than a true header file: it includes arrays of escape sequences,
   ligatures, etc., which govern the interpretation of a user-specified
   text string, or `label'.

   This database is used in `controlification': the mapping of a label (a
   string of unsigned chars) to a string of unsigned shorts, for further
   processing.  This mapping accomplishes two things: first, it maps
   certain escape sequences, beginning with a backslash, to non-ASCII
   ISO-Latin-1 characters (e.g., \mu is mapped to `times'), and second, it
   maps certain escape sequences to internal control codes (e.g. \sp is
   mapped to `start superscript'). */

/* Escape sequences that are mapped to internal control codes.  The order
   in which these appear must agree with the order of internal control
   codes (e.g. C_BEGIN_SUBSCRIPT) appearing in g_control.h. */

#define NUM_CONTROLS 18
static const char * const _control_tbl[NUM_CONTROLS] =
{
  /* \sp = start superscript */
  "sp",
  /* \ep = end superscript */
  "ep",
  /* \sb = start subscript */
  "sb",
  /* \eb = end subscript */
  "eb",
  /* \mk = mark location */
  "mk",
  /* \rt = return */
  "rt",
  /* \r1 = shift right by 1 em */
  "r1",
  /* \r2 = shift right by em/2 */
  "r2",
  /* \r4 = shift right by em/4 */
  "r4",
  /* \r6 = shift right by em/6 */
  "r6",
  /* \r8 = shift right by em/8 */
  "r8",
  /* \r^ = shift right by em/12 */
  "r^",
  /* \l1 = shift left by 1 em */
  "l1",
  /* \l2 = shift left by em/2 */
  "l2",
  /* \l4 = shift left by em/4 */
  "l4",
  /* \l6 = shift left by em/6 */
  "l6",
  /* \l8 = shift left by em/8 */
  "l8",
  /* \l^ = shift left by em/12 */
  "l^",
};

typedef struct
{
  unsigned char byte;
  const char * string;
  const char * ps_name;
} Escape;

/* The basic translation table, applying to all ISO-Latin-1 fonts. */

#define NUM_ISO_ESCAPES 95

static const Escape _iso_escape_tbl[NUM_ISO_ESCAPES] =
{
  /* escape sequences, taken from groff */
  {161, "r!", "exclamdown"},
  {162, "ct", "cent"},
  {163, "Po", "sterling"},
  {164, "Cs", "currency"},
  {165, "Ye", "yen"},
  {166, "bb", "brokenbar"},
  {167, "sc", "section"},
  {168, "ad", "dieresis"},
  {169, "co", "copyright"},
  {170, "Of", "ordfeminine"},
  {171, "Fo", "guillemotleft"},
  {172, "no", "logicalnot"},
  {173, "hy", "hyphen"},
  {174, "rg", "registered"},
  {175, "a-", "macron"},
  {176, "de", "degree"},
  {177, "+-", "plusminus"},
  {178, "S2", "twosuperior"},
  {179, "S3", "threesuperior"},
  {180, "aa", "acute"},
  {181, "*m", "mu"},
  {182, "ps", "paragraph"},
  {183, "md", "periodcentered"},
  {184, "ac", "cedilla"},
  {185, "S1", "onesuperior"},
  {186, "Om", "ordmasculine"},
  {187, "Fc", "guillemotright"},
  {188, "14", "onequarter"},
  {189, "12", "onehalf"},
  {190, "34", "threequarters"},
  {191, "r?", "questiondown"},
  {192, "`A", "Agrave"},
  {193, "'A", "Aacute"},
  {194, "^A", "Acircumflex"},
  {195, "~A", "Atilde"},
  {196, ":A", "Adieresis"},
  {197, "oA", "Aring"},
  {198, "AE", "AE"},
  {199, ",C", "Ccedilla"},
  {200, "`E", "Egrave"},
  {201, "'E", "Eacute"},
  {202, "^E", "Ecircumflex"},
  {203, ":E", "Edieresis"},
  {204, "`I", "Igrave"},
  {205, "'I", "Iacute"},
  {206, "^I", "Icircumflex"},
  {207, ":I", "Idieresis"},
  {208, "-D", "Eth"},
  {209, "~N", "Ntilde"},
  {210, "`O", "Ograve"},
  {211, "'O", "Oacute"},
  {212, "^O", "Ocircumflex"},
  {213, "~O", "Otilde"},
  {214, ":O", "Odieresis"},
  {215, "mu", "multiply"},
  {216, "/O", "Oslash"},
  {217, "`U", "Ugrave"},
  {218, "'U", "Uacute"},
  {219, "^U", "Ucircumflex"},
  {220, ":U", "Udieresis"},
  {221, "'Y", "Yacute"},
  {222, "TP", "Thorn"},
  {223, "ss", "germandbls"},
  {224, "`a", "agrave"},
  {225, "'a", "aacute"},
  {226, "^a", "acircumflex"},
  {227, "~a", "atilde"},
  {228, ":a", "adieresis"},
  {229, "oa", "aring"},
  {230, "ae", "ae"},
  {231, ",c", "ccedilla"},
  {232, "`e", "egrave"},
  {233, "'e", "eacute"},
  {234, "^e", "ecircumflex"},
  {235, ":e", "edieresis"},
  {236, "`i", "igrave"},
  {237, "'i", "iacute"},
  {238, "^i", "icircumflex"},
  {239, ":i", "idieresis"},
  {240, "Sd", "eth"},
  {241, "~n", "ntilde"},
  {242, "`o", "ograve"},
  {243, "'o", "oacute"},
  {244, "^o", "ocircumflex"},
  {245, "~o", "otilde"},
  {246, ":o", "odieresis"},
  {247, "di", "divide"},
  {248, "/o", "oslash"},
  {249, "`u", "ugrave"},
  {250, "'u", "uacute"},
  {251, "^u", "ucircumflex"},
  {252, ":u", "udieresis"},
  {253, "'y", "yacute"},
  {254, "Tp", "thorn"},
  {255, ":y", "ydieresis"},
};

/* For any standard PS font or Hershey font, the following additional
   translation table applies.  It permits the use of symbols and Greek
   characters (taken from the Symbol font, for a PS font, or from the
   HersheySymbol font, for a Hershey font) without explicitly switching
   fonts. */

#define NUM_SYMBOL_ESCAPES 161

static const Escape _symbol_escape_tbl[NUM_SYMBOL_ESCAPES] =
{
  /* escape sequences, taken from groff, except for (1) several characters
   for which the escape sequences were invented, because groff has no
   2-letter escape sequence for them, and (2) three characters (flagged)
   that needed to have their escape sequences capitalized due to conflicts */
  {042, "fa", "universal"},
  {044, "te", "existential"},
  {047, "st", "suchthat"},
  {052, "**", "asteriskmath"},
  {0100, "=~", "congruent"},
  {0101, "*A", "Alpha"},
  {0102, "*B", "Beta"},
  {0103, "*X", "Chi"},
  {0104, "*D", "Delta"},
  {0105, "*E", "Epsilon"},
  {0106, "*F", "Phi"},
  {0107, "*G", "Gamma"},
  {0110, "*Y", "Eta"},
  {0111, "*I", "Iota"},
  {0112, "+h", "theta1"},
  {0113, "*K", "kappa"},
  {0114, "*L", "Lambda"},
  {0115, "*M", "Mu"},
  {0116, "*N", "Nu"},
  {0117, "*O", "Omicron"},
  {0120, "*P", "Pi"},
  {0121, "*H", "Theta"},
  {0122, "*R", "Rho"},
  {0123, "*S", "Sigma"},
  {0124, "*T", "Tau"},
  {0125, "*U", "Upsilon"},
  {0126, "ts", "sigma1"},
  {0127, "*W", "Omega"},
  {0130, "*C", "Xi"},
  {0131, "*Q", "Psi"},
  {0132, "*Z", "Zeta"},
  {0134, "tf", "therefore"},
  {0136, "pp", "perpendicular"},
  {0137, "ul", "underline"},	/* not the same as underscore */
  {0140, "rx", "radicalex"},
  {0141, "*a", "alpha"},
  {0142, "*b", "beta"},
  {0143, "*x", "chi"},
  {0144, "*d", "delta"},
  {0145, "*e", "epsilon"},
  {0146, "*f", "phi"},
  {0147, "*g", "gamma"},
  {0150, "*y", "eta"},
  {0151, "*i", "iota"},
  {0152, "+f", "phi1"},
  {0153, "*k", "kappa"},
  {0154, "*l", "lambda"},
  {0155, "*m", "mu"},
  {0156, "*n", "nu"},
  {0157, "*o", "omicron"},
  {0160, "*p", "pi"},
  {0161, "*h", "theta"},
  {0162, "*r", "rho"},
  {0163, "*s", "sigma"},
  {0164, "*t", "tau"},
  {0165, "*u", "upsilon"},
  {0166, "+p", "omega1"},
  {0167, "*w", "omega"},
  {0170, "*c", "xi"},
  {0171, "*q", "psi"},
  {0172, "*z", "zeta"},
  {0176, "ap", "similar"},
  {0241, "+U", "Upsilon1"},
  {0242, "fm", "minute"},
  {0243, "<=", "lessequal"},
  {0244, "f/", "fraction"},
  {0245, "if", "infinity"},
  {0246, "Fn", "florin"},
  {0247, "CL", "club"},
  {0250, "DI", "diamond"},
  {0251, "HE", "heart"},
  {0252, "SP", "spade"},
  {0253, "<>", "arrowboth"},
  {0254, "<-", "arrowleft"},
  {0255, "ua", "arrowup"},
  {0256, "->", "arrowright"},
  {0257, "da", "arrowdown"},
  {0260, "de", "degree"},
  {0261, "+-", "plusminus"},
  {0262, "sd", "second"},
  {0263, ">=", "greaterequal"},
  {0264, "mu", "multiply"},
  {0265, "pt", "proportional"},
  {0266, "pd", "partialdiff"},
  {0267, "bu", "bullet"},
  {0270, "di", "divide"},
  {0271, "!=", "notequal"},
  {0272, "==", "equivalence"},
  {0273, "~~", "approxequal"},
  {0274, "..", "ellipsis"},
  {0275, "NO_ABBREV", "arrowvertex"},
  {0276, "an", "arrowhorizex"},
  {0277, "CR", "carriagereturn"},
  {0300, "Ah", "aleph"},
  {0301, "Im", "Ifraktur"},
  {0302, "Re", "Rfraktur"},
  {0303, "wp", "weierstrass"},
  {0304, "c*", "circlemultiply"},
  {0305, "c+", "circleplus"},
  {0306, "es", "emptyset"},
  {0307, "ca", "cap"},
  {0310, "cu", "cup"},
  {0311, "SS", "superset"},	/* groff: sp.  Conflicts with \sp. */
  {0312, "ip", "reflexsuperset"},
  {0313, "n<", "notsubset"},
  {0314, "SB", "subset"},	/* groff: sb.  Conflicts with \sb. */
  {0315, "ib", "reflexsubset"},
  {0316, "mo", "element"},
  {0317, "nm", "notelement"},
  {0320, "/_", "angle"},
  {0321, "gr", "nabla"},
  {0322, "rg", "registerserif"},
  {0323, "co", "copyrightserif"},
  {0324, "tm", "trademarkserif"},
  {0325, "PR", "product"},
  {0326, "sr", "radical"},
  {0327, "md", "dotmath"},
  {0330, "no", "logicalnot"},
  {0331, "AN", "logicaland"},
  {0332, "OR", "logicalor"},
  {0333, "hA", "arrowdblboth"},
  {0334, "lA", "arrowdblleft"},
  {0335, "uA", "arrowdblup"},
  {0336, "rA", "arrowdblright"},
  {0337, "dA", "arrowdbldown"},
  {0340, "lz", "lozenge"},
  {0341, "la", "angleleft"},
  {0342, "RG", "registersans"},
  {0343, "CO", "copyrightsans"},
  {0344, "TM", "trademarksans"},
  {0345, "SU", "summation"},
  {0346, "NO_ABBREV", "parenlefttp"},
  {0347, "NO_ABBREV", "parenleftex"},
  {0350, "NO_ABBREV", "parenleftbt"},
  {0351, "lc", "bracketlefttp"},
  {0352, "NO_ABBREV", "bracketleftex"},
  {0353, "lf", "bracketleftbt"},
  {0354, "lt", "bracelefttp"},
  {0355, "lk", "braceleftmid"},
  {0356, "lb", "braceleftbt"},
  {0357, "bv", "braceex"},
  /* Euro symbol, added by Adobe 1997(?).  ObHistorical note: Back in Apple
     LaserWriter days, the Apple logo was stored in this slot. */
  {0360, "eu", "euro"},
  {0361, "ra", "angleright"},
  {0362, "is", "integral"},
  {0363, "NO_ABBREV", "integraltp"},
  {0364, "NO_ABBREV", "integralex"},
  {0365, "NO_ABBREV", "integralbt"},
  {0366, "NO_ABBREV", "parenrighttp"},
  {0367, "NO_ABBREV", "parenrightex"},
  {0370, "NO_ABBREV", "parenrightbt"},
  {0371, "rc", "bracketrighttp"},
  {0372, "NO_ABBREV", "bracketrightex"},
  {0373, "rf", "bracketrightbt"},
  {0374, "RT", "bracerighttp"},	/* groff: rt.  Conflicts with \rt. */
  {0375, "rk", "bracerightmid"},
  {0376, "rb", "bracerightbt"},
  /* traditional UGS aliases (undocumented and redundant; obsolete) */
  {0100, "~=", "congruent"},
  {0242, "pr", "minute"},
  {0245, "in", "infinity"},
  {0271, "n=", "notequal"},
  {0321, "dl", "nabla"},
};

/* For any Hershey font, the following additional translation table
   applies.  It allows access to special vector characters, from the
   Hershey repertory and from the SLAC UGS [Unified Graphics System]
   repertory, which we've placed in the nonprintable 0200..0237 region of
   each Hershey Symbol font.  That region of each Symbol font, like the
   corresponding region of all other fonts, is normally inaccessible; it is
   accessible only through the following translations.  (The Hershey
   zodiacal glyphs, which were a last-minute addition, are similarly stored
   in the other nonprintable range 000..037 of the Symbol fonts.) */

#define NUM_SPECIAL_ESCAPES 40

static const Escape _special_escape_tbl[NUM_SPECIAL_ESCAPES] =
{
  /* zodiacal Hershey glyphs */
  {01, "AR", "aries"},
  {02, "TA", "taurus"},
  {03, "GE", "gemini"},
  {04, "CA", "cancer"},
  {05, "LE", "leo"},
  {06, "VI", "virgo"},
  {07, "LI", "libra"},
  {010, "SC", "scorpio"},
  {011, "SG", "sagittarius"},
  {012, "CP", "capricornus"},
  {013, "AQ", "aquarius"},
  {014, "PI", "pisces"},
  /* some special UGS characters */
  {0204, "~-", "modifiedcongruent"},
  {0205, "hb", "hbar"},
  {0206, "IB", "interbang"},
  {0207, "Lb", "lambdabar"},
  {0210, "UD", "undefined"},
  /* astronomical Hershey glyphs */
  {0211, "SO", "sun"},
  {0212, "ME", "mercury"},
  {0213, "VE", "venus"},
  {0214, "EA", "earth"},
  {0215, "MA", "mars"},
  {0216, "JU", "jupiter"},
  {0217, "SA", "saturn"},
  {0220, "UR", "uranus"},
  {0221, "NE", "neptune"},
  {0222, "PL", "pluto"},
  {0223, "LU", "moon"},
  {0224, "CT", "comet"},
  {0225, "ST", "star"},
  {0226, "AS", "ascendingnode"},
  {0227, "DE", "descendingnode"},
  /* final `s', treated specially in g_cntrlify.c */
#define FINAL_LOWERCASE_S 0230
  {0230, "s-", "s1"},
  /* non-astronomical Hershey glyphs */
  {0231, "dg", "dagger"},
  {0232, "dd", "daggerdbl"},
  {0233, "li", "line integral"},
  {0234, "-+", "minusplus"},
  {0235, "||", "parallel"},
  /* overbar and underbar (which make sense for all fonts, but which are
     implemented for non-Hershey fonts in a different way) */
  {0236, "rn", "overscore"},
#define VECTOR_SYMBOL_FONT_UNDERSCORE 0237
  {0237, "ul", "underscore"},
};

/* The character-to-superscript mapping table, applying only to ISO-8859-1
   Hershey fonts.  Certain printable 8-bit characters are drawn as small
   raised ASCII characters, possibly underlined. */

typedef struct
{
  unsigned char from, to;
  bool underscored;
} Raiseinfo;

#define NUM_RAISED_CHARS 5

static const Raiseinfo _raised_char_tbl[NUM_RAISED_CHARS] =
{
  {170, 97, true},			/* ordfeminine mapped to 'a' */
  {178, 50, false},			/* twosuperior mapped to '2' */
  {179, 51, false},			/* threesuperior mapped to '3' */
  {185, 49, false},			/* onesuperior mapped to '1' */
  {186, 111, true},			/* ordmasculine mapped to 'o'*/
};

/* The single-character `deligature' table, applying to all ISO-8859-1
   Hershey fonts (since they do not include these ligatures, except for
   germandbls [eszet] in Gothic-German). */

typedef struct
{
  unsigned char from;
  const char * to;
  int except_font;
} Deligature;

#define NUM_DELIGATURED_CHARS 3

static const Deligature _deligature_char_tbl[NUM_DELIGATURED_CHARS] =
{
  {198, "AE", 999},
  {230, "ae", 999},
  {223, "ss", HERSHEY_GOTHIC_GERMAN},  /* no deligature of #223 in G.-German */
};

/* Same as preceding, for escape sequences rather than for 8-bit
   ISO-Latin-1 chars */

typedef struct
{
  const char * from;
  const char * to;
  int except_font;
} Deligature_escape;

#define NUM_DELIGATURED_ESCAPES 3

static const Deligature_escape _deligature_escape_tbl[NUM_DELIGATURED_ESCAPES] =
{
  {"AE", "AE", 999},
  {"ae", "ae", 999},
  {"ss", "ss", HERSHEY_GOTHIC_GERMAN},   /* no deligature of \ss in Gothic-G.*/
};

/* A table of the ligatures present in the Hershey fonts.  Ligaturization
   is automatic; see g_cntrlify.c.  The eszet ligature, found only in
   HersheyGothic-German, is not listed here because it is not constructed
   automatically; the user must request it, with either "\ss" or 'ß'.  The
   table has the longer ligatures first because it is scanned from first to
   last, with strcmp().

   The Hershey fonts are indicated by their index in the internal table of
   fonts, in g_fontdb.c. */

typedef struct
{
  int font;
  const char * from;
  unsigned char byte;
} Ligature;

#define NUM_LIGATURES 22

static const Ligature _ligature_tbl[NUM_LIGATURES] =
{
  {HERSHEY_SERIF, "ffi", 0203},
  {HERSHEY_SERIF, "ffl", 0204},
  {HERSHEY_SERIF, "ff", 0200},
  {HERSHEY_SERIF, "fi", 0201},
  {HERSHEY_SERIF, "fl", 0202},
  {HERSHEY_SERIF_ITALIC, "ffi", 0203},
  {HERSHEY_SERIF_ITALIC, "ffl", 0204},
  {HERSHEY_SERIF_ITALIC, "ff", 0200},
  {HERSHEY_SERIF_ITALIC, "fi", 0201},
  {HERSHEY_SERIF_ITALIC, "fl", 0202},
  {HERSHEY_GOTHIC_GERMAN, "ch", 0206},
  {HERSHEY_GOTHIC_GERMAN, "tz", 0207},
  {HERSHEY_CYRILLIC, "ffi", 0203},
  {HERSHEY_CYRILLIC, "ffl", 0204},
  {HERSHEY_CYRILLIC, "ff", 0200},
  {HERSHEY_CYRILLIC, "fi", 0201},
  {HERSHEY_CYRILLIC, "fl", 0202},
  {HERSHEY_EUC, "ffi", 0203},
  {HERSHEY_EUC, "ffl", 0204},
  {HERSHEY_EUC, "ff", 0200},
  {HERSHEY_EUC, "fi", 0201},
  {HERSHEY_EUC, "fl", 0202},
};
