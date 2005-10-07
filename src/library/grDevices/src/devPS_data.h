/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005 R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* to match it with afm of CID Japan1,Korea1,CNS1,GB1 */
static char *CIDBoldFontStr = "/%s-Bold\n"
    "/%s /CIDFont findresource\n"
    "16 dict begin\n"
    "  /basecidfont exch def\n"
    "  /basefont-H /.basefont-H /Identity-H [ basecidfont ] composefont def\n"
    "  /basefont-V /.basefont-V /Identity-V [ basecidfont ] composefont def\n"
    "  /CIDFontName dup basecidfont exch get def\n"
    "  /CIDFontType 1 def\n"
    "  /CIDSystemInfo dup basecidfont exch get def\n"
    "  /FontInfo dup basecidfont exch get def\n"
    "  /FontMatrix [ 1 0 0 1 0 0 ] def\n"
    "  /FontBBox [\n"
    "    basecidfont /FontBBox get cvx exec\n"
    "    4 2 roll basecidfont /FontMatrix get transform\n"
    "    4 2 roll basecidfont /FontMatrix get transform\n"
    "  ] def\n"
    "  /cid 2 string def\n"
    "  /BuildGlyph {\n"
    "    gsave\n"
    "    exch begin\n"
    "      dup 256 idiv cid exch 0 exch put\n"
    "      256 mod cid exch 1 exch put\n"
    "      rootfont\n"
    "        /WMode known { rootfont /WMode get 1 eq } { false } ifelse\n"
    "      { basefont-V } { basefont-H } ifelse setfont\n"
    "      .03 setlinewidth 1 setlinejoin\n"
    "      newpath\n"
    "      0 0 moveto cid false charpath stroke\n"
    "      0 0 moveto cid show\n"
    "      currentpoint setcharwidth\n"
    "    end\n"
    "    grestore\n"
    "  } bind def\n"
    "  currentdict\n"
    "end\n"
    "/CIDFont defineresource pop\n";

static const struct {
    char const *cidfamily;
    char const *cidafmfile[4];
    char const *psfontname ;
    char const *pdffontname ;
    char const *cmapname ;
    char const *encoding ;
    char const *pdfresource ;
}CIDResource [] = {
/* ============================================================================
                                         Japan1
   ============================================================================ */
    {"Japan1",
     {"Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
     },
     "Ryumin-Light",
     "Ryumin-Light",
     "EUC-H",
     "EUC-JP",
     "      /FontDescriptor\n"
     "      <<\n"
     "        /Type /FontDescriptor\n"
     "        /CapHeight 709 /Ascent 723 /Descent -241 /StemV 69\n"
     "        /FontBBox [-170 -331 1024 903]\n"
     "        /ItalicAngle 0 /Flags 6\n"
     "        /Style << /Panose <010502020300000000000000> >>\n"
     "      >>\n"
     "      /CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>\n"
     "      /DW 1000\n"
     "      /W [\n"
     "          231   632 500\n"
     "         8718 [500 500]\n"
     "      ]\n"
    },
    {"Japan1Gothic",
     {"Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
      "Adobe-Japan1-UniJIS-UCS2-H.afm",
     },
     "GothicBBB-Medium",
     "GothicBBB-Medium",
     "EUC-H",
     "EUC-JP",
     "      /FontDescriptor\n"
     "      <<\n"
     "        /Type /FontDescriptor\n"
     "        /CapHeight 737 /Ascent 752 /Descent -271 /StemV 99\n"
     "        /FontBBox [-174 -268 1001 944]\n"
     "        /ItalicAngle 0 /Flags 4\n"
     "        /Style << /Panose <0801020b0500000000000000> >>\n"
     "      >>\n"
     "      /CIDSystemInfo << /Registry(Adobe) /Ordering(Japan1) /Supplement  2 >>\n"
     "      /DW 1000\n"
     "      /W [\n"
     "          231   632 500\n"
     "         8718 [500 500]\n"
     "      ]\n"
    },
/* ============================================================================
                                         Korea1
   ============================================================================ */
    {"Korea1",
     {"Adobe-Korea1-UniKS-UCS2-H.afm",
      "Adobe-Korea1-UniKS-UCS2-H.afm",
      "Adobe-Korea1-UniKS-UCS2-H.afm",
      "Adobe-Korea1-UniKS-UCS2-H.afm",
     },
     "Batang-Regular",
     "HYGothic-Medium-Acro",
     "KSCms-UHC-H",
     "CP949",
     "      /FontDescriptor\n"
     "      <<\n"
     "        /Type /FontDescriptor\n"
     "        /CapHeight 737 /Ascent 752 /Descent -271 /StemV 58\n"
     "        /FontBBox [-6 -145 1003 880]\n"
     "        /ItalicAngle 0 /Flags 4 /XHeight 553\n"
     "        /Style << /Panose <000001000600000000000000> >>\n"
     "      >>\n"
     "      /CIDSystemInfo << /Registry(Adobe) /Ordering(Korea1) /Supplement 1 >>\n"
     "      /DW 1000\n"
     "      /W [\n"
     "            1 94 500\n"
     "           97 [500] \n"
     "         8094 8190 500\n"
     "      ]\n"
    },
/* ============================================================================
                                         CNS1
   ============================================================================ */
    {"CNS1",
     {"Adobe-CNS1-UniCNS-UCS2-H.afm",
      "Adobe-CNS1-UniCNS-UCS2-H.afm",
      "Adobe-CNS1-UniCNS-UCS2-H.afm",
      "Adobe-CNS1-UniCNS-UCS2-H.afm",
     },
     "MOESung-Regular",
     "MHei-Medium-Acro",
     "B5-H",
     "CP950",
     "      /FontDescriptor\n"
     "      <<\n"
     "        /Type /FontDescriptor\n"
     "        /CapHeight 737 /Ascent 752 /Descent -271 /StemV 58\n"
     "        /FontBBox [-45 -250 1015 887]\n"
     "        /ItalicAngle 0 /Flags 4 /XHeight 553\n"
     "        /Style << /Panose <000001000600000000000000> >>\n"
     "      >>\n"
     "      /CIDSystemInfo << /Registry(Adobe) /Ordering(CNS1) /Supplement  0 >>\n"
     "      /DW 1000\n"
     "      /W [\n"
     "           13648 13742 500\n"
     "           17603 [500]\n"
     "      ]\n"
    },
/* ============================================================================
                                         GB1
   ============================================================================ */
    {"GB1",
     {"Adobe-GB1-UniGB-UCS2-H.afm",
      "Adobe-GB1-UniGB-UCS2-H.afm",
      "Adobe-GB1-UniGB-UCS2-H.afm",
      "Adobe-GB1-UniGB-UCS2-H.afm",
     },
     "STSong-Light-Acro",
     "GBK-EUC-H",
     "GBK",
     "      /FontDescriptor\n"
     "      <<\n"
     "        /Type /FontDescriptor\n"
     "        /CapHeight 857 /Ascent 857 /Descent -143 /StemV 91\n"
     "        /FontBBox [-250 -143 600 857]\n"
     "        /ItalicAngle 0 /Flags 6 /XHeight 599\n"
     "        /Style << /Panose <000000000400000000000000> >>\n"
     "      >>\n"
     "      /CIDSystemInfo << /Registry(Adobe) /Ordering(GB1) /Supplement  2 >>\n"
     "      /DW 1000\n"
     "      /W [\n"
     "           814 939 500\n"
     "           7716 [500]\n"
     "           22355 [500 500]\n"
     "           22357 [500]\n"
     "      ]\n"
    },
/* ============================================================================
                                         Terminator
   ============================================================================ */
    { NULL }
};

