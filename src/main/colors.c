/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Mathlib.h"
#include "Graphics.h"

/* This file contains support for color and line textures in graphics. */

/*
 *	Color Specification
 *
 *	Colors are stored internally in integers.  Each integer is
 *	broken into four bytes.  The three least significant bytes
 *	are used to contain levels of red, green and blue.  These
 *	levels are integers in the range [0,255].
 *
 *      Externally, colors are specified either
 *	a) by name, using a large table of color names,
 *	b) by RGB values using a string of the form "#rrggbb"
 *	   where rr, gg and bb are hex integers giving the level
 *	   of red green and blue,
 *	c) as an index into a user setable palette of colors.
 */

	/* Default Color Palette */

char *DefaultPalette[] = {
	"black",
	"red",
	"green3",
	"blue",
	"cyan",
	"magenta",
	"yellow",
	"white",
	NULL
};

	/* The Table of Known Color Names */
	/* Adapted from the X11 RGB database */
        /* Note: the color "white" is moved */
	/* to the top of the database to avoid */
	/* its being known as "gray100" */

static int ColorDataBaseSize;

static struct {
	char *name;	/* X11 Color Name */
	char *rgb;	/* #RRGGBB String */
	unsigned code;	/* Internal R Color Code */
}
ColorDataBase[] = {
	{"white",		"#FFFFFF",	0},
	{"aliceblue",		"#F0F8FF",	0},
	{"antiquewhite",	"#FAEBD7",	0},
	{"antiquewhite1",	"#FFEFDB",	0},
	{"antiquewhite2",	"#EEDFCC",	0},
	{"antiquewhite3",	"#CDC0B0",	0},
	{"antiquewhite4",	"#8B8378",	0},
	{"aquamarine",		"#7FFFD4",	0},
	{"aquamarine1",		"#7FFFD4",	0},
	{"aquamarine2",		"#76EEC6",	0},
	{"aquamarine3",		"#66CDAA",	0},
	{"aquamarine4",		"#458B74",	0},
	{"azure",		"#F0FFFF",	0},
	{"azure1",		"#F0FFFF",	0},
	{"azure2",		"#E0EEEE",	0},
	{"azure3",		"#C1CDCD",	0},
	{"azure4",		"#838B8B",	0},
	{"beige",		"#F5F5DC",	0},
	{"bisque",		"#FFE4C4",	0},
	{"bisque1",		"#FFE4C4",	0},
	{"bisque2",		"#EED5B7",	0},
	{"bisque3",		"#CDB79E",	0},
	{"bisque4",		"#8B7D6B",	0},
	{"black",		"#000000",	0},
	{"blanchedalmond",	"#FFEBCD",	0},
	{"blue",		"#0000FF",	0},
	{"blue1",		"#0000FF",	0},
	{"blue2",		"#0000EE",	0},
	{"blue3",		"#0000CD",	0},
	{"blue4",		"#00008B",	0},
	{"blueviolet",		"#8A2BE2",	0},
	{"brown",		"#A52A2A",	0},
	{"brown1",		"#FF4040",	0},
	{"brown2",		"#EE3B3B",	0},
	{"brown3",		"#CD3333",	0},
	{"brown4",		"#8B2323",	0},
	{"burlywood",		"#DEB887",	0},
	{"burlywood1",		"#FFD39B",	0},
	{"burlywood2",		"#EEC591",	0},
	{"burlywood3",		"#CDAA7D",	0},
	{"burlywood4",		"#8B7355",	0},
	{"cadetblue",		"#5F9EA0",	0},
	{"cadetblue1",		"#98F5FF",	0},
	{"cadetblue2",		"#8EE5EE",	0},
	{"cadetblue3",		"#7AC5CD",	0},
	{"cadetblue4",		"#53868B",	0},
	{"chartreuse",		"#7FFF00",	0},
	{"chartreuse1",		"#7FFF00",	0},
	{"chartreuse2",		"#76EE00",	0},
	{"chartreuse3",		"#66CD00",	0},
	{"chartreuse4",		"#458B00",	0},
	{"chocolate",		"#D2691E",	0},
	{"chocolate1",		"#FF7F24",	0},
	{"chocolate2",		"#EE7621",	0},
	{"chocolate3",		"#CD661D",	0},
	{"chocolate4",		"#8B4513",	0},
	{"coral",		"#FF7F50",	0},
	{"coral1",		"#FF7256",	0},
	{"coral2",		"#EE6A50",	0},
	{"coral3",		"#CD5B45",	0},
	{"coral4",		"#8B3E2F",	0},
	{"cornflowerblue",	"#6495ED",	0},
	{"cornsilk",		"#FFF8DC",	0},
	{"cornsilk1",		"#FFF8DC",	0},
	{"cornsilk2",		"#EEE8CD",	0},
	{"cornsilk3",		"#CDC8B1",	0},
	{"cornsilk4",		"#8B8878",	0},
	{"cyan",		"#00FFFF",	0},
	{"cyan1",		"#00FFFF",	0},
	{"cyan2",		"#00EEEE",	0},
	{"cyan3",		"#00CDCD",	0},
	{"cyan4",		"#008B8B",	0},
	{"darkblue",		"#00008B",	0},
	{"darkcyan",		"#008B8B",	0},
	{"darkgoldenrod",	"#B8860B",	0},
	{"darkgoldenrod1",	"#FFB90F",	0},
	{"darkgoldenrod2",	"#EEAD0E",	0},
	{"darkgoldenrod3",	"#CD950C",	0},
	{"darkgoldenrod4",	"#8B6508",	0},
	{"darkgray",		"#A9A9A9",	0},
	{"darkgreen",		"#006400",	0},
	{"darkgrey",		"#A9A9A9",	0},
	{"darkkhaki",		"#BDB76B",	0},
	{"darkmagenta",		"#8B008B",	0},
	{"darkolivegreen",	"#556B2F",	0},
	{"darkolivegreen1",	"#CAFF70",	0},
	{"darkolivegreen2",	"#BCEE68",	0},
	{"darkolivegreen3",	"#A2CD5A",	0},
	{"darkolivegreen4",	"#6E8B3D",	0},
	{"darkorange",		"#FF8C00",	0},
	{"darkorange1",		"#FF7F00",	0},
	{"darkorange2",		"#EE7600",	0},
	{"darkorange3",		"#CD6600",	0},
	{"darkorange4",		"#8B4500",	0},
	{"darkorchid",		"#9932CC",	0},
	{"darkorchid1",		"#BF3EFF",	0},
	{"darkorchid2",		"#B23AEE",	0},
	{"darkorchid3",		"#9A32CD",	0},
	{"darkorchid4",		"#68228B",	0},
	{"darkred",		"#8B0000",	0},
	{"darksalmon",		"#E9967A",	0},
	{"darkseagreen",	"#8FBC8F",	0},
	{"darkseagreen1",	"#C1FFC1",	0},
	{"darkseagreen2",	"#B4EEB4",	0},
	{"darkseagreen3",	"#9BCD9B",	0},
	{"darkseagreen4",	"#698B69",	0},
	{"darkslateblue",	"#483D8B",	0},
	{"darkslategray",	"#2F4F4F",	0},
	{"darkslategray1",	"#97FFFF",	0},
	{"darkslategray2",	"#8DEEEE",	0},
	{"darkslategray3",	"#79CDCD",	0},
	{"darkslategray4",	"#528B8B",	0},
	{"darkslategrey",	"#2F4F4F",	0},
	{"darkturquoise",	"#00CED1",	0},
	{"darkviolet",		"#9400D3",	0},
	{"deeppink",		"#FF1493",	0},
	{"deeppink1",		"#FF1493",	0},
	{"deeppink2",		"#EE1289",	0},
	{"deeppink3",		"#CD1076",	0},
	{"deeppink4",		"#8B0A50",	0},
	{"deepskyblue",		"#00BFFF",	0},
	{"deepskyblue1",	"#00BFFF",	0},
	{"deepskyblue2",	"#00B2EE",	0},
	{"deepskyblue3",	"#009ACD",	0},
	{"deepskyblue4",	"#00688B",	0},
	{"dimgray",		"#696969",	0},
	{"dimgrey",		"#696969",	0},
	{"dodgerblue",		"#1E90FF",	0},
	{"dodgerblue1",		"#1E90FF",	0},
	{"dodgerblue2",		"#1C86EE",	0},
	{"dodgerblue3",		"#1874CD",	0},
	{"dodgerblue4",		"#104E8B",	0},
	{"firebrick",		"#B22222",	0},
	{"firebrick1",		"#FF3030",	0},
	{"firebrick2",		"#EE2C2C",	0},
	{"firebrick3",		"#CD2626",	0},
	{"firebrick4",		"#8B1A1A",	0},
	{"floralwhite",		"#FFFAF0",	0},
	{"forestgreen",		"#228B22",	0},
	{"gainsboro",		"#DCDCDC",	0},
	{"ghostwhite",		"#F8F8FF",	0},
	{"gold",		"#FFD700",	0},
	{"gold1",		"#FFD700",	0},
	{"gold2",		"#EEC900",	0},
	{"gold3",		"#CDAD00",	0},
	{"gold4",		"#8B7500",	0},
	{"goldenrod",		"#DAA520",	0},
	{"goldenrod1",		"#FFC125",	0},
	{"goldenrod2",		"#EEB422",	0},
	{"goldenrod3",		"#CD9B1D",	0},
	{"goldenrod4",		"#8B6914",	0},
	{"gray",		"#BEBEBE",	0},
	{"gray0",		"#000000",	0},
	{"gray1",		"#030303",	0},
	{"gray2",		"#050505",	0},
	{"gray3",		"#080808",	0},
	{"gray4",		"#0A0A0A",	0},
	{"gray5",		"#0D0D0D",	0},
	{"gray6",		"#0F0F0F",	0},
	{"gray7",		"#121212",	0},
	{"gray8",		"#141414",	0},
	{"gray9",		"#171717",	0},
	{"gray10",		"#1A1A1A",	0},
	{"gray11",		"#1C1C1C",	0},
	{"gray12",		"#1F1F1F",	0},
	{"gray13",		"#212121",	0},
	{"gray14",		"#242424",	0},
	{"gray15",		"#262626",	0},
	{"gray16",		"#292929",	0},
	{"gray17",		"#2B2B2B",	0},
	{"gray18",		"#2E2E2E",	0},
	{"gray19",		"#303030",	0},
	{"gray20",		"#333333",	0},
	{"gray21",		"#363636",	0},
	{"gray22",		"#383838",	0},
	{"gray23",		"#3B3B3B",	0},
	{"gray24",		"#3D3D3D",	0},
	{"gray25",		"#404040",	0},
	{"gray26",		"#424242",	0},
	{"gray27",		"#454545",	0},
	{"gray28",		"#474747",	0},
	{"gray29",		"#4A4A4A",	0},
	{"gray30",		"#4D4D4D",	0},
	{"gray31",		"#4F4F4F",	0},
	{"gray32",		"#525252",	0},
	{"gray33",		"#545454",	0},
	{"gray34",		"#575757",	0},
	{"gray35",		"#595959",	0},
	{"gray36",		"#5C5C5C",	0},
	{"gray37",		"#5E5E5E",	0},
	{"gray38",		"#616161",	0},
	{"gray39",		"#636363",	0},
	{"gray40",		"#666666",	0},
	{"gray41",		"#696969",	0},
	{"gray42",		"#6B6B6B",	0},
	{"gray43",		"#6E6E6E",	0},
	{"gray44",		"#707070",	0},
	{"gray45",		"#737373",	0},
	{"gray46",		"#757575",	0},
	{"gray47",		"#787878",	0},
	{"gray48",		"#7A7A7A",	0},
	{"gray49",		"#7D7D7D",	0},
	{"gray50",		"#7F7F7F",	0},
	{"gray51",		"#828282",	0},
	{"gray52",		"#858585",	0},
	{"gray53",		"#878787",	0},
	{"gray54",		"#8A8A8A",	0},
	{"gray55",		"#8C8C8C",	0},
	{"gray56",		"#8F8F8F",	0},
	{"gray57",		"#919191",	0},
	{"gray58",		"#949494",	0},
	{"gray59",		"#969696",	0},
	{"gray60",		"#999999",	0},
	{"gray61",		"#9C9C9C",	0},
	{"gray62",		"#9E9E9E",	0},
	{"gray63",		"#A1A1A1",	0},
	{"gray64",		"#A3A3A3",	0},
	{"gray65",		"#A6A6A6",	0},
	{"gray66",		"#A8A8A8",	0},
	{"gray67",		"#ABABAB",	0},
	{"gray68",		"#ADADAD",	0},
	{"gray69",		"#B0B0B0",	0},
	{"gray70",		"#B3B3B3",	0},
	{"gray71",		"#B5B5B5",	0},
	{"gray72",		"#B8B8B8",	0},
	{"gray73",		"#BABABA",	0},
	{"gray74",		"#BDBDBD",	0},
	{"gray75",		"#BFBFBF",	0},
	{"gray76",		"#C2C2C2",	0},
	{"gray77",		"#C4C4C4",	0},
	{"gray78",		"#C7C7C7",	0},
	{"gray79",		"#C9C9C9",	0},
	{"gray80",		"#CCCCCC",	0},
	{"gray81",		"#CFCFCF",	0},
	{"gray82",		"#D1D1D1",	0},
	{"gray83",		"#D4D4D4",	0},
	{"gray84",		"#D6D6D6",	0},
	{"gray85",		"#D9D9D9",	0},
	{"gray86",		"#DBDBDB",	0},
	{"gray87",		"#DEDEDE",	0},
	{"gray88",		"#E0E0E0",	0},
	{"gray89",		"#E3E3E3",	0},
	{"gray90",		"#E5E5E5",	0},
	{"gray91",		"#E8E8E8",	0},
	{"gray92",		"#EBEBEB",	0},
	{"gray93",		"#EDEDED",	0},
	{"gray94",		"#F0F0F0",	0},
	{"gray95",		"#F2F2F2",	0},
	{"gray96",		"#F5F5F5",	0},
	{"gray97",		"#F7F7F7",	0},
	{"gray98",		"#FAFAFA",	0},
	{"gray99",		"#FCFCFC",	0},
	{"gray100",		"#FFFFFF",	0},
	{"green",		"#00FF00",	0},
	{"green1",		"#00FF00",	0},
	{"green2",		"#00EE00",	0},
	{"green3",		"#00CD00",	0},
	{"green4",		"#008B00",	0},
	{"greenyellow",		"#ADFF2F",	0},
	{"grey",		"#BEBEBE",	0},
	{"grey0",		"#000000",	0},
	{"grey1",		"#030303",	0},
	{"grey2",		"#050505",	0},
	{"grey3",		"#080808",	0},
	{"grey4",		"#0A0A0A",	0},
	{"grey5",		"#0D0D0D",	0},
	{"grey6",		"#0F0F0F",	0},
	{"grey7",		"#121212",	0},
	{"grey8",		"#141414",	0},
	{"grey9",		"#171717",	0},
	{"grey10",		"#1A1A1A",	0},
	{"grey11",		"#1C1C1C",	0},
	{"grey12",		"#1F1F1F",	0},
	{"grey13",		"#212121",	0},
	{"grey14",		"#242424",	0},
	{"grey15",		"#262626",	0},
	{"grey16",		"#292929",	0},
	{"grey17",		"#2B2B2B",	0},
	{"grey18",		"#2E2E2E",	0},
	{"grey19",		"#303030",	0},
	{"grey20",		"#333333",	0},
	{"grey21",		"#363636",	0},
	{"grey22",		"#383838",	0},
	{"grey23",		"#3B3B3B",	0},
	{"grey24",		"#3D3D3D",	0},
	{"grey25",		"#404040",	0},
	{"grey26",		"#424242",	0},
	{"grey27",		"#454545",	0},
	{"grey28",		"#474747",	0},
	{"grey29",		"#4A4A4A",	0},
	{"grey30",		"#4D4D4D",	0},
	{"grey31",		"#4F4F4F",	0},
	{"grey32",		"#525252",	0},
	{"grey33",		"#545454",	0},
	{"grey34",		"#575757",	0},
	{"grey35",		"#595959",	0},
	{"grey36",		"#5C5C5C",	0},
	{"grey37",		"#5E5E5E",	0},
	{"grey38",		"#616161",	0},
	{"grey39",		"#636363",	0},
	{"grey40",		"#666666",	0},
	{"grey41",		"#696969",	0},
	{"grey42",		"#6B6B6B",	0},
	{"grey43",		"#6E6E6E",	0},
	{"grey44",		"#707070",	0},
	{"grey45",		"#737373",	0},
	{"grey46",		"#757575",	0},
	{"grey47",		"#787878",	0},
	{"grey48",		"#7A7A7A",	0},
	{"grey49",		"#7D7D7D",	0},
	{"grey50",		"#7F7F7F",	0},
	{"grey51",		"#828282",	0},
	{"grey52",		"#858585",	0},
	{"grey53",		"#878787",	0},
	{"grey54",		"#8A8A8A",	0},
	{"grey55",		"#8C8C8C",	0},
	{"grey56",		"#8F8F8F",	0},
	{"grey57",		"#919191",	0},
	{"grey58",		"#949494",	0},
	{"grey59",		"#969696",	0},
	{"grey60",		"#999999",	0},
	{"grey61",		"#9C9C9C",	0},
	{"grey62",		"#9E9E9E",	0},
	{"grey63",		"#A1A1A1",	0},
	{"grey64",		"#A3A3A3",	0},
	{"grey65",		"#A6A6A6",	0},
	{"grey66",		"#A8A8A8",	0},
	{"grey67",		"#ABABAB",	0},
	{"grey68",		"#ADADAD",	0},
	{"grey69",		"#B0B0B0",	0},
	{"grey70",		"#B3B3B3",	0},
	{"grey71",		"#B5B5B5",	0},
	{"grey72",		"#B8B8B8",	0},
	{"grey73",		"#BABABA",	0},
	{"grey74",		"#BDBDBD",	0},
	{"grey75",		"#BFBFBF",	0},
	{"grey76",		"#C2C2C2",	0},
	{"grey77",		"#C4C4C4",	0},
	{"grey78",		"#C7C7C7",	0},
	{"grey79",		"#C9C9C9",	0},
	{"grey80",		"#CCCCCC",	0},
	{"grey81",		"#CFCFCF",	0},
	{"grey82",		"#D1D1D1",	0},
	{"grey83",		"#D4D4D4",	0},
	{"grey84",		"#D6D6D6",	0},
	{"grey85",		"#D9D9D9",	0},
	{"grey86",		"#DBDBDB",	0},
	{"grey87",		"#DEDEDE",	0},
	{"grey88",		"#E0E0E0",	0},
	{"grey89",		"#E3E3E3",	0},
	{"grey90",		"#E5E5E5",	0},
	{"grey91",		"#E8E8E8",	0},
	{"grey92",		"#EBEBEB",	0},
	{"grey93",		"#EDEDED",	0},
	{"grey94",		"#F0F0F0",	0},
	{"grey95",		"#F2F2F2",	0},
	{"grey96",		"#F5F5F5",	0},
	{"grey97",		"#F7F7F7",	0},
	{"grey98",		"#FAFAFA",	0},
	{"grey99",		"#FCFCFC",	0},
	{"grey100",		"#FFFFFF",	0},
	{"honeydew",		"#F0FFF0",	0},
	{"honeydew1",		"#F0FFF0",	0},
	{"honeydew2",		"#E0EEE0",	0},
	{"honeydew3",		"#C1CDC1",	0},
	{"honeydew4",		"#838B83",	0},
	{"hotpink",		"#FF69B4",	0},
	{"hotpink1",		"#FF6EB4",	0},
	{"hotpink2",		"#EE6AA7",	0},
	{"hotpink3",		"#CD6090",	0},
	{"hotpink4",		"#8B3A62",	0},
	{"indianred",		"#CD5C5C",	0},
	{"indianred1",		"#FF6A6A",	0},
	{"indianred2",		"#EE6363",	0},
	{"indianred3",		"#CD5555",	0},
	{"indianred4",		"#8B3A3A",	0},
	{"ivory",		"#FFFFF0",	0},
	{"ivory1",		"#FFFFF0",	0},
	{"ivory2",		"#EEEEE0",	0},
	{"ivory3",		"#CDCDC1",	0},
	{"ivory4",		"#8B8B83",	0},
	{"khaki",		"#F0E68C",	0},
	{"khaki1",		"#FFF68F",	0},
	{"khaki2",		"#EEE685",	0},
	{"khaki3",		"#CDC673",	0},
	{"khaki4",		"#8B864E",	0},
	{"lavender",		"#E6E6FA",	0},
	{"lavenderblush",	"#FFF0F5",	0},
	{"lavenderblush1",	"#FFF0F5",	0},
	{"lavenderblush2",	"#EEE0E5",	0},
	{"lavenderblush3",	"#CDC1C5",	0},
	{"lavenderblush4",	"#8B8386",	0},
	{"lawngreen",		"#7CFC00",	0},
	{"lemonchiffon",	"#FFFACD",	0},
	{"lemonchiffon1",	"#FFFACD",	0},
	{"lemonchiffon2",	"#EEE9BF",	0},
	{"lemonchiffon3",	"#CDC9A5",	0},
	{"lemonchiffon4",	"#8B8970",	0},
	{"lightblue",		"#ADD8E6",	0},
	{"lightblue1",		"#BFEFFF",	0},
	{"lightblue2",		"#B2DFEE",	0},
	{"lightblue3",		"#9AC0CD",	0},
	{"lightblue4",		"#68838B",	0},
	{"lightcoral",		"#F08080",	0},
	{"lightcyan",		"#E0FFFF",	0},
	{"lightcyan1",		"#E0FFFF",	0},
	{"lightcyan2",		"#D1EEEE",	0},
	{"lightcyan3",		"#B4CDCD",	0},
	{"lightcyan4",		"#7A8B8B",	0},
	{"lightgoldenrod",	"#EEDD82",	0},
	{"lightgoldenrod1",	"#FFEC8B",	0},
	{"lightgoldenrod2",	"#EEDC82",	0},
	{"lightgoldenrod3",	"#CDBE70",	0},
	{"lightgoldenrod4",	"#8B814C",	0},
	{"lightgoldenrodyellow","#FAFAD2",	0},
	{"lightgray",		"#D3D3D3",	0},
	{"lightgreen",		"#90EE90",	0},
	{"lightgrey",		"#D3D3D3",	0},
	{"lightpink",		"#FFB6C1",	0},
	{"lightpink1",		"#FFAEB9",	0},
	{"lightpink2",		"#EEA2AD",	0},
	{"lightpink3",		"#CD8C95",	0},
	{"lightpink4",		"#8B5F65",	0},
	{"lightsalmon",		"#FFA07A",	0},
	{"lightsalmon1",	"#FFA07A",	0},
	{"lightsalmon2",	"#EE9572",	0},
	{"lightsalmon3",	"#CD8162",	0},
	{"lightsalmon4",	"#8B5742",	0},
	{"lightseagreen",	"#20B2AA",	0},
	{"lightskyblue",	"#87CEFA",	0},
	{"lightskyblue1",	"#B0E2FF",	0},
	{"lightskyblue2",	"#A4D3EE",	0},
	{"lightskyblue3",	"#8DB6CD",	0},
	{"lightskyblue4",	"#607B8B",	0},
	{"lightslateblue",	"#8470FF",	0},
	{"lightslategray",	"#778899",	0},
	{"lightslategrey",	"#778899",	0},
	{"lightsteelblue",	"#B0C4DE",	0},
	{"lightsteelblue1",	"#CAE1FF",	0},
	{"lightsteelblue2",	"#BCD2EE",	0},
	{"lightsteelblue3",	"#A2B5CD",	0},
	{"lightsteelblue4",	"#6E7B8B",	0},
	{"lightyellow",		"#FFFFE0",	0},
	{"lightyellow1",	"#FFFFE0",	0},
	{"lightyellow2",	"#EEEED1",	0},
	{"lightyellow3",	"#CDCDB4",	0},
	{"lightyellow4",	"#8B8B7A",	0},
	{"limegreen",		"#32CD32",	0},
	{"linen",		"#FAF0E6",	0},
	{"magenta",		"#FF00FF",	0},
	{"magenta1",		"#FF00FF",	0},
	{"magenta2",		"#EE00EE",	0},
	{"magenta3",		"#CD00CD",	0},
	{"magenta4",		"#8B008B",	0},
	{"maroon",		"#B03060",	0},
	{"maroon1",		"#FF34B3",	0},
	{"maroon2",		"#EE30A7",	0},
	{"maroon3",		"#CD2990",	0},
	{"maroon4",		"#8B1C62",	0},
	{"mediumaquamarine",	"#66CDAA",	0},
	{"mediumblue",		"#0000CD",	0},
	{"mediumorchid",	"#BA55D3",	0},
	{"mediumorchid1",	"#E066FF",	0},
	{"mediumorchid2",	"#D15FEE",	0},
	{"mediumorchid3",	"#B452CD",	0},
	{"mediumorchid4",	"#7A378B",	0},
	{"mediumpurple",	"#9370DB",	0},
	{"mediumpurple1",	"#AB82FF",	0},
	{"mediumpurple2",	"#9F79EE",	0},
	{"mediumpurple3",	"#8968CD",	0},
	{"mediumpurple4",	"#5D478B",	0},
	{"mediumseagreen",	"#3CB371",	0},
	{"mediumslateblue",	"#7B68EE",	0},
	{"mediumspringgreen",	"#00FA9A",	0},
	{"mediumturquoise",	"#48D1CC",	0},
	{"mediumvioletred",	"#C71585",	0},
	{"midnightblue",	"#191970",	0},
	{"mintcream",		"#F5FFFA",	0},
	{"mistyrose",		"#FFE4E1",	0},
	{"mistyrose1",		"#FFE4E1",	0},
	{"mistyrose2",		"#EED5D2",	0},
	{"mistyrose3",		"#CDB7B5",	0},
	{"mistyrose4",		"#8B7D7B",	0},
	{"moccasin",		"#FFE4B5",	0},
	{"navajowhite",		"#FFDEAD",	0},
	{"navajowhite1",	"#FFDEAD",	0},
	{"navajowhite2",	"#EECFA1",	0},
	{"navajowhite3",	"#CDB38B",	0},
	{"navajowhite4",	"#8B795E",	0},
	{"navy",		"#000080",	0},
	{"navyblue",		"#000080",	0},
	{"oldlace",		"#FDF5E6",	0},
	{"olivedrab",		"#6B8E23",	0},
	{"olivedrab1",		"#C0FF3E",	0},
	{"olivedrab2",		"#B3EE3A",	0},
	{"olivedrab3",		"#9ACD32",	0},
	{"olivedrab4",		"#698B22",	0},
	{"orange",		"#FFA500",	0},
	{"orange1",		"#FFA500",	0},
	{"orange2",		"#EE9A00",	0},
	{"orange3",		"#CD8500",	0},
	{"orange4",		"#8B5A00",	0},
	{"orangered",		"#FF4500",	0},
	{"orangered1",		"#FF4500",	0},
	{"orangered2",		"#EE4000",	0},
	{"orangered3",		"#CD3700",	0},
	{"orangered4",		"#8B2500",	0},
	{"orchid",		"#DA70D6",	0},
	{"orchid1",		"#FF83FA",	0},
	{"orchid2",		"#EE7AE9",	0},
	{"orchid3",		"#CD69C9",	0},
	{"orchid4",		"#8B4789",	0},
	{"palegoldenrod",	"#EEE8AA",	0},
	{"palegreen",		"#98FB98",	0},
	{"palegreen1",		"#9AFF9A",	0},
	{"palegreen2",		"#90EE90",	0},
	{"palegreen3",		"#7CCD7C",	0},
	{"palegreen4",		"#548B54",	0},
	{"paleturquoise",	"#AFEEEE",	0},
	{"paleturquoise1",	"#BBFFFF",	0},
	{"paleturquoise2",	"#AEEEEE",	0},
	{"paleturquoise3",	"#96CDCD",	0},
	{"paleturquoise4",	"#668B8B",	0},
	{"palevioletred",	"#DB7093",	0},
	{"palevioletred1",	"#FF82AB",	0},
	{"palevioletred2",	"#EE799F",	0},
	{"palevioletred3",	"#CD6889",	0},
	{"palevioletred4",	"#8B475D",	0},
	{"papayawhip",		"#FFEFD5",	0},
	{"peachpuff",		"#FFDAB9",	0},
	{"peachpuff1",		"#FFDAB9",	0},
	{"peachpuff2",		"#EECBAD",	0},
	{"peachpuff3",		"#CDAF95",	0},
	{"peachpuff4",		"#8B7765",	0},
	{"peru",		"#CD853F",	0},
	{"pink",		"#FFC0CB",	0},
	{"pink1",		"#FFB5C5",	0},
	{"pink2",		"#EEA9B8",	0},
	{"pink3",		"#CD919E",	0},
	{"pink4",		"#8B636C",	0},
	{"plum",		"#DDA0DD",	0},
	{"plum1",		"#FFBBFF",	0},
	{"plum2",		"#EEAEEE",	0},
	{"plum3",		"#CD96CD",	0},
	{"plum4",		"#8B668B",	0},
	{"powderblue",		"#B0E0E6",	0},
	{"purple",		"#A020F0",	0},
	{"purple1",		"#9B30FF",	0},
	{"purple2",		"#912CEE",	0},
	{"purple3",		"#7D26CD",	0},
	{"purple4",		"#551A8B",	0},
	{"red",			"#FF0000",	0},
	{"red1",		"#FF0000",	0},
	{"red2",		"#EE0000",	0},
	{"red3",		"#CD0000",	0},
	{"red4",		"#8B0000",	0},
	{"rosybrown",		"#BC8F8F",	0},
	{"rosybrown1",		"#FFC1C1",	0},
	{"rosybrown2",		"#EEB4B4",	0},
	{"rosybrown3",		"#CD9B9B",	0},
	{"rosybrown4",		"#8B6969",	0},
	{"royalblue",		"#4169E1",	0},
	{"royalblue1",		"#4876FF",	0},
	{"royalblue2",		"#436EEE",	0},
	{"royalblue3",		"#3A5FCD",	0},
	{"royalblue4",		"#27408B",	0},
	{"saddlebrown",		"#8B4513",	0},
	{"salmon",		"#FA8072",	0},
	{"salmon1",		"#FF8C69",	0},
	{"salmon2",		"#EE8262",	0},
	{"salmon3",		"#CD7054",	0},
	{"salmon4",		"#8B4C39",	0},
	{"sandybrown",		"#F4A460",	0},
	{"seagreen",		"#2E8B57",	0},
	{"seagreen1",		"#54FF9F",	0},
	{"seagreen2",		"#4EEE94",	0},
	{"seagreen3",		"#43CD80",	0},
	{"seagreen4",		"#2E8B57",	0},
	{"seashell",		"#FFF5EE",	0},
	{"seashell1",		"#FFF5EE",	0},
	{"seashell2",		"#EEE5DE",	0},
	{"seashell3",		"#CDC5BF",	0},
	{"seashell4",		"#8B8682",	0},
	{"sienna",		"#A0522D",	0},
	{"sienna1",		"#FF8247",	0},
	{"sienna2",		"#EE7942",	0},
	{"sienna3",		"#CD6839",	0},
	{"sienna4",		"#8B4726",	0},
	{"skyblue",		"#87CEEB",	0},
	{"skyblue1",		"#87CEFF",	0},
	{"skyblue2",		"#7EC0EE",	0},
	{"skyblue3",		"#6CA6CD",	0},
	{"skyblue4",		"#4A708B",	0},
	{"slateblue",		"#6A5ACD",	0},
	{"slateblue1",		"#836FFF",	0},
	{"slateblue2",		"#7A67EE",	0},
	{"slateblue3",		"#6959CD",	0},
	{"slateblue4",		"#473C8B",	0},
	{"slategray",		"#708090",	0},
	{"slategray1",		"#C6E2FF",	0},
	{"slategray2",		"#B9D3EE",	0},
	{"slategray3",		"#9FB6CD",	0},
	{"slategray4",		"#6C7B8B",	0},
	{"slategrey",		"#708090",	0},
	{"snow",		"#FFFAFA",	0},
	{"snow1",		"#FFFAFA",	0},
	{"snow2",		"#EEE9E9",	0},
	{"snow3",		"#CDC9C9",	0},
	{"snow4",		"#8B8989",	0},
	{"springgreen",		"#00FF7F",	0},
	{"springgreen1",	"#00FF7F",	0},
	{"springgreen2",	"#00EE76",	0},
	{"springgreen3",	"#00CD66",	0},
	{"springgreen4",	"#008B45",	0},
	{"steelblue",		"#4682B4",	0},
	{"steelblue1",		"#63B8FF",	0},
	{"steelblue2",		"#5CACEE",	0},
	{"steelblue3",		"#4F94CD",	0},
	{"steelblue4",		"#36648B",	0},
	{"tan",			"#D2B48C",	0},
	{"tan1",		"#FFA54F",	0},
	{"tan2",		"#EE9A49",	0},
	{"tan3",		"#CD853F",	0},
	{"tan4",		"#8B5A2B",	0},
	{"thistle",		"#D8BFD8",	0},
	{"thistle1",		"#FFE1FF",	0},
	{"thistle2",		"#EED2EE",	0},
	{"thistle3",		"#CDB5CD",	0},
	{"thistle4",		"#8B7B8B",	0},
	{"tomato",		"#FF6347",	0},
	{"tomato1",		"#FF6347",	0},
	{"tomato2",		"#EE5C42",	0},
	{"tomato3",		"#CD4F39",	0},
	{"tomato4",		"#8B3626",	0},
	{"turquoise",		"#40E0D0",	0},
	{"turquoise1",		"#00F5FF",	0},
	{"turquoise2",		"#00E5EE",	0},
	{"turquoise3",		"#00C5CD",	0},
	{"turquoise4",		"#00868B",	0},
	{"violet",		"#EE82EE",	0},
	{"violetred",		"#D02090",	0},
	{"violetred1",		"#FF3E96",	0},
	{"violetred2",		"#EE3A8C",	0},
	{"violetred3",		"#CD3278",	0},
	{"violetred4",		"#8B2252",	0},
	{"wheat",		"#F5DEB3",	0},
	{"wheat1",		"#FFE7BA",	0},
	{"wheat2",		"#EED8AE",	0},
	{"wheat3",		"#CDBA96",	0},
	{"wheat4",		"#8B7E66",	0},
	{"whitesmoke",		"#F5F5F5",	0},
	{"yellow",		"#FFFF00",	0},
	{"yellow1",		"#FFFF00",	0},
	{"yellow2",		"#EEEE00",	0},
	{"yellow3",		"#CDCD00",	0},
	{"yellow4",		"#8B8B00",	0},
	{"yellowgreen",		"#9ACD32",	0},
	{NULL,			NULL,		0}
};


#define COLOR_TABLE_SIZE 256

static int ColorTableSize;
static unsigned int ColorTable[COLOR_TABLE_SIZE];

	/* Hex Digit to Integer Conversion */

static unsigned hexdigit(int digit)
{
	if('0' <= digit && digit <= '9') return digit - '0';
	else if('A' <= digit && digit <= 'F') return 10 + digit - 'A';
	else if('a' <= digit && digit <= 'f') return 10 + digit - 'a';
	else error("invalid hex digit in color\n");
}

	/* Integer to Hex Digit */

static char HexDigits[] = "0123456789ABCDEF";

#ifdef UNUSED
static unsigned digithex(int digit)
{
	return HexDigits[abs(digit) % 16];
}
#endif

	/* String Comparison Ignoring Case and Squeezing Out Blanks */

static int StrMatch(char *s, char *t)
{
	for(;;) {
		if(*s == '\0' && *t == '\0') return 1;
		if(*s == ' ') {
			s++; continue;
		}
		if(*t == ' ') {
			t++; continue;
		}
		if(tolower(*s++) != tolower(*t++)) return 0;
	}
}


	/* #RRGGBB String to Internal Color Code */

unsigned int rgb2col(char *rgb)
{
	unsigned int r, g, b;
	if(rgb[0] != '#' || strlen(rgb) != 7)
		error("invalid RGB specification\n");
	r = 16 * hexdigit(rgb[1]) + hexdigit(rgb[2]);
	g = 16 * hexdigit(rgb[3]) + hexdigit(rgb[4]);
	b = 16 * hexdigit(rgb[5]) + hexdigit(rgb[6]);
	return R_RGB(r, g, b);
}

	/* External Color Name to Internal Color Code */

unsigned int name2col(char *nm)
{
	int i;
	for(i=0 ; ColorDataBase[i].name ; i++) {
		if(StrMatch(ColorDataBase[i].name, nm))
			return ColorDataBase[i].code;
	}
	error("invalid color name\n");
}

	/* Index (as string) to Internal Color Code */

unsigned int number2col(char *nm)
{
	int index;
	char *ptr;
	index = strtod(nm, &ptr);
	if(*ptr) error("invalid color specification\n");
	if(index == 0) return DP->bg;
	else return ColorTable[(index-1) % ColorTableSize];
}

static char ColBuf[8];

char *RGB2rgb(unsigned int r, unsigned int g, unsigned int b)
{
	ColBuf[0] = '#';
	ColBuf[1] = HexDigits[(r >> 4) & 15];
	ColBuf[2] = HexDigits[r & 15];
	ColBuf[3] = HexDigits[(g >> 4) & 15];
	ColBuf[4] = HexDigits[g & 15];
	ColBuf[5] = HexDigits[(b >> 4) & 15];
	ColBuf[6] = HexDigits[b & 15];
	ColBuf[7] = '\0';
	return &ColBuf[0];
}

	/* Internal to External Color Representation */
	/* Search the color name database first */
	/* If this fails, create an #RRGGBB string */


char *col2name(unsigned int col)
{
	int i;
	for(i=0 ; ColorDataBase[i].name ; i++) {
		if(col == ColorDataBase[i].code)
			return ColorDataBase[i].name;
	}
	ColBuf[0] = '#';
	ColBuf[1] = HexDigits[(col >>  4) & 15];
	ColBuf[2] = HexDigits[(col      ) & 15];
	ColBuf[3] = HexDigits[(col >> 12) & 15];
	ColBuf[4] = HexDigits[(col >>  8) & 15];
	ColBuf[5] = HexDigits[(col >> 20) & 15];
	ColBuf[6] = HexDigits[(col >> 16) & 15];
	ColBuf[7] = '\0';
	return &ColBuf[0];
}

unsigned str2col(char *s)
{
	if(s[0] == '#') return rgb2col(s);
	else if(isdigit(s[0])) return number2col(s);
	else return name2col(s);
}

static unsigned char2col(char *s)
{
	if(s[0] == '#') return rgb2col(s);
	else return name2col(s);
}

	/* We Assume that Checks Have Been Done */

unsigned RGBpar(SEXP x, int i)
{
	int index;
	if(isString(x)) {
		return str2col(CHAR(STRING(x)[i]));
	}
	else if(isInteger(x) || isLogical(x)) {
		if(INTEGER(x)[i] == NA_INTEGER) return NA_INTEGER;
		index = INTEGER(x)[i] - 1;
		if(index < 0) return DP->bg;
		else return ColorTable[abs(index) % ColorTableSize];
	}
	else if(isReal(x)) {
		if(!FINITE(REAL(x)[i])) return NA_INTEGER;
		index = REAL(x)[i] - 1;
		if(index < 0) return DP->bg;
		else return ColorTable[abs(index) % ColorTableSize];
	}
}

	/* Initialize the Color Databases */

void InitColors()
{
	int i;

	/* Initialize the Color Database */
	for(i=0 ; ColorDataBase[i].name ; i++)
		ColorDataBase[i].code = rgb2col(ColorDataBase[i].rgb);
	ColorDataBaseSize = i;

	/* Install Default Palette */
	for(i=0 ; DefaultPalette[i] ; i++)
		ColorTable[i] = str2col(DefaultPalette[i]);
	ColorTableSize = i;
}

static unsigned int ScaleColor(double x)
{
	if(!FINITE(x) || x < 0.0 || x > 1.0)
		error("invalid color intensity\n");
	return (unsigned int)(255*x);
}

static setpalette(char **palette)
{
	int i;
	for(i=0 ; (i<COLOR_TABLE_SIZE)&&palette[i] ; i++)
		ColorTable[i] = name2col(palette[i]);
	ColorTableSize = i;
}

SEXP do_palette(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP val, ans;
	unsigned int ncols[COLOR_TABLE_SIZE];
	int i, n;
	checkArity(op,args);
	/* Record the current palette */
	PROTECT(ans = allocVector(STRSXP, ColorTableSize));
	for(i=0 ; i<ColorTableSize ; i++)
		STRING(ans)[i] = mkChar(col2name(ColorTable[i]));
	val = CAR(args);
	if(!isString(val)) errorcall(call, "invalid argument type\n");
	if((n=length(val)) == 1) {
		if(StrMatch("default", CHAR(STRING(val)[0])))
			setpalette(DefaultPalette);
		else errorcall(call, "unknown palette\n");
	}
	else if(n > 1) {
		for(i=0 ; i<n ; i++)
			ncols[i] = char2col(CHAR(STRING(val)[i]));
		for(i=0 ; i<n ; i++)
			ColorTable[i] = ncols[i];
		ColorTableSize = n;
	}
	UNPROTECT(1);
	return ans;
}

SEXP do_colors(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans;
	int n;
	n = 0;
	while(ColorDataBase[n].name!=NULL)
		n++;
	ans = allocVector(STRSXP, n);
	n = 0;
	while(ColorDataBase[n].name!=NULL)
		STRING(ans)[n++] = mkChar(ColorDataBase[n].name);
	return ans;
}

SEXP do_hsv(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP c, h, s, v, gm;
	double hh, ss, vv, gg, r, g, b;
	int i, min, max, nh, ns, nv, ng;

	checkArity(op, args);

	PROTECT(h = coerceVector(CAR(args),REALSXP)); args = CDR(args);
	PROTECT(s = coerceVector(CAR(args),REALSXP)); args = CDR(args);
	PROTECT(v = coerceVector(CAR(args),REALSXP)); args = CDR(args);
	PROTECT(gm = coerceVector(CAR(args),REALSXP)); args = CDR(args);

	nh = LENGTH(h);
	ns = LENGTH(s);
	nv = LENGTH(v);
	ng = LENGTH(gm);
	max = nh;
	if(max < ns) max = ns;
	if(max < nv) max = nv;
	if(max < ng) max = ng;
	min = nh;
	if(min > ns) min = ns;
	if(min > nv) min = nv;
	if(min > ng) min = ng;
	if(min <= 0)
		errorcall(call, "invalid argument length\n");

	PROTECT(c = allocVector(STRSXP, max));
	for(i=0 ; i<max ; i++) {
		hh = REAL(h)[i%nh];
		ss = REAL(s)[i%ns];
		vv = REAL(v)[i%nv];
		gg = REAL(gm)[i%ng];
		if(hh < 0 || hh > 1 || ss < 0 || ss > 1 || vv < 0 || vv > 1)
			errorcall(call, "invalid HSV color\n");
		hsv2rgb(hh, ss, vv, &r, &g, &b);
		r = pow(r, gg);
		g = pow(g, gg);
		b = pow(b, gg);
		STRING(c)[i] = mkChar(RGB2rgb(ScaleColor(r),
					ScaleColor(g),
					ScaleColor(b)));
	}
	UNPROTECT(5);
	return c;
}


SEXP do_rgb(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP c, r, g, b, n;
	int i, min, max, nr, ng, nb;
	unsigned int ri, gi, bi;

	checkArity(op, args);

	PROTECT(r = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(g = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(b = coerceVector(CAR(args), REALSXP)); args = CDR(args);
	PROTECT(n = coerceVector(CAR(args), STRSXP)); args = CDR(args);

	nr = LENGTH(r); ng = LENGTH(g); nb = LENGTH(b);
	max = nr; if(max < ng) max = ng; if(max < nb) max = nb;
	min = nr; if(min > ng) min = ng; if(min > nb) min = nb;
	if(min <= 0) errorcall(call, "invalid argument length\n");

	if(length(n) != 0 && length(n) != max)
		errorcall(call, "invalid names vector\n");

	PROTECT(c = allocVector(STRSXP, max));
	for(i=0 ; i<max ; i++) {
		ri = ScaleColor(REAL(r)[i%nr]);
		gi = ScaleColor(REAL(g)[i%ng]);
		bi = ScaleColor(REAL(b)[i%nb]);
		STRING(c)[i] = mkChar(RGB2rgb(ri, gi, bi));
	}
	if(length(n) != 0)
		setAttrib(c, R_NamesSymbol, n);
	UNPROTECT(5);
	return c;
}

SEXP do_gray(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP lev, ans;
	double level;
	int i, ilevel, nlev;

	checkArity(op, args);

	PROTECT(lev = coerceVector(CAR(args),REALSXP));
	nlev = LENGTH(lev);
	PROTECT(ans = allocVector(STRSXP, nlev));
	for(i=0 ; i<nlev ; i++) {
		level = REAL(lev)[i];
		if(!FINITE(level) || level < 0 || level > 1)
			errorcall(call, "invalid gray level\n");
		ilevel = 255 * level;
		STRING(ans)[i] = mkChar(RGB2rgb(ilevel, ilevel, ilevel));
	}
	UNPROTECT(2);
	return ans;
}

/*
 *	LINE TEXTURE SPECIFICATION
 *
 *	Linetypes are stored internally in integers.  An integer
 *	is interpreted as containing a sequence of 8 4-bit integers
 *	which give the lengths of up to 8 on-off line segments.
 *	The lengths are typically interpreted as pixels on a screen
 *	and as "points" in postscript.
 */

typedef struct {
	char *name;
	unsigned int pattern;
} LineTYPE;

static LineTYPE linetype[] = {
	{ "solid",	LTY_SOLID},
	{ "dashed",	LTY_DASHED},
	{ "dotted",	LTY_DOTTED},
	{ "dotdash",	LTY_DOTDASH},
	{ NULL,		0},
};

static int nlinetype = (sizeof(linetype)/sizeof(LineTYPE)-1);

unsigned int LTYpar(SEXP value, int index)
{
	char *p;
	int i, code, shift, digit;

	if(isString(value)) {
		for(i=0 ; linetype[i].name ; i++) {
			if(!strcmp(CHAR(STRING(value)[index]),linetype[i].name))
				return linetype[i].pattern;
		}
		code = 0;
		shift = 0;
		for(p=CHAR(STRING(value)[index]); *p ; p++) {
			digit = hexdigit(*p);
			code = code | (digit<<shift);
			shift = shift + 4;
		}
		return code;
	}
	else if(isInteger(value)) {
		code = INTEGER(value)[index];
		if(code==NA_INTEGER || code <= 0)
			return NA_INTEGER;
		code = (code-1) % nlinetype;
		return linetype[code].pattern;
	}
	else if(isReal(value)) {
		code = REAL(value)[index];
		if(!FINITE(code) || code <= 0)
			return NA_INTEGER;
		code = (code-1) % nlinetype;
		return linetype[code].pattern;
	}
	else error("invalid line type\n");
	/*NOTREACHED*/
}

SEXP LTYget(unsigned int lty)
{
	SEXP ans;
	int i, ndash;
	char dash[8];
	unsigned int l;

	for(i=0 ; linetype[i].name ; i++) {
		if(linetype[i].pattern == lty)
			return mkString(linetype[i].name);
	}

	l = lty; ndash = 0;
	for(i=0; i<8 && l&15 ; i++) {
		dash[ndash++] = l&15;
		l = l>>4;
	}
	PROTECT(ans = allocVector(STRSXP, 1));
	STRING(ans)[0] = allocString(ndash);
	for(i=0 ; i<ndash ; i++) {
		CHAR(STRING(ans)[0])[i] = dash[i] + '0';
	}
	UNPROTECT(1);
	return ans;
}
