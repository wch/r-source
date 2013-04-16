/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-12   The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/*  This file was contributed by Ei-ji Nakama.
 *  It exports locale2charset for use in gram.y, and rlocale.c on MacOS X.
 *  And sysutils.c, grDevices/src/devPS.c
 */

/* setlocale(LC_CTYPE,NULL) to encodingname cf nl_langinfo(LC_CTYPE) */


/*********************************************************************
 * usage : char *locale2charset(const char *locale)                  *
 * return : ASCII - default and undefine                             *
 *          other - encodename                                       *
 *                                                                   *
 *         cc -o localecharset -DDEBUG_TEST=1  localecharset.c       *
 *                                or                                 *
 *         cc -o localecharset -DDEBUG_TEST=2  localecharset.c       *
 *********************************************************************/
#ifdef DEBUG_TEST
#define SPRINT(x) printf("%6d:" #x "=%s\n", __LINE__, x)
#define DPRINT(x) printf("%6d:" #x "=%d\n", __LINE__, x)
#define HAVE_STRING_H
#endif



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <memory.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

//#include <rlocale.h> /* To get the correct linkage for locale2charset */

/* name_value struct */
typedef struct {
    char *name;
    char *value;
} name_value;


/*
 * codeset name defined.
 *
 cat /usr/X11R6/lib/X11/locale/locale.alias | \
 sed -e '/#.*$/d' -e 's/://' | \
 awk '{gsub(/^[^.]+\./, "", $2);
       $2=toupper($2);
       gsub(/^EUC/, "EUC-",$2);
       gsub(/^BIG5HKSCS$/, "BIG5-HKSCS",$2);
       if (($2!="")&&(!system("iconv --list|grep " $2 ))) print $2
       }' | \
       sed -e '/\/$/d' | \
       sort | uniq | \
       awk '{NAME=$1;gsub(/-/,"_",NAME);
	     printf("static  const   char    ENC_%-20s\"%s\";\n",
	     NAME "[]=" ,
	     $1)}'
  */
static  char    ENC_ARMSCII_8[]=        "ARMSCII-8";
static  char    ENC_BIG5[]=             "BIG5";
static  char    ENC_BIG5_HKSCS[]=       "BIG5-HKSCS";
static  char    ENC_C[]=                "C";
static  char    ENC_CP1251[]=           "CP1251";
static  char    ENC_CP1255[]=           "CP1255";
static  char    ENC_CP1256[]=           "CP1256";
static  char    ENC_EUC_CN[]=           "EUC-CN";
static  char    ENC_EUC_JP[]=           "EUC-JP";
static  char    ENC_EUC_KR[]=           "EUC-KR";
static  char    ENC_EUC_TW[]=           "EUC-TW";
static  char    ENC_GB2312[]=           "GB2312";
static  char    ENC_GBK[]=              "GBK";
static  char    ENC_GEORGIAN_ACADEMY[]= "GEORGIAN-ACADEMY";
/* static  char    ENC_GEORGIAN_PS[]=      "GEORGIAN-PS"; */
/* static  char    ENC_ISIRI_3342[]=       "ISIRI-3342"; */
static  char    ENC_ISO8859_1[]=        "ISO8859-1";
static  char    ENC_ISO8859_10[]=       "ISO8859-10";
static  char    ENC_ISO8859_11[]=       "ISO8859-11";
static  char    ENC_ISO8859_13[]=       "ISO8859-13";
/* static  char    ENC_ISO8859_14[]=       "ISO8859-14"; */
static  char    ENC_ISO8859_15[]=       "ISO8859-15";
static  char    ENC_ISO8859_2[]=        "ISO8859-2";
static  char    ENC_ISO8859_3[]=        "ISO8859-3";
/* static  char    ENC_ISO8859_4[]=        "ISO8859-4"; */
static  char    ENC_ISO8859_5[]=        "ISO8859-5";
static  char    ENC_ISO8859_6[]=        "ISO8859-6";
static  char    ENC_ISO8859_7[]=        "ISO8859-7";
static  char    ENC_ISO8859_8[]=        "ISO8859-8";
static  char    ENC_ISO8859_9[]=        "ISO8859-9";
static  char    ENC_KOI8_R[]=           "KOI8-R";
static  char    ENC_KOI8_U[]=           "KOI8-U";
/* static  char    ENC_SJIS[]=             "SJIS"; */
static  char    ENC_TCVN[]=             "TCVN";
/* static  char    ENC_TIS620[]=           "TIS620"; */
static  char    ENC_UTF_8[]=            "UTF-8";
/* static  char    ENC_VISCII[]=           "VISCII"; */

/*
   # charset getscript. iconv list output line is backslant.
 cat /usr/X11R6/lib/X11/locale/locale.alias | \
 sed -e '/#.*$/d ; /^[A-z]*\./d' -e 's/://' | \
 awk '{gsub(/^[^.]+\./, "", $2);
       $2=toupper($2);
       gsub(/^EUC/, "EUC-",$2);
       gsub(/^BIG5HKSCS$/, "BIG5-HKSCS",$2);
       NAME=$2;
       gsub(/\xe7/,"\"\"\\xe7\"\"",$1);
       gsub(/\xe5/,"\"\"\\xe5\"\"",$1);
       gsub(/-/, "_",NAME);
       NAME="ENC_" NAME;
       if (($2!="")&&(!system("iconv --list|grep " $2 ))) print  $1 " " NAME
    }' | \
 sed -e '/\/$/d' | \
 sort -k 1 | uniq | \
 awk '{printf ("    {%-34s%s},\n", "\"" $1 "\",", $2)}'
*/

static const name_value guess[] = {
    {"Cextend",                        ENC_ISO8859_1},
    {"English_United-States.437",      ENC_C},
    {"ISO-8859-1",                     ENC_ISO8859_1},
    {"ISO8859-1",                      ENC_ISO8859_1},
    {"Japanese-EUC",                   ENC_EUC_JP},
    {"Jp_JP",                          ENC_EUC_JP},
    {"POSIX",                          ENC_C},
    {"POSIX-UTF2",                     ENC_C},
    {"aa_DJ",                          ENC_ISO8859_1},
    {"aa_ER",                          ENC_UTF_8},
    {"aa_ER@saaho",                    ENC_UTF_8},
    {"aa_ET",                          ENC_UTF_8},
    {"af",                             ENC_ISO8859_1},
    {"af_ZA",                          ENC_ISO8859_1},
    {"am",                             ENC_UTF_8},
    {"am_ET",                          ENC_UTF_8},
    {"an_ES",                          ENC_ISO8859_15},
    {"ar",                             ENC_ISO8859_6},
    {"ar_AA",                          ENC_ISO8859_6},
    {"ar_AE",                          ENC_ISO8859_6},
    {"ar_BH",                          ENC_ISO8859_6},
    {"ar_DZ",                          ENC_ISO8859_6},
    {"ar_EG",                          ENC_ISO8859_6},
    {"ar_IN",                          ENC_UTF_8},
    {"ar_IQ",                          ENC_ISO8859_6},
    {"ar_JO",                          ENC_ISO8859_6},
    {"ar_KW",                          ENC_ISO8859_6},
    {"ar_LB",                          ENC_ISO8859_6},
    {"ar_LY",                          ENC_ISO8859_6},
    {"ar_MA",                          ENC_ISO8859_6},
    {"ar_OM",                          ENC_ISO8859_6},
    {"ar_QA",                          ENC_ISO8859_6},
    {"ar_SA",                          ENC_ISO8859_6},
    {"ar_SD",                          ENC_ISO8859_6},
    {"ar_SY",                          ENC_ISO8859_6},
    {"ar_TN",                          ENC_ISO8859_6},
    {"ar_YE",                          ENC_ISO8859_6},
    {"be",                             ENC_CP1251},
    {"be_BY",                          ENC_CP1251},
    {"bg",                             ENC_CP1251},
    {"bg_BG",                          ENC_CP1251},
    {"bn_BD",                          ENC_UTF_8},
    {"bn_IN",                          ENC_UTF_8},
    {"bokm""\xe5""l",                  ENC_ISO8859_1},
    {"bokmal",                         ENC_ISO8859_1},
    {"br",                             ENC_ISO8859_1},
    {"br_FR",                          ENC_ISO8859_1},
    {"br_FR@euro",                     ENC_ISO8859_15},
    {"bs_BA",                          ENC_ISO8859_2},
    {"bulgarian",                      ENC_CP1251},
    {"byn_ER",                         ENC_UTF_8},
    {"c-french.iso88591",              ENC_ISO8859_1},
    {"ca",                             ENC_ISO8859_1},
    {"ca_ES",                          ENC_ISO8859_1},
    {"ca_ES@euro",                     ENC_ISO8859_15},
    {"catalan",                        ENC_ISO8859_1},
    {"chinese-s",                      ENC_EUC_CN},
    {"chinese-t",                      ENC_EUC_TW},
    {"croatian",                       ENC_ISO8859_2},
    {"cs",                             ENC_ISO8859_2},
    {"cs_CS",                          ENC_ISO8859_2},
    {"cs_CZ",                          ENC_ISO8859_2},
    {"cy",                             ENC_ISO8859_1},
    {"cy_GB",                          ENC_ISO8859_1},
    {"cz",                             ENC_ISO8859_2},
    {"cz_CZ",                          ENC_ISO8859_2},
    {"czech",                          ENC_ISO8859_2},
    {"da",                             ENC_ISO8859_1},
    {"da_DK",                          ENC_ISO8859_1},
    {"danish",                         ENC_ISO8859_1},
    {"dansk",                          ENC_ISO8859_1},
    {"de",                             ENC_ISO8859_1},
    {"de_AT",                          ENC_ISO8859_1},
    {"de_AT@euro",                     ENC_ISO8859_15},
    {"de_BE",                          ENC_ISO8859_1},
    {"de_BE@euro",                     ENC_ISO8859_15},
    {"de_CH",                          ENC_ISO8859_1},
    {"de_DE",                          ENC_ISO8859_1},
    {"de_DE@euro",                     ENC_ISO8859_15},
    {"de_LI",                          ENC_ISO8859_1},
    {"de_LI@euro",                     ENC_ISO8859_15},
    {"de_LU",                          ENC_ISO8859_1},
    {"de_LU@euro",                     ENC_ISO8859_15},
    {"deutsch",                        ENC_ISO8859_1},
    {"dutch",                          ENC_ISO8859_1},
    {"eesti",                          ENC_ISO8859_1},
    {"el",                             ENC_ISO8859_7},
    {"el_GR",                          ENC_ISO8859_7},
    {"en",                             ENC_ISO8859_1},
    {"en_AU",                          ENC_ISO8859_1},
    {"en_BW",                          ENC_ISO8859_1},
    {"en_CA",                          ENC_ISO8859_1},
    {"en_DK",                          ENC_ISO8859_1},
    {"en_GB",                          ENC_ISO8859_1},
    {"en_HK",                          ENC_ISO8859_1},
    {"en_IE",                          ENC_ISO8859_1},
    {"en_IE@euro",                     ENC_ISO8859_15},
    {"en_IN",                          ENC_UTF_8},
    {"en_NZ",                          ENC_ISO8859_1},
    {"en_PH",                          ENC_ISO8859_1},
    {"en_SG",                          ENC_ISO8859_1},
    {"en_UK",                          ENC_ISO8859_1},
    {"en_US",                          ENC_ISO8859_1},
    {"en_ZA",                          ENC_ISO8859_1},
    {"en_ZW",                          ENC_ISO8859_1},
    {"es",                             ENC_ISO8859_1},
    {"es_AR",                          ENC_ISO8859_1},
    {"es_BO",                          ENC_ISO8859_1},
    {"es_CL",                          ENC_ISO8859_1},
    {"es_CO",                          ENC_ISO8859_1},
    {"es_CR",                          ENC_ISO8859_1},
    {"es_DO",                          ENC_ISO8859_1},
    {"es_EC",                          ENC_ISO8859_1},
    {"es_ES",                          ENC_ISO8859_1},
    {"es_ES@euro",                     ENC_ISO8859_15},
    {"es_GT",                          ENC_ISO8859_1},
    {"es_HN",                          ENC_ISO8859_1},
    {"es_MX",                          ENC_ISO8859_1},
    {"es_NI",                          ENC_ISO8859_1},
    {"es_PA",                          ENC_ISO8859_1},
    {"es_PE",                          ENC_ISO8859_1},
    {"es_PR",                          ENC_ISO8859_1},
    {"es_PY",                          ENC_ISO8859_1},
    {"es_SV",                          ENC_ISO8859_1},
    {"es_US",                          ENC_ISO8859_1},
    {"es_UY",                          ENC_ISO8859_1},
    {"es_VE",                          ENC_ISO8859_1},
    {"estonian",                       ENC_ISO8859_1},
    {"et",                             ENC_ISO8859_15},
    {"et_EE",                          ENC_ISO8859_15},
    {"eu",                             ENC_ISO8859_1},
    {"eu_ES",                          ENC_ISO8859_1},
    {"eu_ES@euro",                     ENC_ISO8859_15},
    {"eu_FR",                          ENC_ISO8859_1},
    {"eu_FR@euro",                     ENC_ISO8859_15},
    {"fa",                             ENC_UTF_8},
    {"fa_IR",                          ENC_UTF_8},
    {"fi",                             ENC_ISO8859_1},
    {"fi_FI",                          ENC_ISO8859_1},
    {"fi_FI@euro",                     ENC_ISO8859_15},
    {"finnish",                        ENC_ISO8859_1},
    {"fo",                             ENC_ISO8859_1},
    {"fo_FO",                          ENC_ISO8859_1},
    {"fr",                             ENC_ISO8859_1},
    {"fr_BE",                          ENC_ISO8859_1},
    {"fr_BE@euro",                     ENC_ISO8859_15},
    {"fr_CA",                          ENC_ISO8859_1},
    {"fr_CH",                          ENC_ISO8859_1},
    {"fr_FR",                          ENC_ISO8859_1},
    {"fr_FR@euro",                     ENC_ISO8859_15},
    {"fr_LU",                          ENC_ISO8859_1},
    {"fr_LU@euro",                     ENC_ISO8859_15},
    {"fran""\xe7""ais",                ENC_ISO8859_1},
    {"french",                         ENC_ISO8859_1},
    {"ga",                             ENC_ISO8859_1},
    {"ga_IE",                          ENC_ISO8859_1},
    {"ga_IE@euro",                     ENC_ISO8859_15},
    {"galego",                         ENC_ISO8859_1},
    {"galician",                       ENC_ISO8859_1},
    {"gd",                             ENC_ISO8859_1},
    {"gd_GB",                          ENC_ISO8859_1},
    {"german",                         ENC_ISO8859_1},
    {"gez_ER",                         ENC_UTF_8},
    {"gez_ER@abegede",                 ENC_UTF_8},
    {"gez_ET",                         ENC_UTF_8},
    {"gez_ET@abegede",                 ENC_UTF_8},
    {"gl",                             ENC_ISO8859_1},
    {"gl_ES",                          ENC_ISO8859_1},
    {"gl_ES@euro",                     ENC_ISO8859_15},
    {"greek",                          ENC_ISO8859_7},
    {"gu_IN",                          ENC_UTF_8},
    {"gv",                             ENC_ISO8859_1},
    {"gv_GB",                          ENC_ISO8859_1},
    {"he",                             ENC_ISO8859_8},
    {"he_IL",                          ENC_ISO8859_8},
    {"hebrew",                         ENC_ISO8859_8},
    {"hr",                             ENC_ISO8859_2},
    {"hr_HR",                          ENC_ISO8859_2},
    {"hrvatski",                       ENC_ISO8859_2},
    {"hu",                             ENC_ISO8859_2},
    {"hu_HU",                          ENC_ISO8859_2},
    {"hungarian",                      ENC_ISO8859_2},
    {"hy",                             ENC_ARMSCII_8},
    {"hy_AM",                          ENC_ARMSCII_8},
    {"icelandic",                      ENC_ISO8859_1},
    {"id",                             ENC_ISO8859_1},
    {"id_ID",                          ENC_ISO8859_1},
    {"in",                             ENC_ISO8859_1},
    {"in_ID",                          ENC_ISO8859_1},
    {"is",                             ENC_ISO8859_1},
    {"is_IS",                          ENC_ISO8859_1},
    {"iso_8859_1",                     ENC_ISO8859_1},
    {"it",                             ENC_ISO8859_1},
    {"it_CH",                          ENC_ISO8859_1},
    {"it_IT",                          ENC_ISO8859_1},
    {"it_IT@euro",                     ENC_ISO8859_15},
    {"italian",                        ENC_ISO8859_1},
    {"iw",                             ENC_ISO8859_8},
    {"iw_IL",                          ENC_ISO8859_8},
    {"ja",                             ENC_EUC_JP},
    {"ja_JP",                          ENC_EUC_JP},
    {"japan",                          ENC_EUC_JP},
    {"japanese",                       ENC_EUC_JP},
    {"ka",                             ENC_GEORGIAN_ACADEMY},
    {"ka_GE",                          ENC_GEORGIAN_ACADEMY},
    {"kl",                             ENC_ISO8859_1},
    {"kl_GL",                          ENC_ISO8859_1},
    {"kn_IN",                          ENC_UTF_8},
    {"ko",                             ENC_EUC_KR},
    {"ko_KR",                          ENC_EUC_KR},
    {"korean",                         ENC_EUC_KR},
    {"kw",                             ENC_ISO8859_1},
    {"kw_GB",                          ENC_ISO8859_1},
    {"lg_UG",                          ENC_ISO8859_10},
    {"lithuanian",                     ENC_ISO8859_13},
    {"lt",                             ENC_ISO8859_13},
    {"lt_LT",                          ENC_ISO8859_13},
    {"lv",                             ENC_ISO8859_13},
    {"lv_LV",                          ENC_ISO8859_13},
    {"mi",                             ENC_ISO8859_13},
    {"mi_NZ",                          ENC_ISO8859_13},
    {"mk",                             ENC_ISO8859_5},
    {"mk_MK",                          ENC_ISO8859_5},
    {"ml_IN",                          ENC_UTF_8},
    {"mn_MN",                          ENC_UTF_8},
    {"mr_IN",                          ENC_UTF_8},
    {"ms",                             ENC_ISO8859_1},
    {"ms_MY",                          ENC_ISO8859_1},
    {"mt",                             ENC_ISO8859_3},
    {"mt_MT",                          ENC_ISO8859_3},
    {"nb",                             ENC_ISO8859_1},
    {"nb_NO",                          ENC_ISO8859_1},
    {"ne_NP",                          ENC_UTF_8},
    {"nl",                             ENC_ISO8859_1},
    {"nl_BE",                          ENC_ISO8859_1},
    {"nl_BE@euro",                     ENC_ISO8859_15},
    {"nl_NL",                          ENC_ISO8859_1},
    {"nl_NL@euro",                     ENC_ISO8859_15},
    {"nn",                             ENC_ISO8859_1},
    {"nn_NO",                          ENC_ISO8859_1},
    {"no",                             ENC_ISO8859_1},
    {"no@nynorsk",                     ENC_ISO8859_1},
    {"no_NO",                          ENC_ISO8859_1},
    {"norwegian",                      ENC_ISO8859_1},
    {"nynorsk",                        ENC_ISO8859_1},
    {"oc",                             ENC_ISO8859_1},
    {"oc_FR",                          ENC_ISO8859_1},
    {"oc_FR@euro",                     ENC_ISO8859_15},
    {"om_ET",                          ENC_UTF_8},
    {"om_KE",                          ENC_ISO8859_1},
    {"pa_IN",                          ENC_UTF_8},
    {"ph",                             ENC_ISO8859_1},
    {"ph_PH",                          ENC_ISO8859_1},
    {"pl",                             ENC_ISO8859_2},
    {"pl_PL",                          ENC_ISO8859_2},
    {"polish",                         ENC_ISO8859_2},
    {"portuguese",                     ENC_ISO8859_1},
    {"pp",                             ENC_ISO8859_1},
    {"pp_AN",                          ENC_ISO8859_1},
    {"pt",                             ENC_ISO8859_1},
    {"pt_BR",                          ENC_ISO8859_1},
    {"pt_PT",                          ENC_ISO8859_1},
    {"pt_PT@euro",                     ENC_ISO8859_15},
    {"ro",                             ENC_ISO8859_2},
    {"ro_RO",                          ENC_ISO8859_2},
    {"romanian",                       ENC_ISO8859_2},
    {"ru",                             ENC_KOI8_R},
    {"ru_RU",                          ENC_KOI8_R},
    {"ru_UA",                          ENC_KOI8_U},
    {"rumanian",                       ENC_ISO8859_2},
    {"russian",                        ENC_ISO8859_5},
    {"se_NO",                          ENC_UTF_8},
    {"serbocroatian",                  ENC_ISO8859_2},
    {"sh",                             ENC_ISO8859_2},
    {"sh_SP",                          ENC_ISO8859_2},
    {"sh_YU",                          ENC_ISO8859_2},
    {"sid_ET",                         ENC_UTF_8},
    {"sk",                             ENC_ISO8859_2},
    {"sk_SK",                          ENC_ISO8859_2},
    {"sl",                             ENC_ISO8859_2},
    {"sl_SI",                          ENC_ISO8859_2},
    {"slovak",                         ENC_ISO8859_2},
    {"slovene",                        ENC_ISO8859_2},
    {"slovenian",                      ENC_ISO8859_2},
    {"so_DJ",                          ENC_ISO8859_1},
    {"so_ET",                          ENC_UTF_8},
    {"so_KE",                          ENC_ISO8859_1},
    {"so_SO",                          ENC_ISO8859_1},
    {"sp",                             ENC_ISO8859_5},
    {"sp_YU",                          ENC_ISO8859_5},
    {"spanish",                        ENC_ISO8859_1},
    {"sq",                             ENC_ISO8859_2},
    {"sq_AL",                          ENC_ISO8859_2},
    {"sr",                             ENC_ISO8859_5},
    {"sr@cyrillic",                    ENC_ISO8859_5},
    {"sr_SP",                          ENC_ISO8859_2},
    {"sr_YU",                          ENC_ISO8859_5},
    {"sr_YU@cyrillic",                 ENC_ISO8859_5},
    {"st_ZA",                          ENC_ISO8859_1},
    {"sv",                             ENC_ISO8859_1},
    {"sv_FI",                          ENC_ISO8859_1},
    {"sv_FI@euro",                     ENC_ISO8859_15},
    {"sv_SE",                          ENC_ISO8859_1},
    {"sv_SE@euro",                     ENC_ISO8859_15},
    {"swedish",                        ENC_ISO8859_1},
    {"te_IN",                          ENC_UTF_8},
    {"th",                             ENC_ISO8859_11},
    {"th_TH",                          ENC_ISO8859_11},
    {"thai",                           ENC_ISO8859_11},
    {"ti_ER",                          ENC_UTF_8},
    {"ti_ET",                          ENC_UTF_8},
    {"tig_ER",                         ENC_UTF_8},
    {"tl",                             ENC_ISO8859_1},
    {"tl_PH",                          ENC_ISO8859_1},
    {"tr",                             ENC_ISO8859_9},
    {"tr_TR",                          ENC_ISO8859_9},
    {"turkish",                        ENC_ISO8859_9},
    {"uk",                             ENC_KOI8_U},
    {"uk_UA",                          ENC_KOI8_U},
    {"ur",                             ENC_CP1256},
    {"ur_PK",                          ENC_CP1256},
    {"uz_UZ",                          ENC_ISO8859_1},
    {"uz_UZ@cyrillic",                 ENC_UTF_8},
    {"vi",                             ENC_TCVN},
    {"vi_VN",                          ENC_TCVN},
    {"wa",                             ENC_ISO8859_1},
    {"wa_BE",                          ENC_ISO8859_1},
    {"wa_BE@euro",                     ENC_ISO8859_15},
    {"xh_ZA",                          ENC_ISO8859_1},
    {"yi",                             ENC_CP1255},
    {"yi_US",                          ENC_CP1255},
    {"zh_CN",                          ENC_GBK},
    {"zh_HK",                          ENC_BIG5_HKSCS},
    {"zh_SG",                          ENC_GB2312},
    {"zh_TW",                          ENC_BIG5},
    {"zu_ZA",                          ENC_ISO8859_1},
};
static const int guess_count = (sizeof(guess)/sizeof(name_value));

static const name_value known[] = {
    {"iso88591", "ISO8859-1"},
    {"iso88592", "ISO8859-2"},
    {"iso88593", "ISO8859-3"},
    {"iso88596", "ISO8859-6"},
    {"iso88597", "ISO8859-7"},
    {"iso88598", "ISO8859-8"},
    {"iso88599", "ISO8859-9"},
    {"iso885910", "ISO8859-10"},
    {"iso885913", "ISO8859-13"},
    {"iso885914", "ISO8859-14"},
    {"iso885915", "ISO8859-15"},
    {"cp1251", "CP1251"},
    {"cp1255", "CP1255"},
    {"eucjp", "EUC-JP"},
    {"euckr", "EUC-KR"},
    {"euctw", "EUC-TW"},
    {"georgianps", "GEORGIAN-PS"},
    {"koi8u", "KOI8-U"},
    {"tcvn", "TCVN"},
    {"big5", "BIG5"},
    {"gb2312", "GB2312"},
    {"gb18030", "GB18030"},
    {"gbk", "GBK"},
    {"tis-620", "TIS-620"},
    {"sjis", "SHIFT_JIS"},
    {"euccn", "GB2312"},
    {"big5-hkscs", "BIG5-HKSCS"},
#ifdef __APPLE__
    /* known additional Apple encodings (see locale -a) up to Mac OS X 10.5,
       unlike other systems they correspond directly */
    {"iso8859-1", "ISO8859-1"},
    {"iso8859-2", "ISO8859-2"},
    {"iso8859-4", "ISO8859-4"},
    {"iso8859-7", "ISO8859-7"},
    {"iso8859-9", "ISO8859-9"},
    {"iso8859-13", "ISO8859-13"},
    {"iso8859-15", "ISO8859-15"},
    {"koi8-u", "KOI8-U"},
    {"koi8-r", "KOI8-R"},
    {"pt154", "PT154"},
    {"us-ascii", "ASCII"},
    {"armscii-8", "ARMSCII-8"},
    {"iscii-dev", "ISCII-DEV"},
    {"big5hkscs", "BIG5-HKSCS"},
#endif
};
static const int known_count = (sizeof(known)/sizeof(name_value));


#ifndef __APPLE__
static char* name_value_search(const char *name, const name_value table[],
			       const int table_count)
{
    int min, mid, max;

#if defined(DEBUG_TEST)
    static last;
    DPRINT(last);
    last = 0;
#endif

    min = 0;
    max = table_count - 1;

    if ( 0 > strcmp(name,table[min].name) ||
	 0 < strcmp(name,table[max].name) ) {
#if defined(DEBUG_TEST) && DEBUG_TEST > 1
	DPRINT(strcmp(name, table[min].name));
	DPRINT(strcmp(name, table[max].name));
#endif
	return (NULL);
    }
    while (max >= min) {
#if defined(DEBUG_TEST)
	last++;
#endif
	mid = (min + max) / 2;
#if defined(DEBUG_TEST) && DEBUG_TEST > 1
	SPRINT(table[mid].name);
#endif
	if (0 < strcmp(name,table[mid].name)) {
#if defined(DEBUG_TEST) && DEBUG_TEST > 1
	    DPRINT(strcmp(name, table[mid].name));
#endif
	    min = mid + 1;
	} else if (0 > strcmp(name, table[mid].name)) {
#if defined(DEBUG_TEST) && DEBUG_TEST > 1
	    DPRINT(strcmp(name, table[mid].name));
#endif
	    max = mid - 1;
	} else {
#if defined(DEBUG_TEST) && DEBUG_TEST > 1
	    DPRINT(strcmp(name, table[mid].name));
#endif
	    return(table[mid].value);
	}
    }
    return (NULL);
}
#endif

const char *locale2charset(const char *locale)
{
    static char charset[128];

    char la_loc[128];
    char enc[128], *p;
    int i;
    int  cp;
#ifndef __APPLE__
    char *value;
#endif

    if ((locale == NULL) || (0 == strcmp(locale, "NULL")))
	locale = setlocale(LC_CTYPE,NULL);

    /* in some rare circumstances Darwin may return NULL */
    if (!locale || !strcmp(locale, "C") || !strcmp(locale, "POSIX"))
	return ("ASCII");

    memset(charset,0,sizeof(charset));

    /* separate language_locale.encoding
       NB, under Windows 'locale' may contains dots
     */
    memset(la_loc, 0, sizeof(la_loc));
    memset(enc, 0, sizeof(enc));
    p = strrchr(locale, '.');
    if(p) {
	strncpy(enc, p+1, sizeof(enc)-1);
	strncpy(la_loc, locale, sizeof(la_loc)-1);
	p = strrchr(la_loc, '.');
	if(p) *p = '\0';
    }
    
#ifdef Win32
    /*
      ## PUTTY suggests mapping Windows code pages as
      ## 1250 -> ISO 8859-2: this is WRONG
      ## 1251 -> KOI8-U
      ## 1252 -> ISO 8859-1
      ## 1253 -> ISO 8859-7
      ## 1254 -> ISO 8859-9
      ## 1255 -> ISO 8859-8
      ## 1256 -> ISO 8859-6
      ## 1257 -> ISO 8859-13
    */
    switch(cp = atoi(enc)) {
	/* case 1250: return "ISO8859-2"; */
	/* case 1251: return "KOI8-U"; This is not anywhere near the same */
    case 1252: return "ISO8859-1";
	/*
	  case 1253: return "ISO8859-7";
	  case 1254: return "ISO8859-9";
	  case 1255: return "ISO8859-8";
	  case 1256: return "ISO8859-6";
	*/
    case 1257: return "ISO8859-13";
    default:
	snprintf(charset, 128, "CP%u", cp);
	return charset;
    }
#endif

    /*
     * Assume locales are like en_US[.utf8[@euro]]
     */
    /* cut encoding @hoge  no use.
       for(i=0;enc[i] && enc[i]!='@' && i<sizeof(enc)-1;i++);
       enc[i]='\0';
    */

    /* for AIX */
    if (0 == strcmp(enc, "UTF-8")) strcpy(enc, "utf8");

    if(strcmp(enc, "") && strcmp(enc, "utf8")) {
	for(i = 0; enc[i]; i++) enc[i] = (char) tolower(enc[i]);

	for(i = 0; i < known_count; i++)
	    if (0 == strcmp(known[i].name,enc)) return known[i].value;

	/* cut encoding old linux cp- */
	if (0 == strncmp(enc, "cp-", 3)){
	    snprintf(charset, 128, "CP%s", enc+3);
	    return charset;
	}
	/* cut encoding IBM ibm- */
	if (0 == strncmp(enc, "ibm", 3)){
	    cp = atoi(enc + 3);
	    snprintf(charset, 128, "IBM-%d", abs(cp));
	    /* IBM-[0-9]+ case */
	    if(cp != 0) return charset;
	    /* IBM-eucXX case */
	    strncpy(charset, (enc[3] == '-') ? enc+4: enc+3, sizeof(charset));
	    if(strncmp(charset, "euc", 3)) {
		if (charset[3] != '-') {
		    for(i = (int) strlen(charset)-3; 0 < i; i--)
			charset[i+1] = charset[i];
		    charset[3] = '-';
		}
		for(i = 0; charset[i]; i++)
		    charset[i] = (char) toupper(charset[i]);
		return charset;
	    }
	}

	/* let's hope it is a ll_* name */
	if (0 == strcmp(enc, "euc")) {
	    /* This is OK as encoding names are ASCII */
	    if(isalpha((int)la_loc[0]) && isalpha((int)la_loc[1])
	       && (la_loc[2] == '_')) {
		if (0 == strncmp("ja", la_loc, 2)) return "EUC-JP";
		if (0 == strncmp("ko", la_loc, 2)) return "EUC-KR";
		if (0 == strncmp("zh", la_loc, 2)) return "GB2312";
	    }
	}

    }

#ifdef __APPLE__
    /* on Mac OS X *all* real locales w/o encoding part are UTF-8 locales
       (C and POSIX are virtual and taken care of previously) */
    return "UTF-8";
#else

    if(0 == strcmp(enc, "utf8")) return "UTF-8";

    value = name_value_search(la_loc, guess, guess_count);
    return value == NULL ? "ASCII" : value;
#endif
}

/*****************************************************
 * Test !!
 *****************************************************/
#ifdef DEBUG_TEST
main()
{
    int i;
    i=0;
    setlocale(LC_CTYPE,"");
    DPRINT(guess_count);
#ifndef Win32
    SPRINT(locale2charset(NULL));
    SPRINT(locale2charset("ja"));
    SPRINT(locale2charset("ja_JP"));
    SPRINT(locale2charset("ja_JP.eucJP"));
    SPRINT(locale2charset("ja_JP.ujis"));
    SPRINT(locale2charset("ja_JP.IBM-eucJP"));
    SPRINT(locale2charset("ja_JP.sjis"));
    SPRINT(locale2charset("ja_JP.IBM-932"));
    /* cannot encoding only zh */
    SPRINT(locale2charset("zh""\0""BIG5"));
    SPRINT(locale2charset("zh_CN"));
    SPRINT(locale2charset("zh_CN.BIG5"));
    SPRINT(locale2charset("zh_TW"));
    SPRINT(locale2charset("zh_TW.eucTW"));
    SPRINT(locale2charset("zh_TW.big5"));
    SPRINT(locale2charset("zh_SG"));
    SPRINT(locale2charset("zh_HK"));
    SPRINT(locale2charset("ko"));
    SPRINT(locale2charset("en"));
    SPRINT(locale2charset("en_IE@euro"));
    SPRINT(locale2charset("en_IN"));
    SPRINT(locale2charset("C"));
    SPRINT(locale2charset("fran""\xe7""ais"));
    for(i=0;i<guess_count;i++){
	locale2charset(guess[i].name);
    }
#else
    SPRINT(locale2charset("japanese_JAPAN.932"));
    SPRINT(locale2charset("japanese_JAPAN.932"));
#endif
}

#endif /* DEBUG_TEST */
