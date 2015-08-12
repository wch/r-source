/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-2015   The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#include <string.h>

/*
  From http://unicode.org/cldr/data/diff/supplemental/windows_tzid.html
  Added some entries from the XP Registry (and checked on Vista).
  Table updated from Unicode in August 2015.
 */
const static struct {
    const wchar_t * const reg;
    const char * const Olson;
} TZtable[] = {
    { L"Afghanistan Standard Time", "Asia/Kabul" },
    { L"Alaskan Standard Time", "America/Anchorage" },
    { L"Arab Standard Time", "Asia/Riyadh" },
    { L"Arabian Standard Time", "Asia/Bahrain" },
    { L"Arabic Standard Time", "Asia/Baghdad" },
    { L"Armenian Standard Time", "Asia/Yerevan" },
    { L"Atlantic Standard Time", "America/Curacao" },
    { L"AUS Central Standard Time", "Australia/Darwin" },
    { L"AUS Eastern Standard Time", "Australia/Sydney" },
    { L"Azerbaijan Standard Time", "Asia/Baku" },
    { L"Azores Standard Time", "Atlantic/Azores" },
    { L"Canada Central Standard Time", "America/Regina" },
    { L"Cape Verde Standard Time", "Atlantic/Cape_Verde" },
    { L"Caucasus Standard Time", "Asia/Yerevan" },
    { L"Cen. Australia Standard Time", "Australia/Adelaide" },
    { L"Central America Standard Time", "America/Guatemala" },
    { L"Central Asia Standard Time", "Asia/Dhaka" },
    { L"Central Brazilian Standard Time", "America/Manaus" },
    { L"Central Europe Standard Time", "Europe/Prague" },
    { L"Central European Standard Time", "Europe/Warsaw" },
    { L"Central Pacific Standard Time", "Pacific/Guadalcanal" },
    { L"Central Standard Time", "America/Chicago" },
    { L"Central Standard Time (Mexico)", "America/Mexico_City" },
    { L"China Standard Time", "Asia/Taipei" },
    { L"Dateline Standard Time", "Pacific/Kwajalein" },
    { L"E. Africa Standard Time", "Africa/Nairobi" },
    { L"E. Australia Standard Time", "Australia/Brisbane" },
    { L"E. Europe Standard Time", "Europe/Minsk" },
    { L"E. South America Standard Time", "America/Sao_Paulo" },
    { L"Eastern Standard Time", "America/New_York" },
    { L"Egypt Standard Time", "Africa/Cairo" },
    { L"Ekaterinburg Standard Time", "Asia/Yekaterinburg" },
    { L"Fiji Standard Time", "Pacific/Fiji" },
    { L"FLE Standard Time", "Europe/Helsinki" },
    { L"Georgian Standard Time", "Asia/Tbilisi" },
    { L"GMT Standard Time", "Europe/London" },
    { L"Greenland Standard Time", "America/Godthab" },
    { L"Greenwich Standard Time", "Africa/Casablanca" },
    { L"GTB Standard Time", "Europe/Istanbul" },
    { L"Hawaiian Standard Time", "Pacific/Honolulu" },
    { L"India Standard Time", "Asia/Calcutta" },
    { L"Iran Standard Time", "Asia/Tehran" },
    /* next is name of timezone, not of Std */
    { L"Israel Standard Time", "Asia/Jerusalem" },
    { L"Jerusalem Standard Time", "Asia/Jerusalem" },
    { L"Jordan Standard Time", "Asia/Amman" },
    { L"Korea Standard Time", "Asia/Seoul" },
    { L"Malay Peninsula Standard Time", "Asia/Kuala_Lumpur" },
    { L"Mexico Standard Time", "America/Mexico_City" },
    { L"Mexico Standard Time 2", "America/Chihuahua" },
    { L"Mid-Atlantic Standard Time", "America/Noronha" },
    { L"Middle East Standard Time", "Asia/Beirut" },
    { L"Montevideo Standard Time", "America/Montevideo" },
    { L"Mountain Standard Time", "America/Denver" },
    { L"Mountain Standard Time (Mexico)", "America/Chihuahua" },
    { L"Myanmar Standard Time", "Asia/Rangoon" },
    { L"N. Central Asia Standard Time", "Asia/Novosibirsk" },
    { L"Namibia Standard Time", "Africa/Windhoek" },
    { L"Nepal Standard Time", "Asia/Katmandu" },
    { L"New Zealand Standard Time", "Pacific/Auckland" },
    { L"Newfoundland Standard Time", "America/St_Johns" },
    { L"North Asia East Standard Time", "Asia/Ulaanbaatar" },
    { L"North Asia Standard Time", "Asia/Krasnoyarsk" },
    { L"Pacific SA Standard Time", "America/Santiago" },
    { L"Pacific Standard Time", "America/Los_Angeles" },
    { L"Pacific Standard Time (Mexico)", "America/Tijuana" },
    { L"Romance Standard Time", "Europe/Paris" },
    { L"Russian Standard Time", "Europe/Moscow" },
    { L"SA Eastern Standard Time", "America/Buenos_Aires" },
    { L"SA Pacific Standard Time", "America/Bogota" },
    { L"SA Western Standard Time", "America/Caracas" },
    { L"Samoa Standard Time", "Pacific/Apia" },
    { L"SE Asia Standard Time", "Asia/Bangkok" },
    /* next is name of timezone, not of Std */
    { L"Singapore Standard Time", "Asia/Kuala_Lumpur" },
    { L"South Africa Standard Time", "Africa/Johannesburg" },
    { L"Sri Lanka Standard Time", "Asia/Colombo" },
    { L"Taipei Standard Time", "Asia/Taipei" },
    { L"Tasmania Standard Time", "Australia/Hobart" },
    { L"Tokyo Standard Time", "Asia/Tokyo" },
    { L"Tonga Standard Time", "Pacific/Tongatapu" },
    { L"US Eastern Standard Time", "America/Indianapolis" },
    { L"US Mountain Standard Time", "America/Phoenix" },
    { L"Venezuela Standard Time", "America/Caracas" },
    { L"Vladivostok Standard Time", "Asia/Vladivostok" },
    { L"W. Australia Standard Time", "Australia/Perth" },
    { L"W. Central Africa Standard Time", "Africa/Lagos" },
    { L"W. Europe Standard Time", "Europe/Berlin" },
    { L"West Asia Standard Time", "Asia/Karachi" },
    { L"West Pacific Standard Time", "Pacific/Guam" },
    { L"Yakutsk Standard Time", "Asia/Yakutsk" },

/* The ones below do not occur on BDR's XP machine */
    { L"Acre Standard Time", "America/Rio_Branco" },
    { L"Africa_Central Standard Time", "Africa/Kigali" },
    { L"Africa_Eastern Standard Time", "Africa/Kampala" },
    { L"Africa_FarWestern Standard Time", "Africa/El_Aaiun" },
    { L"Africa_Southern Standard Time", "Africa/Johannesburg" },
    { L"Africa_Western Standard Time", "Africa/Niamey" },
    { L"Aktyubinsk Standard Time", "Asia/Aqtobe" },
    { L"Alaska Standard Time", "America/Juneau" },
    { L"Alaska_Hawaii Standard Time", "America/Anchorage" },
    { L"Almaty Standard Time", "Asia/Almaty" },
    { L"Amazon Standard Time", "America/Manaus" },
    { L"America_Central Standard Time", "America/Winnipeg" },
    { L"America_Eastern Standard Time", "America/Panama" },
    { L"America_Mountain Standard Time", "America/Edmonton" },
    { L"America_Pacific Standard Time", "America/Vancouver" },
    { L"Anadyr Standard Time", "Asia/Anadyr" },
    { L"Aqtau Standard Time", "Asia/Aqtau" },
    { L"Aqtobe Standard Time", "Asia/Aqtobe" },
    { L"Argentina Standard Time", "America/Buenos_Aires" },
    { L"Argentina_Western Standard Time", "America/Mendoza" },
    { L"Armenia Standard Time", "Asia/Yerevan" },
    { L"Ashkhabad Standard Time", "Asia/Ashgabat" },
    { L"Australia_Central Standard Time", "Australia/Adelaide" },
    { L"Australia_CentralWestern Standard Time", "Australia/Eucla" },
    { L"Australia_Eastern Standard Time", "Australia/Sydney" },
    { L"Australia_Western Standard Time", "Australia/Perth" },
    { L"Baku Standard Time", "Asia/Baku" },
    { L"Bangladesh Standard Time", "Asia/Dhaka" },
    { L"Bering Standard Time", "America/Adak" },
    { L"Bhutan Standard Time", "Asia/Thimphu" },
    { L"Bolivia Standard Time", "America/La_Paz" },
    { L"Borneo Standard Time", "Asia/Kuching" },
    { L"Brasilia Standard Time", "America/Sao_Paulo" },
    { L"British Standard Time", "Europe/London" },
    { L"Brunei Standard Time", "Asia/Brunei" },
    { L"Chamorro Standard Time", "Pacific/Guam" },
    { L"Changbai Standard Time", "Asia/Harbin" },
    { L"Chatham Standard Time", "Pacific/Chatham" },
    { L"Chile Standard Time", "America/Santiago" },
    { L"Choibalsan Standard Time", "Asia/Choibalsan" },
    { L"Christmas Standard Time", "Indian/Christmas" },
    { L"Cocos Standard Time", "Indian/Cocos" },
    { L"Colombia Standard Time", "America/Bogota" },
    { L"Cook Standard Time", "Pacific/Rarotonga" },
    { L"Cuba Standard Time", "America/Havana" },
    { L"Dacca Standard Time", "Asia/Dhaka" },
    { L"Davis Standard Time", "Antarctica/Davis" },
    { L"Dominican Standard Time", "America/Santo_Domingo" },
    { L"DumontDUrville Standard Time", "Antarctica/DumontDUrville" },
    { L"Dushanbe Standard Time", "Asia/Dushanbe" },
    { L"Dutch_Guiana Standard Time", "America/Paramaribo" },
    { L"East_Timor Standard Time", "Asia/Dili" },
    { L"Easter Standard Time", "Pacific/Easter" },
    { L"Ecuador Standard Time", "America/Guayaquil" },
    { L"Europe_Central Standard Time", "Europe/Oslo" },
    { L"Europe_Eastern Standard Time", "Europe/Vilnius" },
    { L"Europe_Western Standard Time", "Atlantic/Canary" },
    { L"Falkland Standard Time", "Atlantic/Stanley" },
    { L"French_Guiana Standard Time", "America/Cayenne" },
    { L"French_Southern Standard Time", "Indian/Kerguelen" },
    { L"Frunze Standard Time", "Asia/Bishkek" },
    { L"Galapagos Standard Time", "Pacific/Galapagos" },
    { L"Gambier Standard Time", "Pacific/Gambier" },
    { L"Georgia Standard Time", "Asia/Tbilisi" },
    { L"Gilbert_Islands Standard Time", "Pacific/Tarawa" },
    { L"Goose_Bay Standard Time", "America/Goose_Bay" },
    { L"Greenland_Central Standard Time", "America/Scoresbysund" },
    { L"Greenland_Eastern Standard Time", "America/Scoresbysund" },
    { L"Greenland_Western Standard Time", "America/Godthab" },
    { L"Guam Standard Time", "Pacific/Guam" },
    { L"Gulf Standard Time", "Asia/Muscat" },
    { L"Guyana Standard Time", "America/Guyana" },
    { L"Hawaii_Aleutian Standard Time", "Pacific/Honolulu" },
    { L"Hong_Kong Standard Time", "Asia/Hong_Kong" },
    { L"Hovd Standard Time", "Asia/Hovd" },
    { L"Indian_Ocean Standard Time", "Indian/Chagos" },
    { L"Indochina Standard Time", "Asia/Vientiane" },
    { L"Indonesia_Central Standard Time", "Asia/Makassar" },
    { L"Indonesia_Eastern Standard Time", "Asia/Jayapura" },
    { L"Indonesia_Western Standard Time", "Asia/Jakarta" },
    { L"Irish Standard Time", "Europe/Dublin" },
    { L"Irkutsk Standard Time", "Asia/Irkutsk" },
    { L"Japan Standard Time", "Asia/Tokyo" },
    { L"Kamchatka Standard Time", "Asia/Kamchatka" },
    { L"Karachi Standard Time", "Asia/Karachi" },
    { L"Kashgar Standard Time", "Asia/Kashgar" },
    { L"Kazakhstan_Eastern Standard Time", "Asia/Almaty" },
    { L"Kazakhstan_Western Standard Time", "Asia/Aqtobe" },
    { L"Kizilorda Standard Time", "Asia/Qyzylorda" },
    { L"Kosrae Standard Time", "Pacific/Kosrae" },
    { L"Krasnoyarsk Standard Time", "Asia/Krasnoyarsk" },
    { L"Kuybyshev Standard Time", "Europe/Samara" },
    { L"Kwajalein Standard Time", "Pacific/Kwajalein" },
    { L"Kyrgystan Standard Time", "Asia/Bishkek" },
    { L"Lanka Standard Time", "Asia/Colombo" },
    { L"Liberia Standard Time", "Africa/Monrovia" },
    { L"Line_Islands Standard Time", "Pacific/Kiritimati" },
    { L"Long_Shu Standard Time", "Asia/Chongqing" },
    { L"Lord_Howe Standard Time", "Australia/Lord_Howe" },
    { L"Macau Standard Time", "Asia/Macau" },
    { L"Magadan Standard Time", "Asia/Magadan" },
    { L"Malaya Standard Time", "Asia/Kuala_Lumpur" },
    { L"Malaysia Standard Time", "Asia/Kuching" },
    { L"Maldives Standard Time", "Indian/Maldives" },
    { L"Marquesas Standard Time", "Pacific/Marquesas" },
    { L"Marshall_Islands Standard Time", "Pacific/Majuro" },
    { L"Mauritius Standard Time", "Indian/Mauritius" },
    { L"Mawson Standard Time", "Antarctica/Mawson" },
    { L"Mongolia Standard Time", "Asia/Ulaanbaatar" },
    { L"Moscow Standard Time", "Europe/Moscow" },
    { L"Nauru Standard Time", "Pacific/Nauru" },
    { L"New_Caledonia Standard Time", "Pacific/Noumea" },
    { L"New_Zealand Standard Time", "Pacific/Auckland" },
    { L"Niue Standard Time", "Pacific/Niue" },
    { L"Norfolk Standard Time", "Pacific/Norfolk" },
    { L"Noronha Standard Time", "America/Noronha" },
    { L"North_Mariana Standard Time", "Pacific/Saipan" },
    { L"Novosibirsk Standard Time", "Asia/Novosibirsk" },
    { L"Omsk Standard Time", "Asia/Omsk" },
    { L"Oral Standard Time", "Asia/Oral" },
    { L"Pakistan Standard Time", "Asia/Karachi" },
    { L"Palau Standard Time", "Pacific/Palau" },
    { L"Papua_New_Guinea Standard Time", "Pacific/Port_Moresby" },
    { L"Paraguay Standard Time", "America/Asuncion" },
    { L"Peru Standard Time", "America/Lima" },
    { L"Philippines Standard Time", "Asia/Manila" },
    { L"Phoenix_Islands Standard Time", "Pacific/Enderbury" },
    { L"Pierre_Miquelon Standard Time", "America/Miquelon" },
    { L"Pitcairn Standard Time", "Pacific/Pitcairn" },
    { L"Ponape Standard Time", "Pacific/Ponape" },
    { L"Qyzylorda Standard Time", "Asia/Qyzylorda" },
    { L"Reunion Standard Time", "Indian/Reunion" },
    { L"Rothera Standard Time", "Antarctica/Rothera" },
    { L"Sakhalin Standard Time", "Asia/Sakhalin" },
    { L"Samara Standard Time", "Europe/Samara" },
    { L"Samarkand Standard Time", "Asia/Samarkand" },
    { L"Seychelles Standard Time", "Indian/Mahe" },
    { L"Shevchenko Standard Time", "Asia/Aqtau" },
    { L"Solomon Standard Time", "Pacific/Guadalcanal" },
    { L"South_Georgia Standard Time", "Atlantic/South_Georgia" },
    { L"Suriname Standard Time", "America/Paramaribo" },
    { L"Sverdlovsk Standard Time", "Asia/Yekaterinburg" },
    { L"Syowa Standard Time", "Antarctica/Syowa" },
    { L"Tahiti Standard Time", "Pacific/Tahiti" },
    { L"Tajikistan Standard Time", "Asia/Dushanbe" },
    { L"Tashkent Standard Time", "Asia/Tashkent" },
    { L"Tbilisi Standard Time", "Asia/Tbilisi" },
    { L"Tokelau Standard Time", "Pacific/Fakaofo" },
    { L"Truk Standard Time", "Pacific/Truk" },
    { L"Turkey Standard Time", "Europe/Istanbul" },
    { L"Turkmenistan Standard Time", "Asia/Ashgabat" },
    { L"Tuvalu Standard Time", "Pacific/Funafuti" },
    { L"Uralsk Standard Time", "Asia/Oral" },
    { L"Uruguay Standard Time", "America/Montevideo" },
    { L"Urumqi Standard Time", "Asia/Urumqi" },
    { L"Uzbekistan Standard Time", "Asia/Tashkent" },
    { L"Vanuatu Standard Time", "Pacific/Efate" },
    { L"Volgograd Standard Time", "Europe/Volgograd" },
    { L"Vostok Standard Time", "Antarctica/Vostok" },
    { L"Wake Standard Time", "Pacific/Wake" },
    { L"Wallis Standard Time", "Pacific/Wallis" },
    { L"Yekaterinburg Standard Time", "Asia/Yekaterinburg" },
    { L"Yerevan Standard Time", "Asia/Yerevan" },
    { L"Yukon Standard Time", "America/Yakutat" },

/* 2015 additions, seen in then-current Windows 7 */
    { L"Argentina Standard Time", "America/Buenos_Aires" }
    { L"Bahia Standard Time", "America/Bahia" }
    { L"Belarus Standard Time", "Europe/Minsk" }
    { L"Kaliningrad Standard Time", "Europe/Kaliningrad" },
    { L"Kamchatka Standard Time", "Asia/Kamchatka" }
    { L"Libya Standard Time", "Africa/Tripoli" }
    { L"Morocco Standard Time", "Africa/Casablanca" }
    { L"Syria Standard Time", "Asia/Damascus" }
    { L"Ulaanbaatar Standard Time", "Asia/Ulaanbaatar" }
    { L"Russia Time Zone 3", "Europe/Samara" }
    { L"Russia Time Zone 10", "Asia/Srednekolymsk" }
    { L"Russia Time Zone 11", "Asia/Kamchatka" }

    { NULL,  "" }
};

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wchar.h>
#include <stdlib.h> /* for wcstombs */

/* #define DEBUG */

#ifdef DEBUG
# include <stdio.h>
#endif
    

extern void Rf_warning(const char *, ...);

/* Since the registry entries can be localized and we might not be
   running in the native charset, use Unicode here. */

static wchar_t basekey[] = 
  L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\";
static char tzikey[] = 
   "SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation";

static const char *reg2Olson(const wchar_t *s)
{
    int i;
    LONG rc;
    HKEY hkey;
    DWORD type, size;
    BYTE regdata[1000]; /* 64 was not enough on Vista */

    /* Try for English names first */
    for (i = 0; TZtable[i].reg; i++)
	if (!wcscmp(s, TZtable[i].reg)) return TZtable[i].Olson;

    /* On Vista, the English name of the current timezone is in the registry */
    if(RegOpenKeyEx(HKEY_LOCAL_MACHINE, tzikey, 0, KEY_QUERY_VALUE, &hkey)
       == ERROR_SUCCESS) {
	size = 1000;
	rc = RegQueryValueExW(hkey, L"TimeZoneKeyName", NULL, &type, 
			      regdata, &size);
	RegCloseKey(hkey);
	if(rc == ERROR_SUCCESS) {
	    for (i = 0; TZtable[i].reg; i++)
		if (!wcscmp((wchar_t *) regdata, TZtable[i].reg))
		    return TZtable[i].Olson;
	}
    }

    {
	wchar_t keyname[100];
	/* That failed, so we are not on English Windows, nor on
	   Vista, probably.  However, the registry time zone key names
	   are in English and a subset of the above list (and need to
	   be for use to identify an Olson name).  So try them all for
	   a match to StandardName.
	*/
	for (i = 0; TZtable[i].reg; i++) {
	    /* Retrieve
	       HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\TZtable[i].reg\Std
	    */
	    wcscpy(keyname, basekey);
	    wcscat(keyname, TZtable[i].reg);
	    rc = RegOpenKeyExW(HKEY_LOCAL_MACHINE, keyname, 0, 
			       KEY_QUERY_VALUE, &hkey);
	    if (rc != ERROR_SUCCESS) continue;
	    size = 1000;
	    rc = RegQueryValueExW(hkey, L"Std", NULL, &type, regdata, &size);
	    RegCloseKey(hkey);
	    if (rc != ERROR_SUCCESS) continue;
	    if (!wcscmp(s, (wchar_t *) regdata))  return TZtable[i].Olson;
	}
    }
    
    Rf_warning("unable to identify current timezone '%s':\nplease set environment variable 'TZ'", s);
    return "unknown";
}


static TIME_ZONE_INFORMATION tzi;
/* Longest currently is 31 chars */
static char StandardName[64], DaylightName[64], Olson[64] = "";

/* Uwe Ligges says these entries are localized in the Windows' base language */
const char *getTZinfo(void)
{
    if(!Olson[0]) {
	GetTimeZoneInformation(&tzi);
	wcstombs(StandardName, tzi.StandardName, 64);
	wcstombs(DaylightName, tzi.DaylightName, 64);
	strcpy(Olson, reg2Olson(tzi.StandardName));
#ifdef DEBUG
	printf("names %s, %s\n", StandardName, DaylightName);
	printf("TZ = %s\n", Olson);
#endif
    }
    return Olson;
}
