/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-8   The R Development Core Team
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

#include <string.h>

/*
  From http://unicode.org/cldr/data/diff/supplemental/windows_tzid.html
  Added some entries from the XP Registry.
 */
const static struct {
    const char * const reg;
    const char * const Olson;
} TZtable[] = {
    { "Afghanistan Standard Time", "Asia/Kabul" },
    { "Alaskan Standard Time", "America/Anchorage" },
    { "Arab Standard Time", "Asia/Riyadh" },
    { "Arabian Standard Time", "Asia/Bahrain" },
    { "Arabic Standard Time", "Asia/Baghdad" },
    { "Armenian Standard Time", "Asia/Yerevan" },
    { "Atlantic Standard Time", "America/Curacao" },
    { "AUS Central Standard Time", "Australia/Darwin" },
    { "AUS Eastern Standard Time", "Australia/Sydney" },
    { "Azerbaijan Standard Time", "Asia/Baku" },
    { "Azores Standard Time", "Atlantic/Azores" },
    { "Canada Central Standard Time", "America/Regina" },
    { "Cape Verde Standard Time", "Atlantic/Cape_Verde" },
    { "Caucasus Standard Time", "Asia/Yerevan" },
    { "Cen. Australia Standard Time", "Australia/Adelaide" },
    { "Central America Standard Time", "America/Guatemala" },
    { "Central Asia Standard Time", "Asia/Dhaka" },
    { "Central Brazilian Standard Time", "America/Manaus" },
    { "Central Europe Standard Time", "Europe/Prague" },
    { "Central European Standard Time", "Europe/Warsaw" },
    { "Central Pacific Standard Time", "Pacific/Guadalcanal" },
    { "Central Standard Time", "America/Chicago" },
    { "Central Standard Time (Mexico)", "America/Mexico_City" },
    { "China Standard Time", "Asia/Taipei" },
    { "Dateline Standard Time", "Pacific/Kwajalein" },
    { "E. Africa Standard Time", "Africa/Nairobi" },
    { "E. Australia Standard Time", "Australia/Brisbane" },
    { "E. Europe Standard Time", "Europe/Minsk" },
    { "E. South America Standard Time", "America/Sao_Paulo" },
    { "Eastern Standard Time", "America/New_York" },
    { "Egypt Standard Time", "Africa/Cairo" },
    { "Ekaterinburg Standard Time", "Asia/Yekaterinburg" },
    { "Fiji Standard Time", "Pacific/Fiji" },
    { "FLE Standard Time", "Europe/Helsinki" },
    { "Georgian Standard Time", "Asia/Tbilisi" },
    { "GMT Standard Time", "Europe/London" },
    { "Greenland Standard Time", "America/Godthab" },
    { "Greenwich Standard Time", "Africa/Casablanca" },
    { "GTB Standard Time", "Europe/Istanbul" },
    { "Hawaiian Standard Time", "Pacific/Honolulu" },
    { "India Standard Time", "Asia/Calcutta" },
    { "Iran Standard Time", "Asia/Tehran" },
    /* next is name of timezone, not of Std */
    { "Israel Standard Time", "Asia/Jerusalem" },
    { "Jerusalem Standard Time", "Asia/Jerusalem" },
    { "Jordan Standard Time", "Asia/Amman" },
    { "Korea Standard Time", "Asia/Seoul" },
    { "Malay Peninsula Standard Time", "Asia/Kuala_Lumpur" },
    { "Mexico Standard Time", "America/Mexico_City" },
    { "Mexico Standard Time 2", "America/Chihuahua" },
    { "Mid-Atlantic Standard Time", "America/Noronha" },
    { "Middle East Standard Time", "Asia/Beirut" },
    { "Montevideo Standard Time", "America/Montevideo" },
    { "Mountain Standard Time", "America/Denver" },
    { "Mountain Standard Time (Mexico)", "America/Chihuahua" },
    { "Myanmar Standard Time", "Asia/Rangoon" },
    { "N. Central Asia Standard Time", "Asia/Novosibirsk" },
    { "Namibia Standard Time", "Africa/Windhoek" },
    { "Nepal Standard Time", "Asia/Katmandu" },
    { "New Zealand Standard Time", "Pacific/Auckland" },
    { "Newfoundland Standard Time", "America/St_Johns" },
    { "North Asia East Standard Time", "Asia/Ulaanbaatar" },
    { "North Asia Standard Time", "Asia/Krasnoyarsk" },
    { "Pacific SA Standard Time", "America/Santiago" },
    { "Pacific Standard Time", "America/Los_Angeles" },
    { "Pacific Standard Time (Mexico)", "America/Tijuana" },
    { "Romance Standard Time", "Europe/Paris" },
    { "Russian Standard Time", "Europe/Moscow" },
    { "SA Eastern Standard Time", "America/Buenos_Aires" },
    { "SA Pacific Standard Time", "America/Bogota" },
    { "SA Western Standard Time", "America/Caracas" },
    { "Samoa Standard Time", "Pacific/Apia" },
    { "SE Asia Standard Time", "Asia/Bangkok" },
    /* next is name of timezone, not of Std */
    { "Singapore Standard Time", "Asia/Kuala_Lumpur" },
    { "South Africa Standard Time", "Africa/Johannesburg" },
    { "Sri Lanka Standard Time", "Asia/Colombo" },
    { "Taipei Standard Time", "Asia/Taipei" },
    { "Tasmania Standard Time", "Australia/Hobart" },
    { "Tokyo Standard Time", "Asia/Tokyo" },
    { "Tonga Standard Time", "Pacific/Tongatapu" },
    { "US Eastern Standard Time", "America/Indianapolis" },
    { "US Mountain Standard Time", "America/Phoenix" },
    { "Venezuela Standard Time", "America/Caracas" },
    { "Vladivostok Standard Time", "Asia/Vladivostok" },
    { "W. Australia Standard Time", "Australia/Perth" },
    { "W. Central Africa Standard Time", "Africa/Lagos" },
    { "W. Europe Standard Time", "Europe/Berlin" },
    { "West Asia Standard Time", "Asia/Karachi" },
    { "West Pacific Standard Time", "Pacific/Guam" },
    { "Yakutsk Standard Time", "Asia/Yakutsk" },

/* The ones below do not occur on BDR's XP machine */
    { "Acre Standard Time", "America/Rio_Branco" },
    { "Africa_Central Standard Time", "Africa/Kigali" },
    { "Africa_Eastern Standard Time", "Africa/Kampala" },
    { "Africa_FarWestern Standard Time", "Africa/El_Aaiun" },
    { "Africa_Southern Standard Time", "Africa/Johannesburg" },
    { "Africa_Western Standard Time", "Africa/Niamey" },
    { "Aktyubinsk Standard Time", "Asia/Aqtobe" },
    { "Alaska Standard Time", "America/Juneau" },
    { "Alaska_Hawaii Standard Time", "America/Anchorage" },
    { "Almaty Standard Time", "Asia/Almaty" },
    { "Amazon Standard Time", "America/Manaus" },
    { "America_Central Standard Time", "America/Winnipeg" },
    { "America_Eastern Standard Time", "America/Panama" },
    { "America_Mountain Standard Time", "America/Edmonton" },
    { "America_Pacific Standard Time", "America/Vancouver" },
    { "Anadyr Standard Time", "Asia/Anadyr" },
    { "Aqtau Standard Time", "Asia/Aqtau" },
    { "Aqtobe Standard Time", "Asia/Aqtobe" },
    { "Argentina Standard Time", "America/Buenos_Aires" },
    { "Argentina_Western Standard Time", "America/Mendoza" },
    { "Armenia Standard Time", "Asia/Yerevan" },
    { "Ashkhabad Standard Time", "Asia/Ashgabat" },
    { "Australia_Central Standard Time", "Australia/Adelaide" },
    { "Australia_CentralWestern Standard Time", "Australia/Eucla" },
    { "Australia_Eastern Standard Time", "Australia/Sydney" },
    { "Australia_Western Standard Time", "Australia/Perth" },
    { "Baku Standard Time", "Asia/Baku" },
    { "Bangladesh Standard Time", "Asia/Dhaka" },
    { "Bering Standard Time", "America/Adak" },
    { "Bhutan Standard Time", "Asia/Thimphu" },
    { "Bolivia Standard Time", "America/La_Paz" },
    { "Borneo Standard Time", "Asia/Kuching" },
    { "Brasilia Standard Time", "America/Sao_Paulo" },
    { "British Standard Time", "Europe/London" },
    { "Brunei Standard Time", "Asia/Brunei" },
    { "Chamorro Standard Time", "Pacific/Guam" },
    { "Changbai Standard Time", "Asia/Harbin" },
    { "Chatham Standard Time", "Pacific/Chatham" },
    { "Chile Standard Time", "America/Santiago" },
    { "Choibalsan Standard Time", "Asia/Choibalsan" },
    { "Christmas Standard Time", "Indian/Christmas" },
    { "Cocos Standard Time", "Indian/Cocos" },
    { "Colombia Standard Time", "America/Bogota" },
    { "Cook Standard Time", "Pacific/Rarotonga" },
    { "Cuba Standard Time", "America/Havana" },
    { "Dacca Standard Time", "Asia/Dhaka" },
    { "Davis Standard Time", "Antarctica/Davis" },
    { "Dominican Standard Time", "America/Santo_Domingo" },
    { "DumontDUrville Standard Time", "Antarctica/DumontDUrville" },
    { "Dushanbe Standard Time", "Asia/Dushanbe" },
    { "Dutch_Guiana Standard Time", "America/Paramaribo" },
    { "East_Timor Standard Time", "Asia/Dili" },
    { "Easter Standard Time", "Pacific/Easter" },
    { "Ecuador Standard Time", "America/Guayaquil" },
    { "Europe_Central Standard Time", "Europe/Oslo" },
    { "Europe_Eastern Standard Time", "Europe/Vilnius" },
    { "Europe_Western Standard Time", "Atlantic/Canary" },
    { "Falkland Standard Time", "Atlantic/Stanley" },
    { "French_Guiana Standard Time", "America/Cayenne" },
    { "French_Southern Standard Time", "Indian/Kerguelen" },
    { "Frunze Standard Time", "Asia/Bishkek" },
    { "Galapagos Standard Time", "Pacific/Galapagos" },
    { "Gambier Standard Time", "Pacific/Gambier" },
    { "Georgia Standard Time", "Asia/Tbilisi" },
    { "Gilbert_Islands Standard Time", "Pacific/Tarawa" },
    { "Goose_Bay Standard Time", "America/Goose_Bay" },
    { "Greenland_Central Standard Time", "America/Scoresbysund" },
    { "Greenland_Eastern Standard Time", "America/Scoresbysund" },
    { "Greenland_Western Standard Time", "America/Godthab" },
    { "Guam Standard Time", "Pacific/Guam" },
    { "Gulf Standard Time", "Asia/Muscat" },
    { "Guyana Standard Time", "America/Guyana" },
    { "Hawaii_Aleutian Standard Time", "Pacific/Honolulu" },
    { "Hong_Kong Standard Time", "Asia/Hong_Kong" },
    { "Hovd Standard Time", "Asia/Hovd" },
    { "Indian_Ocean Standard Time", "Indian/Chagos" },
    { "Indochina Standard Time", "Asia/Vientiane" },
    { "Indonesia_Central Standard Time", "Asia/Makassar" },
    { "Indonesia_Eastern Standard Time", "Asia/Jayapura" },
    { "Indonesia_Western Standard Time", "Asia/Jakarta" },
    { "Irish Standard Time", "Europe/Dublin" },
    { "Irkutsk Standard Time", "Asia/Irkutsk" },
    { "Japan Standard Time", "Asia/Tokyo" },
    { "Kamchatka Standard Time", "Asia/Kamchatka" },
    { "Karachi Standard Time", "Asia/Karachi" },
    { "Kashgar Standard Time", "Asia/Kashgar" },
    { "Kazakhstan_Eastern Standard Time", "Asia/Almaty" },
    { "Kazakhstan_Western Standard Time", "Asia/Aqtobe" },
    { "Kizilorda Standard Time", "Asia/Qyzylorda" },
    { "Kosrae Standard Time", "Pacific/Kosrae" },
    { "Krasnoyarsk Standard Time", "Asia/Krasnoyarsk" },
    { "Kuybyshev Standard Time", "Europe/Samara" },
    { "Kwajalein Standard Time", "Pacific/Kwajalein" },
    { "Kyrgystan Standard Time", "Asia/Bishkek" },
    { "Lanka Standard Time", "Asia/Colombo" },
    { "Liberia Standard Time", "Africa/Monrovia" },
    { "Line_Islands Standard Time", "Pacific/Kiritimati" },
    { "Long_Shu Standard Time", "Asia/Chongqing" },
    { "Lord_Howe Standard Time", "Australia/Lord_Howe" },
    { "Macau Standard Time", "Asia/Macau" },
    { "Magadan Standard Time", "Asia/Magadan" },
    { "Malaya Standard Time", "Asia/Kuala_Lumpur" },
    { "Malaysia Standard Time", "Asia/Kuching" },
    { "Maldives Standard Time", "Indian/Maldives" },
    { "Marquesas Standard Time", "Pacific/Marquesas" },
    { "Marshall_Islands Standard Time", "Pacific/Majuro" },
    { "Mauritius Standard Time", "Indian/Mauritius" },
    { "Mawson Standard Time", "Antarctica/Mawson" },
    { "Mongolia Standard Time", "Asia/Ulaanbaatar" },
    { "Moscow Standard Time", "Europe/Moscow" },
    { "Nauru Standard Time", "Pacific/Nauru" },
    { "New_Caledonia Standard Time", "Pacific/Noumea" },
    { "New_Zealand Standard Time", "Pacific/Auckland" },
    { "Niue Standard Time", "Pacific/Niue" },
    { "Norfolk Standard Time", "Pacific/Norfolk" },
    { "Noronha Standard Time", "America/Noronha" },
    { "North_Mariana Standard Time", "Pacific/Saipan" },
    { "Novosibirsk Standard Time", "Asia/Novosibirsk" },
    { "Omsk Standard Time", "Asia/Omsk" },
    { "Oral Standard Time", "Asia/Oral" },
    { "Pakistan Standard Time", "Asia/Karachi" },
    { "Palau Standard Time", "Pacific/Palau" },
    { "Papua_New_Guinea Standard Time", "Pacific/Port_Moresby" },
    { "Paraguay Standard Time", "America/Asuncion" },
    { "Peru Standard Time", "America/Lima" },
    { "Philippines Standard Time", "Asia/Manila" },
    { "Phoenix_Islands Standard Time", "Pacific/Enderbury" },
    { "Pierre_Miquelon Standard Time", "America/Miquelon" },
    { "Pitcairn Standard Time", "Pacific/Pitcairn" },
    { "Ponape Standard Time", "Pacific/Ponape" },
    { "Qyzylorda Standard Time", "Asia/Qyzylorda" },
    { "Reunion Standard Time", "Indian/Reunion" },
    { "Rothera Standard Time", "Antarctica/Rothera" },
    { "Sakhalin Standard Time", "Asia/Sakhalin" },
    { "Samara Standard Time", "Europe/Samara" },
    { "Samarkand Standard Time", "Asia/Samarkand" },
    { "Seychelles Standard Time", "Indian/Mahe" },
    { "Shevchenko Standard Time", "Asia/Aqtau" },
    { "Solomon Standard Time", "Pacific/Guadalcanal" },
    { "South_Georgia Standard Time", "Atlantic/South_Georgia" },
    { "Suriname Standard Time", "America/Paramaribo" },
    { "Sverdlovsk Standard Time", "Asia/Yekaterinburg" },
    { "Syowa Standard Time", "Antarctica/Syowa" },
    { "Tahiti Standard Time", "Pacific/Tahiti" },
    { "Tajikistan Standard Time", "Asia/Dushanbe" },
    { "Tashkent Standard Time", "Asia/Tashkent" },
    { "Tbilisi Standard Time", "Asia/Tbilisi" },
    { "Tokelau Standard Time", "Pacific/Fakaofo" },
    { "Truk Standard Time", "Pacific/Truk" },
    { "Turkey Standard Time", "Europe/Istanbul" },
    { "Turkmenistan Standard Time", "Asia/Ashgabat" },
    { "Tuvalu Standard Time", "Pacific/Funafuti" },
    { "Uralsk Standard Time", "Asia/Oral" },
    { "Uruguay Standard Time", "America/Montevideo" },
    { "Urumqi Standard Time", "Asia/Urumqi" },
    { "Uzbekistan Standard Time", "Asia/Tashkent" },
    { "Vanuatu Standard Time", "Pacific/Efate" },
    { "Volgograd Standard Time", "Europe/Volgograd" },
    { "Vostok Standard Time", "Antarctica/Vostok" },
    { "Wake Standard Time", "Pacific/Wake" },
    { "Wallis Standard Time", "Pacific/Wallis" },
    { "Yekaterinburg Standard Time", "Asia/Yekaterinburg" },
    { "Yerevan Standard Time", "Asia/Yerevan" },
    { "Yukon Standard Time", "America/Yakutat" }
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

static char basekey[] = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\";
static const char *reg2Olson(const char *s)
{
    int i;

    /* Try for English names first */
    for (i = 0; TZtable[i].reg; i++)
	if (!strcmp(s, TZtable[i].reg)) return TZtable[i].Olson;

    {
	LONG rc;
	HKEY hkey;
	DWORD type, size;
	
	char keyname[100];
	unsigned char regname[64];
	/* That failed, so we are not on English Windows, probably.
	   However, the registry time zone key names are in English and a
	   subset of the above list (and need to be for use to identify an
	   Olson name). So try them all for a match to Standard Name.
	*/
	for (i = 0; TZtable[i].reg; i++) {
	    /* Retrieve
	       HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones\TZtable[i].reg\Std
	    */
	    strcpy(keyname, basekey);
	    strcat(keyname, TZtable[i].reg);
	    rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, keyname, 0, 
			      KEY_QUERY_VALUE, &hkey);
	    if (rc != ERROR_SUCCESS) continue;
	    size = 64;
	    rc = RegQueryValueEx(hkey, "Std", NULL, &type, regname, &size);
	    RegCloseKey(hkey);
	    if (rc != ERROR_SUCCESS) continue;
	    if (!strcmp(s, (char *)regname))  return TZtable[i].Olson;
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
	strcpy(Olson, reg2Olson(StandardName));
#ifdef DEBUG
	printf("names %s, %s\n", StandardName, DaylightName);
	printf("TZ = %s\n", Olson);
#endif
    }
    return Olson;
}
