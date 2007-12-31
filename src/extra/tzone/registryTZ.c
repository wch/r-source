/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007   The R Development Core Team
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
 */
const static struct {
    const char * const reg;
    const char * const unix;
} TZtable[] = {
    { "AUS Central", "Australia/Darwin" },
    { "AUS Eastern", "Australia/Sydney" },
    { "Acre", "America/Rio_Branco" },
    { "Afghanistan", "Asia/Kabul" },
    { "Africa_Central", "Africa/Kigali" },
    { "Africa_Eastern", "Africa/Kampala" },
    { "Africa_FarWestern", "Africa/El_Aaiun" },
    { "Africa_Southern", "Africa/Johannesburg" },
    { "Africa_Western", "Africa/Niamey" },
    { "Aktyubinsk", "Asia/Aqtobe" },
    { "Alaska", "America/Juneau" },
    { "Alaska_Hawaii", "America/Anchorage" },
    { "Alaskan", "America/Anchorage" },
    { "Almaty", "Asia/Almaty" },
    { "Amazon", "America/Manaus" },
    { "America_Central", "America/Winnipeg" },
    { "America_Eastern", "America/Panama" },
    { "America_Mountain", "America/Edmonton" },
    { "America_Pacific", "America/Vancouver" },
    { "Anadyr", "Asia/Anadyr" },
    { "Aqtau", "Asia/Aqtau" },
    { "Aqtobe", "Asia/Aqtobe" },
    { "Arab", "Asia/Riyadh" },
    { "Arabian", "Asia/Bahrain" },
    { "Arabic", "Asia/Baghdad" },
    { "Argentina", "America/Buenos_Aires" },
    { "Argentina_Western", "America/Mendoza" },
    { "Armenia", "Asia/Yerevan" },
    { "Ashkhabad", "Asia/Ashgabat" },
    { "Atlantic", "America/Curacao" },
    { "Australia_Central", "Australia/Adelaide" },
    { "Australia_CentralWestern", "Australia/Eucla" },
    { "Australia_Eastern", "Australia/Sydney" },
    { "Australia_Western", "Australia/Perth" },
    { "Azerbaijan", "Asia/Baku" },
    { "Azores", "Atlantic/Azores" },
    { "Baku", "Asia/Baku" },
    { "Bangladesh", "Asia/Dhaka" },
    { "Bering", "America/Adak" },
    { "Bhutan", "Asia/Thimphu" },
    { "Bolivia", "America/La_Paz" },
    { "Borneo", "Asia/Kuching" },
    { "Brasilia", "America/Sao_Paulo" },
    { "British", "Europe/London" },
    { "Brunei", "Asia/Brunei" },
    { "Canada Central", "America/Regina" },
    { "Cape Verde", "Atlantic/Cape_Verde" },
    { "Caucasus", "Asia/Yerevan" },
    { "Cen. Australia", "Australia/Adelaide" },
    { "Central", "America/Chicago" },
    { "Central America", "America/Guatemala" },
    { "Central Asia", "Asia/Dhaka" },
    { "Central Brazilian", "America/Manaus" },
    { "Central Europe", "Europe/Prague" },
    { "Central European", "Europe/Warsaw" },
    { "Central Pacific", "Pacific/Guadalcanal" },
    { "Central Standard Time (Mexico)", "America/Mexico_City" },
    { "Chamorro", "Pacific/Guam" },
    { "Changbai", "Asia/Harbin" },
    { "Chatham", "Pacific/Chatham" },
    { "Chile", "America/Santiago" },
    { "China", "Asia/Taipei" },
    { "Choibalsan", "Asia/Choibalsan" },
    { "Christmas", "Indian/Christmas" },
    { "Cocos", "Indian/Cocos" },
    { "Colombia", "America/Bogota" },
    { "Cook", "Pacific/Rarotonga" },
    { "Cuba", "America/Havana" },
    { "Dacca", "Asia/Dhaka" },
    { "Dateline", "Pacific/Kwajalein" },
    { "Davis", "Antarctica/Davis" },
    { "Dominican", "America/Santo_Domingo" },
    { "DumontDUrville", "Antarctica/DumontDUrville" },
    { "Dushanbe", "Asia/Dushanbe" },
    { "Dutch_Guiana", "America/Paramaribo" },
    { "E. Africa", "Africa/Nairobi" },
    { "E. Australia", "Australia/Brisbane" },
    { "E. Europe", "Europe/Minsk" },
    { "E. South America", "America/Sao_Paulo" },
    { "East_Timor", "Asia/Dili" },
    { "Easter", "Pacific/Easter" },
    { "Eastern", "America/New_York" },
    { "Ecuador", "America/Guayaquil" },
    { "Egypt", "Africa/Cairo" },
    { "Ekaterinburg", "Asia/Yekaterinburg" },
    { "Europe_Central", "Europe/Oslo" },
    { "Europe_Eastern", "Europe/Vilnius" },
    { "Europe_Western", "Atlantic/Canary" },
    { "FLE", "Europe/Helsinki" },
    { "Falkland", "Atlantic/Stanley" },
    { "Fiji", "Pacific/Fiji" },
    { "French_Guiana", "America/Cayenne" },
    { "French_Southern", "Indian/Kerguelen" },
    { "Frunze", "Asia/Bishkek" },
    { "GMT", "Europe/London" },
    { "GTB", "Europe/Istanbul" },
    { "Galapagos", "Pacific/Galapagos" },
    { "Gambier", "Pacific/Gambier" },
    { "Georgia", "Asia/Tbilisi" },
    { "Georgian", "Asia/Tbilisi" },
    { "Gilbert_Islands", "Pacific/Tarawa" },
    { "Goose_Bay", "America/Goose_Bay" },
    { "Greenland", "America/Godthab" },
    { "Greenland_Central", "America/Scoresbysund" },
    { "Greenland_Eastern", "America/Scoresbysund" },
    { "Greenland_Western", "America/Godthab" },
    { "Greenwich", "Africa/Casablanca" },
    { "Guam", "Pacific/Guam" },
    { "Gulf", "Asia/Muscat" },
    { "Guyana", "America/Guyana" },
    { "Hawaii_Aleutian", "Pacific/Honolulu" },
    { "Hawaiian", "Pacific/Honolulu" },
    { "Hong_Kong", "Asia/Hong_Kong" },
    { "Hovd", "Asia/Hovd" },
    { "India", "Asia/Calcutta" },
    { "Indian_Ocean", "Indian/Chagos" },
    { "Indochina", "Asia/Vientiane" },
    { "Indonesia_Central", "Asia/Makassar" },
    { "Indonesia_Eastern", "Asia/Jayapura" },
    { "Indonesia_Western", "Asia/Jakarta" },
    { "Iran", "Asia/Tehran" },
    { "Irish", "Europe/Dublin" },
    { "Irkutsk", "Asia/Irkutsk" },
    { "Israel", "Asia/Jerusalem" },
    { "Japan", "Asia/Tokyo" },
    { "Jordan", "Asia/Amman" },
    { "Kamchatka", "Asia/Kamchatka" },
    { "Karachi", "Asia/Karachi" },
    { "Kashgar", "Asia/Kashgar" },
    { "Kazakhstan_Eastern", "Asia/Almaty" },
    { "Kazakhstan_Western", "Asia/Aqtobe" },
    { "Kizilorda", "Asia/Qyzylorda" },
    { "Korea", "Asia/Seoul" },
    { "Kosrae", "Pacific/Kosrae" },
    { "Krasnoyarsk", "Asia/Krasnoyarsk" },
    { "Kuybyshev", "Europe/Samara" },
    { "Kwajalein", "Pacific/Kwajalein" },
    { "Kyrgystan", "Asia/Bishkek" },
    { "Lanka", "Asia/Colombo" },
    { "Liberia", "Africa/Monrovia" },
    { "Line_Islands", "Pacific/Kiritimati" },
    { "Long_Shu", "Asia/Chongqing" },
    { "Lord_Howe", "Australia/Lord_Howe" },
    { "Macau", "Asia/Macau" },
    { "Magadan", "Asia/Magadan" },
    { "Malaya", "Asia/Kuala_Lumpur" },
    { "Malaysia", "Asia/Kuching" },
    { "Maldives", "Indian/Maldives" },
    { "Marquesas", "Pacific/Marquesas" },
    { "Marshall_Islands", "Pacific/Majuro" },
    { "Mauritius", "Indian/Mauritius" },
    { "Mawson", "Antarctica/Mawson" },
    { "Mexico", "America/Mexico_City" },
    { "Mexico Standard Time 2", "America/Chihuahua" },
    { "Mid-Atlantic", "America/Noronha" },
    { "Middle East", "Asia/Beirut" },
    { "Mongolia", "Asia/Ulaanbaatar" },
    { "Montevideo", "America/Montevideo" },
    { "Moscow", "Europe/Moscow" },
    { "Mountain", "America/Denver" },
    { "Mountain Standard Time (Mexico)", "America/Chihuahua" },
    { "Myanmar", "Asia/Rangoon" },
    { "N. Central Asia", "Asia/Novosibirsk" },
    { "Namibia", "Africa/Windhoek" },
    { "Nauru", "Pacific/Nauru" },
    { "Nepal", "Asia/Katmandu" },
    { "New Zealand", "Pacific/Auckland" },
    { "New_Caledonia", "Pacific/Noumea" },
    { "New_Zealand", "Pacific/Auckland" },
    { "Newfoundland", "America/St_Johns" },
    { "Niue", "Pacific/Niue" },
    { "Norfolk", "Pacific/Norfolk" },
    { "Noronha", "America/Noronha" },
    { "North Asia", "Asia/Krasnoyarsk" },
    { "North Asia East", "Asia/Ulaanbaatar" },
    { "North_Mariana", "Pacific/Saipan" },
    { "Novosibirsk", "Asia/Novosibirsk" },
    { "Omsk", "Asia/Omsk" },
    { "Oral", "Asia/Oral" },
    { "Pacific", "America/Los_Angeles" },
    { "Pacific SA", "America/Santiago" },
    { "Pacific Standard Time (Mexico)", "America/Tijuana" },
    { "Pakistan", "Asia/Karachi" },
    { "Palau", "Pacific/Palau" },
    { "Papua_New_Guinea", "Pacific/Port_Moresby" },
    { "Paraguay", "America/Asuncion" },
    { "Peru", "America/Lima" },
    { "Philippines", "Asia/Manila" },
    { "Phoenix_Islands", "Pacific/Enderbury" },
    { "Pierre_Miquelon", "America/Miquelon" },
    { "Pitcairn", "Pacific/Pitcairn" },
    { "Ponape", "Pacific/Ponape" },
    { "Qyzylorda", "Asia/Qyzylorda" },
    { "Reunion", "Indian/Reunion" },
    { "Romance", "Europe/Paris" },
    { "Rothera", "Antarctica/Rothera" },
    { "Russian", "Europe/Moscow" },
    { "SA Eastern", "America/Buenos_Aires" },
    { "SA Pacific", "America/Bogota" },
    { "SA Western", "America/Caracas" },
    { "SE Asia", "Asia/Bangkok" },
    { "Sakhalin", "Asia/Sakhalin" },
    { "Samara", "Europe/Samara" },
    { "Samarkand", "Asia/Samarkand" },
    { "Samoa", "Pacific/Apia" },
    { "Seychelles", "Indian/Mahe" },
    { "Shevchenko", "Asia/Aqtau" },
    { "Singapore", "Asia/Singapore" },
    { "Solomon", "Pacific/Guadalcanal" },
    { "South Africa", "Africa/Johannesburg" },
    { "South_Georgia", "Atlantic/South_Georgia" },
    { "Sri Lanka", "Asia/Colombo" },
    { "Suriname", "America/Paramaribo" },
    { "Sverdlovsk", "Asia/Yekaterinburg" },
    { "Syowa", "Antarctica/Syowa" },
    { "Tahiti", "Pacific/Tahiti" },
    { "Taipei", "Asia/Taipei" },
    { "Tajikistan", "Asia/Dushanbe" },
    { "Tashkent", "Asia/Tashkent" },
    { "Tasmania", "Australia/Hobart" },
    { "Tbilisi", "Asia/Tbilisi" },
    { "Tokelau", "Pacific/Fakaofo" },
    { "Tokyo", "Asia/Tokyo" },
    { "Tonga", "Pacific/Tongatapu" },
    { "Truk", "Pacific/Truk" },
    { "Turkey", "Europe/Istanbul" },
    { "Turkmenistan", "Asia/Ashgabat" },
    { "Tuvalu", "Pacific/Funafuti" },
    { "US Eastern", "America/Indianapolis" },
    { "US Mountain", "America/Phoenix" },
    { "Uralsk", "Asia/Oral" },
    { "Uruguay", "America/Montevideo" },
    { "Urumqi", "Asia/Urumqi" },
    { "Uzbekistan", "Asia/Tashkent" },
    { "Vanuatu", "Pacific/Efate" },
    { "Venezuela", "America/Caracas" },
    { "Vladivostok", "Asia/Vladivostok" },
    { "Volgograd", "Europe/Volgograd" },
    { "Vostok", "Antarctica/Vostok" },
    { "W. Australia", "Australia/Perth" },
    { "W. Central Africa", "Africa/Lagos" },
    { "W. Europe", "Europe/Berlin" },
    { "Wake", "Pacific/Wake" },
    { "Wallis", "Pacific/Wallis" },
    { "West Asia", "Asia/Karachi" },
    { "West Pacific", "Pacific/Guam" },
    { "Yakutsk", "Asia/Yakutsk" },
    { "Yekaterinburg", "Asia/Yekaterinburg" },
    { "Yerevan", "Asia/Yerevan" },
    { "Yukon", "America/Yakutat" }
};

extern void Rf_warning(const char *, ...);

static const char *reg2unix(const char *s)
{
    int i;
    for (i = 0; TZtable[i].reg; i++)
	if (!strcmp(s, TZtable[i].reg)) return TZtable[i].unix;
    Rf_warning("unable to identify current timezone '%s':\nplease set environment variable 'TZ'", s);
    return "unknown";
}

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wchar.h>

static TIME_ZONE_INFORMATION tzi;
static char StandardName[32], DaylightName[32], TZID[32];

#ifdef DEBUG
# include <stdio.h>
#endif
    
const char *getTZinfo(void)
{
    char *p;

    GetTimeZoneInformation(&tzi);
    wcstombs(StandardName, tzi.StandardName, 32);
    wcstombs(DaylightName, tzi.DaylightName, 32);
    strcpy(TZID, StandardName);
    p = strstr(TZID, " Standard Time");
    if(p) *p = '\0';
#ifdef DEBUG
    printf ("bias %d mins\n", (int) Bias);
    printf("names %s, %s\n", StandardName, DaylightName);
    printf("TZ = %s\n", reg2unix(TZID));
#endif
    return reg2unix(TZID);
}
