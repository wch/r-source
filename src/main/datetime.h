/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2014  The R Core Team.
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
 *
 */

# include <stdint.h>
// C99/C11 require this to be implemented.
typedef int_fast64_t R_time_t;

struct Rtm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
  long tm_gmtoff;
  const char *tm_zone;
};

typedef struct Rtm stm;

#define time_t R_time_t
#define gmtime R_gmtime
#define gmtime_r R_gmtime_r
#define localtime R_localtime
#define localtime_r R_localtime_r
#define mktime R_mktime
#define tzset R_tzset
extern stm* Rgmtime (const R_time_t*);
extern stm* R_gmtime_r (const R_time_t*, stm*);
extern stm* R_localtime (const R_time_t*);
extern stm* R_localtime_r(const R_time_t*, stm*);
extern R_time_t R_mktime (stm*);
extern void R_tzset(void);
extern void R_tzsetwall(void);
extern char *R_tzname[2];
extern int_fast64_t R_timegm(stm*);

extern size_t
R_strftime(char * const s, const size_t maxsize, const char *const format,
	   const stm *const t);
