/*

  This file was adapted to R for MacOS by Stefano M. Iacus from the original distribution of:
  
  Copyright (c) 1990-2000 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2000-Apr-09 or later
  (the contents of which are also included in unzip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html
*/
/*****************************************************************************/
/*  Includes                                                                 */
/*****************************************************************************/

#include "UnZp.h"

#include <string.h>
#include "version.h"
#include <stdio.h>

/*****************************************************************************/
/*  Global Vars                                                              */
/*****************************************************************************/

char fileList[256];


/*****************************************************************************/
/*  Prototypes                                                               */
/*****************************************************************************/

int UzpMain(int argc,char **argv);

char *GetUnZipLocalVersion(void);
char *GetUnZipInfoVersions(void);
int  macgetch(void);
void UserStop(void);



/*****************************************************************************/
/*  Functions                                                                */
/*****************************************************************************/



char *GetUnZipLocalVersion(void)
{
static char UnZipVersionLocal[50];

memset(UnZipVersionLocal,0,sizeof(UnZipVersionLocal));

sprintf(UnZipVersionLocal, "[%s %s]", __DATE__, __TIME__);

return UnZipVersionLocal;
}




char *GetUnZipInfoVersions(void)
{
static char UnzipVersion[200];

memset(UnzipVersion,0,sizeof(UnzipVersion));

sprintf(UnzipVersion, "Unzip Module\n%d.%d%d%s of %s", UZ_MAJORVER,
        UZ_MINORVER, UZ_PATCHLEVEL, UZ_BETALEVEL, UZ_VERSION_DATE);

return UnzipVersion;
}
