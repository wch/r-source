/* 
   Support routines for device GUI, copied from src/gnuwin32/devga.c
   Changes
   - PrivateCopyDevice: GraphApp specific code to set cursor modified
   - SaveAsPostscript: Set "command" so we can print
*/

#include <gnome.h>
#include "Defn.h"
#include "Graphics.h"
#include "Rdevices.h"
#include "devGNOME.h"
#include "device-support.h"

static void R_ShowMessage(char *s)
{
  if (!s) return;
  R_WriteConsole(s, strlen(s));
}

/* WARNING:  This code is base-graphics(-GRZ)-specific
 */
static void PrivateCopyDevice(NewDevDesc *dd, NewDevDesc *ndev, char *name)
{
    GEDevDesc* ndd;
  R_Busy(TRUE);
  gsetVar(install(".Device"), mkString(name), R_NilValue);
  ndd = GEcreateDevDesc(ndev);
  addDevice((DevDesc*) ndd);
  GEcopyDisplayList(devNumber((DevDesc*) dd));
  KillDevice((DevDesc*) ndd);
  R_Busy(FALSE);
}   

static void GetPSOption (char *buf, const SEXP s, const char *name,
			 const char *def)
{
  SEXP names;
  int i;

  /* Set default value */
  strcpy(buf, def);

  /* Then try to get it from s */
  names = getAttrib(s, R_NamesSymbol);
  for (i=0; i<length(s); i++) {
    if(!strcmp(name, CHAR(STRING_ELT(names, i)))) {
      strcpy(buf, CHAR(STRING_ELT(VECTOR_ELT(s, i), 0)));
    }
  }
}

void SaveAsPostscript(NewDevDesc *dd, char *fn)
{
  SEXP s = findVar(install(".PostScript.Options"), R_GlobalEnv);

  NewDevDesc *ndev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc));
  GEDevDesc* gdd = (GEDevDesc*) GetDevice(devNumber((DevDesc*) dd));
  char family[256], encoding[256], paper[256], bg[256], fg[256], 
    command[256], **afmpaths = NULL;
  
  if (!ndev) {
    R_ShowMessage("Not enough memory to copy graphics window");
    return;
  }
  if(!R_CheckDeviceAvailableBool()) {
    free(ndev);
    R_ShowMessage("No device available to copy graphics window");
    return;
  }

  ndev->displayList = R_NilValue;

  /* Set default values... */
  strcpy(encoding, "ISOLatin1.enc"); /*FIXME: should be machine dependent */
  /* Try to get values from .PostScript.Options */
  GetPSOption(family, s, "family", "Helvetica");
  GetPSOption(paper, s, "paper", "default");
  GetPSOption(bg, s, "bg", "white");
  GetPSOption(fg, s, "fg", "black");
  GetPSOption(command, s, "command", "default");
  /* if print command isn't set in .PostScript.Options, get if from options */
  if (!strcmp(command, "default")) {
    char *cmd = (char*)CHAR(STRING_ELT(GetOption(install("printcmd"),
						 R_NilValue), 0));
    printf("%s", cmd);
    strcpy(command, cmd);
  }

  if (PSDeviceDriver((DevDesc*) ndev, 
		     fn, paper, family, afmpaths, encoding, bg, fg,
		     fromDeviceWidth(toDeviceWidth(1.0, GE_NDC, gdd), 
				     GE_INCHES, gdd),
		     fromDeviceHeight(toDeviceHeight(-1.0, GE_NDC, gdd), 
				      GE_INCHES, gdd),
		     (double)0, ((gnomeDesc*) dd->deviceSpecific)->fontsize,
		     0, 1, 0, command))
    /* horizontal=F, onefile=F, pagecentre=T, print.it=F */
    PrivateCopyDevice(dd, ndev, "postscript");
}

