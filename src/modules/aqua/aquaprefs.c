/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2003  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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


#ifndef __AQUA_PREFS__
#define __AQUA_PREFS__

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif



#ifdef HAVE_AQUA
#define __DEBUGGING__
#include <Carbon/Carbon.h>

#include "Raqua.h"

extern WindowRef	RPrefsWindow;

pascal void RPrefsHandler(WindowRef window);
void ActivatePrefsWindow(void);
void DeactivatePrefsWindow(void);

void CopyPrefs(RAquaPrefsPointer From, RAquaPrefsPointer To);

void RSetTab(void);
void RSetFontSize(void);
void RSetFont(void);


RAquaPrefs DefaultPrefs;
RAquaPrefs CurrentPrefs, TempPrefs;
RAquaPrefs LastSavedPrefs;


  
FMFontFamilyInstance    instance;
FMFontSize              fontSize;       

void pickColor(RGBColor inColour, RGBColor *outColor);

static void SetInitialTabState(WindowRef theWindow);
                
void CallFontPanel(void);

        

/* R Preferences version */                
#define RAquaPrefsVer	3L

/* button ID for Preferences panel */
#define kConsoleFontButton  	1002
#define kConsoleFontText 	1003
#define kTabSizeField	    	1005
#define kOutputColorText 	1007
#define kInputColorText 	1008
#define kOutputColorButton  	1009
#define kInputColorButton   	1010
#define kOutputBackButton   	1011
#define kInputBackButton    	1012
#define kBufferingBox    	1013
#define kBufferSizeSlider       1014

#define	kDeviceFontButton   	3001
#define kDeviceFontText 	3002
#define kOverrideRDefBox	3007
#define kWidthField 		3008
#define kHeightField 		3009
#define kAntiAliasingBox 	3013
#define kAutoRefreshBox 	3014

#define kCRANmirrorText         4501
#define kBIOCmirrorText         4502
#define kGlobalPackagesBox      4503

#define kGrabStdoutBox		6000
#define kGrabStderrBox		6001

#define kApplyPrefsButton	5000
#define kCancelPrefsButton	5001 
#define kDefaultPrefsButton 	5002
#define kSavePrefsButton	5003

                
#define	kTabMasterSig		'PRTT'
#define	kTabMasterID		1000
#define	kTabPaneSig		'PRTB'
#define	kPrefControlsSig	'PREF'
#define kDummyValue		0
#define kMaxNumTabs		4

ControlID	MainControlID = { kPrefControlsSig, kConsoleFontText };
ControlID	ConsoleFontTextID = { kPrefControlsSig, kConsoleFontText };
ControlID	DeviceFontTextID = { kPrefControlsSig, kDeviceFontText };
ControlID	TabSizeID = { kPrefControlsSig, kTabSizeField };
ControlID	OverrideRDefBoxID = { kPrefControlsSig, kOverrideRDefBox };
ControlID	WidthID = { kPrefControlsSig, kWidthField };
ControlID	HeightID = { kPrefControlsSig, kHeightField };
ControlID	AutoRefreshID = { kPrefControlsSig, kAutoRefreshBox };
ControlID	AntiAliasingID = { kPrefControlsSig, kAntiAliasingBox };
ControlID	InputColorID = { kPrefControlsSig, kInputColorText };
ControlID	OutputColorID = { kPrefControlsSig, kOutputColorText };
ControlID	BufferingID = { kPrefControlsSig, kBufferingBox };
ControlID	BufferSizeID = { kPrefControlsSig, kBufferSizeSlider };
ControlID	CRANmirrorID = { kPrefControlsSig, kCRANmirrorText };
ControlID	BIOCmirrorID = { kPrefControlsSig, kBIOCmirrorText };
ControlID       GlobalPackagesID = {kPrefControlsSig, kGlobalPackagesBox };
ControlID       GrabStdoutID = {kPrefControlsSig, kGrabStdoutBox };
ControlID       GrabStderrID = {kPrefControlsSig, kGrabStderrBox };

static	char		DefaultConsoleFontName[] = "Courier";
static	int		DefaultConsoleFontSize = 14;
static	int		DefaultTabSize = 10;
static	RGBColor	DefaultFGInputColor = {0xffff, 0x0000, 0x0000};
static	RGBColor	DefaultBGInputColor = {0xffff, 0xffff, 0xffff};
static	RGBColor	DefaultFGOutputColor = {0x0000, 0x0000, 0xffff};
static	RGBColor	DefaultBGOutputColor = {0xffff, 0xffff, 0xffff};
static	char 		DefaultDeviceFontName[] = "Helvetica";
static	int		DefaultDevicePointSize = 12;
static	double 		DefaultDeviceWidth = 5.0;
static	double		DefaultDeviceHeight = 5.0;	
static	int		DefaultAntiAlias = 1;
static	int		DefaultAutoRefresh = 1;
static	int		DefaultOverrideRDefaults = 0;
static	int		DefaultBuffering = 1;
static  int             DefaultBufferSize = 10;
static  char            DefaultCRANmirror[] ="http://cran.r-project.org";
static  char            DefaultBIOCmirror[] ="http://www.bioconductor.org";
static  int             DefaultGlobalPackages = 0;
static  int             DefaultGrabStdout = 0;
static  int             DefaultGrabStderr = 0;

ControlRef GrabCRef(WindowRef theWindow,OSType theSig, SInt32 theNum);

		     
Boolean isConsoleFont = true;
             
OSStatus MySetFontSelection(void);                              
OSStatus MyGetFontSelection(EventRef event);
void RSetPackagePrefs(void);
void RSetPipes(void);
        
/* external symbols from aquaconsole.c */                                   
extern void RSetColors(void);
extern void consolecmd(char* cmd);
extern	void	OpenStdoutPipe();
extern	void	OpenStderrPipe();
extern	void	CloseStdoutPipe();
extern	void	CloseStderrPipe();

                    

EventTypeSpec	tabControlEvents[] = {
    { kEventClassControl, kEventControlHit },
    { kEventClassCommand, kEventCommandProcess }
};

EventTypeSpec	RPrefsSpec[] = { 
    { kEventClassWindow, kEventWindowClose }
};

EventTypeSpec	okcanxControlEvents[] = {
    { kEventClassControl, kEventControlHit }
};


static pascal OSStatus RPrefsWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);
static pascal OSStatus PrefsTabEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
static  OSStatus GenContEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );

void SetDefaultPrefs(void);
void SetUpPrefsWindow(RAquaPrefsPointer Settings);
void GetDialogPrefs(void);
void GetRPrefs(void);
void SaveRPrefs(void);

CFStringRef appName, tabsizeKey, fontsizeKey, consolefontKey, devicefontKey;
CFStringRef outfgKey, outbgKey, infgKey, inbgKey, bufferingKey, bufferSizeKey;
CFStringRef devWidthKey, devHeightKey, devPSizeKey;
CFStringRef devAutoRefreshKey, devAntialiasingKey, devOverrideRDefKey;
CFStringRef CRANmirrorKey, BIOCmirrorKey, GlobalPackagesKey, GrabStderrKey, GrabStdoutKey;
           
         
             
OSStatus InstallPrefsHandlers(void);
void	SetUpPrefSymbols(void);

void	SetUpPrefSymbols(void){
    
    appName = CFSTR("RAqua");
    tabsizeKey = CFSTR("Tab Size");
    fontsizeKey = CFSTR("Font Size");
    consolefontKey = CFSTR("Console Font");
    devicefontKey = CFSTR("Device Font");
    outfgKey = CFSTR("OutFg Color");
    outbgKey = CFSTR("OutBg Color");
    infgKey = CFSTR("InFg Color");
    inbgKey = CFSTR("InBg Color");
    devWidthKey = CFSTR("Device Width");
    devHeightKey = CFSTR("Device Height");
    devPSizeKey = CFSTR("Device PointSize");
    devAutoRefreshKey = CFSTR("Device Autorefresh");
    devAntialiasingKey = CFSTR("Device Antialiasing");
    devOverrideRDefKey = CFSTR("Override R Defaults");
    bufferingKey = CFSTR("Buffered Output");
    bufferSizeKey = CFSTR("Output Buffer Size");
    CRANmirrorKey = CFSTR("CRAN mirror");
    BIOCmirrorKey = CFSTR("BIOC mirror");
    GlobalPackagesKey = CFSTR("Global Packages");
    GrabStdoutKey = CFSTR("Grab Stdout");
    GrabStderrKey = CFSTR("Grab Stderr");
}

OSStatus InstallPrefsHandlers(void){
    OSStatus err = noErr;
    
    err = InstallWindowEventHandler(RPrefsWindow, NewEventHandlerUPP(RPrefsWinHandler), GetEventTypeCount(RPrefsSpec), RPrefsSpec,  (void *)RPrefsWindow, NULL);


    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kTabMasterSig,kTabMasterID),  PrefsTabEventHandlerProc , GetEventTypeCount(tabControlEvents), tabControlEvents, RPrefsWindow, NULL );

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kApplyPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kCancelPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kDefaultPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kSavePrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
     
    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kConsoleFontButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kOutputColorButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kInputColorButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kOutputBackButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kInputBackButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

    err = InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kDeviceFontButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
     
    return err;
}
                   
pascal void RPrefsHandler(WindowRef window)
{
    CFStringRef	text;
    CFStringRef	appBundle;
    ControlID	versionInfoID = {kRAppSignature, kRVersionInfoID};
    ControlRef	versionControl;
    ControlFontStyleRec	controlStyle;
    
    CopyPrefs(&CurrentPrefs,&TempPrefs);
    SetUpPrefsWindow(&TempPrefs);
    SetInitialTabState(window);
     
    ShowWindow(window);
}


pascal OSStatus RPrefsWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData)
{
    OSStatus result = eventNotHandledErr;
    UInt32	eventKind;
     UInt32		eventClass;
     
    eventKind = GetEventKind(event);
    eventClass = GetEventClass(event);
    
    switch(eventClass){
     case kEventClassWindow:
      switch(eventKind){
     
      case kEventWindowClose:
       HideWindow( (WindowRef)userData );
       result = noErr;
      break;
    
      default:
      break;
      }
     break;
    }    
    
    return result;
}


void SetDefaultPrefs(void)
{
    DefaultPrefs.prefsTypeVers = RAquaPrefsVer;

    strcpy(DefaultPrefs.ConsoleFontName, DefaultConsoleFontName);
    DefaultPrefs.ConsoleFontSize = DefaultConsoleFontSize;
    DefaultPrefs.TabSize = DefaultTabSize;
    DefaultPrefs.FGInputColor = DefaultFGInputColor;
    DefaultPrefs.BGInputColor = DefaultBGInputColor;
    DefaultPrefs.FGOutputColor = DefaultFGOutputColor;
    DefaultPrefs.BGOutputColor = DefaultBGOutputColor;
    strcpy(DefaultPrefs.DeviceFontName, DefaultDeviceFontName);
    DefaultPrefs.DevicePointSize = DefaultDevicePointSize;
    DefaultPrefs.DeviceWidth = DefaultDeviceWidth;
    DefaultPrefs.DeviceHeight = DefaultDeviceHeight;	
    DefaultPrefs.AntiAlias = DefaultAntiAlias;
    DefaultPrefs.AutoRefresh = DefaultAutoRefresh;
    DefaultPrefs.OverrideRDefaults = DefaultOverrideRDefaults;
    DefaultPrefs.Buffering = DefaultBuffering;
    DefaultPrefs.BufferSize = DefaultBufferSize;
    strcpy(DefaultPrefs.CRANmirror,DefaultCRANmirror);
    strcpy(DefaultPrefs.BIOCmirror,DefaultBIOCmirror);
    DefaultPrefs.GlobalPackages = DefaultGlobalPackages;
    DefaultPrefs.GrabStdout = DefaultGrabStdout;
    DefaultPrefs.GrabStderr = DefaultGrabStderr;
}

void CopyPrefs(RAquaPrefsPointer From, RAquaPrefsPointer To)
{
    To->prefsTypeVers = From->prefsTypeVers;
    strcpy(To->ConsoleFontName,  From->ConsoleFontName);
    To->ConsoleFontSize = From->ConsoleFontSize;
    To->TabSize = From->TabSize;
    To->FGInputColor = From->FGInputColor;
    To->BGInputColor = From->BGInputColor;
    To->FGOutputColor = From->FGOutputColor;
    To->BGOutputColor = From->BGOutputColor;
    strcpy(To->DeviceFontName, From->DeviceFontName);
    To->DevicePointSize = From->DevicePointSize;
    To->DeviceWidth = From->DeviceWidth;
    To->DeviceHeight = From->DeviceHeight;
    To->AntiAlias = From->AntiAlias;
    To->AutoRefresh = From->AutoRefresh;
    To->OverrideRDefaults = From->OverrideRDefaults;
    To->Buffering = From->Buffering;
    To->BufferSize = From->BufferSize;
    strcpy(To->CRANmirror, From->CRANmirror);
    strcpy(To->BIOCmirror, From->BIOCmirror);
    To->GlobalPackages = From->GlobalPackages;
    To->GrabStdout = From->GrabStdout;
    To->GrabStderr = From->GrabStderr;
}

    
void GetRPrefs(void)
{
    
    CFNumberRef value;
    CFStringRef text;
    int 	tabsize, fontsize,pointsize;
    double	devheight, devwidth;
    CFDataRef	color;
    RGBColor    fgout,bgout,fgin,bgin;
    char consolefont[255], devicefont[255], CRANmirror[255], BIOCmirror[255];
    int	autorefresh, antialiasing, overrideRdef, buffering, buffersize;
    int	grabstdout, grabstderr, globalpackages;
    
    
    SetUpPrefSymbols();
    SetDefaultPrefs();
    CopyPrefs(&DefaultPrefs,&CurrentPrefs);

/* Tab Size */
    value = CFPreferencesCopyAppValue(tabsizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &tabsize)) tabsize = DefaultPrefs.TabSize;
	CFRelease(value);
    } else 
	tabsize = DefaultPrefs.TabSize; /* set default value */

    CurrentPrefs.TabSize  = tabsize;
    if(value)
     CFRelease(value);

/* Font Size */
    value = CFPreferencesCopyAppValue(fontsizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &fontsize)) fontsize = DefaultPrefs.ConsoleFontSize;
	CFRelease(value);
    } else 
	fontsize = DefaultPrefs.ConsoleFontSize; /* set default value */

    CurrentPrefs.ConsoleFontSize = fontsize;

/* Console Font Name */

    text = CFPreferencesCopyAppValue(consolefontKey, appName);   
    if (text) {
	if (! CFStringGetCString (text, consolefont, 255,  kCFStringEncodingMacRoman)) strcpy(consolefont, DefaultPrefs.ConsoleFontName);
	CFRelease(text);
    } else 
	strcpy(consolefont, DefaultPrefs.ConsoleFontName); /* set default value */

    strcpy(CurrentPrefs.ConsoleFontName, consolefont);


/* Device Point Size */
  value = CFPreferencesCopyAppValue(devPSizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &pointsize)) pointsize = DefaultPrefs.DevicePointSize;
	CFRelease(value);
    } else 
	pointsize = DefaultPrefs.DevicePointSize; /* set default value */

    CurrentPrefs.DevicePointSize = pointsize;

/* Device Font Name */

    text = CFPreferencesCopyAppValue(devicefontKey, appName);   
    if (text) {
	if (! CFStringGetCString (text, devicefont, 255,  kCFStringEncodingMacRoman)) strcpy(devicefont, DefaultPrefs.DeviceFontName);
	CFRelease(text);
    } else 
	strcpy(devicefont, DefaultPrefs.DeviceFontName); /* set default value */

    strcpy(CurrentPrefs.DeviceFontName, devicefont);


/* Device Width */
    value = CFPreferencesCopyAppValue(devWidthKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberDoubleType, &devwidth)) devwidth = DefaultPrefs.DeviceWidth;
	CFRelease(value);
    } else 
	devwidth = DefaultPrefs.DeviceWidth; /* set default value */

    CurrentPrefs.DeviceWidth = devwidth;

/* Device Height */
    value = CFPreferencesCopyAppValue(devHeightKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberDoubleType, &devheight)) devheight = DefaultPrefs.DeviceHeight;
	CFRelease(value);
    } else 
	devheight = DefaultPrefs.DeviceHeight; /* set default value */

    CurrentPrefs.DeviceHeight = devheight;

/* Device AntiAliasing */
    value = CFPreferencesCopyAppValue(devAntialiasingKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &antialiasing)) antialiasing = DefaultPrefs.AntiAlias;
	CFRelease(value);
    } else 
	antialiasing = DefaultPrefs.AntiAlias; /* set default value */

    CurrentPrefs.AntiAlias = antialiasing;

/* Device AutoRefresh */
    value = CFPreferencesCopyAppValue(devAutoRefreshKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &autorefresh)) autorefresh = DefaultPrefs.AutoRefresh;
	CFRelease(value);
    } else 
	autorefresh = DefaultPrefs.AutoRefresh; /* set default value */

    CurrentPrefs.AutoRefresh = autorefresh;

/* Override R Defaults */
    value = CFPreferencesCopyAppValue(devOverrideRDefKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &overrideRdef)) overrideRdef = DefaultPrefs.OverrideRDefaults;
	CFRelease(value);
    } else 
	overrideRdef = DefaultPrefs.OverrideRDefaults; /* set default value */

    CurrentPrefs.OverrideRDefaults = overrideRdef;

    /* Buffered Text Output */
    value = CFPreferencesCopyAppValue(bufferingKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &buffering)) buffering = DefaultPrefs.Buffering;
	CFRelease(value);
    } else 
	buffering = DefaultPrefs.Buffering; /* set default value */

    CurrentPrefs.Buffering = buffering;

    value = CFPreferencesCopyAppValue(bufferSizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &buffersize)) buffersize = DefaultPrefs.BufferSize;
	CFRelease(value);
    } else 
	buffersize = DefaultPrefs.BufferSize; /* set default value */

    CurrentPrefs.BufferSize = buffersize;

/*  Console Out Foreground color */
    color = CFPreferencesCopyAppValue(outfgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), (UInt8*)&fgout); 
    } else {
     fgout = DefaultPrefs.FGOutputColor;
     }

    CurrentPrefs.FGOutputColor = fgout;
    
    if(color)
     CFRelease(color);

/*  Console Out Background color */
    color = CFPreferencesCopyAppValue(outbgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), (UInt8*)&bgout); 
    } else {
     bgout = DefaultPrefs.BGOutputColor;
     }

    CurrentPrefs.BGOutputColor = bgout;
    
    
    if(color)
     CFRelease(color);
   
/*  Console In Foreground color */
    color = CFPreferencesCopyAppValue(infgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), (UInt8*)&fgin); 
    } else {
     fgin = DefaultPrefs.FGInputColor;
    }

    CurrentPrefs.FGInputColor  = fgin;
    
    if(color)
     CFRelease(color);

/*  Console In Background color */
    color = CFPreferencesCopyAppValue(inbgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), (UInt8*)&bgin); 
    } else {
     bgin = DefaultPrefs.BGInputColor;
     }

    CurrentPrefs.BGInputColor = bgin;

     if(color)
       CFRelease(color);

   
    /* CRAN mirror */
    text = CFPreferencesCopyAppValue(CRANmirrorKey, appName);   
    if (text) {
	if (! CFStringGetCString (text, CRANmirror, 255,  kCFStringEncodingMacRoman)) strcpy(CRANmirror, DefaultPrefs.CRANmirror);
	CFRelease(text);
    } else 
	strcpy(CRANmirror, DefaultPrefs.CRANmirror); /* set default value */

    strcpy(CurrentPrefs.CRANmirror, CRANmirror);
 
    /* BIOC repository  */
    text = CFPreferencesCopyAppValue(BIOCmirrorKey, appName);   
    if (text) {
	if (! CFStringGetCString (text, BIOCmirror, 255,  kCFStringEncodingMacRoman)) strcpy(BIOCmirror, DefaultPrefs.BIOCmirror);
	CFRelease(text);
    } else 
	strcpy(BIOCmirror, DefaultPrefs.BIOCmirror); /* set default value */

    strcpy(CurrentPrefs.BIOCmirror, BIOCmirror);

    /* new packages go in root */
    value = CFPreferencesCopyAppValue(GlobalPackagesKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &globalpackages)) globalpackages = DefaultPrefs.GlobalPackages;
	CFRelease(value);
    } else 
	globalpackages = DefaultPrefs.GlobalPackages; /* set default value */

    CurrentPrefs.GlobalPackages = globalpackages;

    value = CFPreferencesCopyAppValue(GrabStdoutKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &grabstdout)) grabstdout = DefaultPrefs.GrabStdout;
	CFRelease(value);
    } else 
	grabstdout = DefaultPrefs.GrabStdout; /* set default value */

    CurrentPrefs.GrabStdout = grabstdout;
    
    value = CFPreferencesCopyAppValue(GrabStderrKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &grabstderr)) grabstderr = DefaultPrefs.GrabStderr;
	CFRelease(value);
    } else 
	grabstderr = DefaultPrefs.GrabStderr; /* set default value */

    CurrentPrefs.GrabStderr = grabstderr;
 
    SetUpPrefsWindow(&CurrentPrefs);

}

void SetUpPrefsWindow(RAquaPrefsPointer Settings)
{
    ControlRef	myControl;
    ControlFontStyleRec	controlStyle;
    CFStringRef	text;
 
/* Sets the Console Font name and size */
   GetControlByID(RPrefsWindow, &ConsoleFontTextID, &myControl);
   text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%s %d pt"), Settings->ConsoleFontName, Settings->ConsoleFontSize );
   SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
   DrawOneControl(myControl);    
   CFRelease(text);
   
 
/* Sets the Device Font name */
   GetControlByID(RPrefsWindow, &DeviceFontTextID, &myControl);
   text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%s %d pt"), Settings->DeviceFontName, Settings->DevicePointSize );
   SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
   DrawOneControl(myControl);    
   CFRelease(text);
  
/* Sets the CRAN mirror  name */
   GetControlByID(RPrefsWindow, &CRANmirrorID, &myControl);
   text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%s"), Settings->CRANmirror);
   SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
   DrawOneControl(myControl);    
   CFRelease(text);
  
/* Sets the BioC repository  name */
   GetControlByID(RPrefsWindow, &BIOCmirrorID, &myControl);
   text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%s"), Settings->BIOCmirror);
   SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
   DrawOneControl(myControl);    
   CFRelease(text);
  
/* Sets color font for Output Console */
    GetControlByID(RPrefsWindow, &OutputColorID, &myControl);
    controlStyle.foreColor = Settings->FGOutputColor;
    controlStyle.backColor = Settings->BGOutputColor;
    SetControlFontStyle(myControl, &controlStyle);
    DrawOneControl(myControl);    
    
/* Sets color font for Input Console */
    GetControlByID(RPrefsWindow, &InputColorID, &myControl);
    controlStyle.foreColor = Settings->FGInputColor;
    controlStyle.backColor = Settings->BGInputColor;
    SetControlFontStyle(myControl, &controlStyle);    
    DrawOneControl(myControl);    
    
/* Sets Tab Size */
    GetControlByID(RPrefsWindow, &TabSizeID, &myControl);
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%d"), Settings->TabSize );
    SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    CFRelease( text );
 
/* Sets Device Width */
    GetControlByID(RPrefsWindow, &WidthID, &myControl);
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%f"), Settings->DeviceWidth );
    SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    CFRelease( text );
                         
/* Sets Device Height */
    GetControlByID(RPrefsWindow, &HeightID, &myControl);
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("%f"), Settings->DeviceHeight );
    SetControlData(myControl, kControlEditTextPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    CFRelease( text );


    GetControlByID(RPrefsWindow, &AutoRefreshID, &myControl);
    SetControl32BitValue(myControl, Settings->AutoRefresh);

    GetControlByID(RPrefsWindow, &AntiAliasingID, &myControl);
    SetControl32BitValue(myControl, Settings->AntiAlias);

    GetControlByID(RPrefsWindow, &BufferingID, &myControl);
    SetControl32BitValue(myControl, Settings->Buffering);

    GetControlByID(RPrefsWindow, &BufferSizeID, &myControl);
    SetControl32BitValue(myControl, Settings->BufferSize);

    GetControlByID(RPrefsWindow, &OverrideRDefBoxID, &myControl);
    SetControl32BitValue(myControl, Settings->OverrideRDefaults);

    GetControlByID(RPrefsWindow, &GlobalPackagesID, &myControl);
    SetControl32BitValue(myControl, Settings->GlobalPackages);

    GetControlByID(RPrefsWindow, &GrabStdoutID, &myControl);
    SetControl32BitValue(myControl, Settings->GrabStdout);

    GetControlByID(RPrefsWindow, &GrabStderrID, &myControl);
    SetControl32BitValue(myControl, Settings->GrabStderr);

}



void SaveRPrefs(void)
{
    CFNumberRef value;
    CFStringRef	text;
    int tabsize, fontsize,pointsize;
    double	devwidth, devheight;
    CFDataRef	color;
    RGBColor    fgout,bgout,fgin,bgin;
    char 	consolefont[255], devicefont[255], cran[255], bioc[255];
    int	autorefresh, antialiasing, overrideRdef, buffering, buffersize;
    int	grabstdout, grabstderr, globalpackages;

    
    tabsize = CurrentPrefs.TabSize;
    fontsize = CurrentPrefs.ConsoleFontSize;
    strcpy(consolefont, CurrentPrefs.ConsoleFontName);
    strcpy(devicefont, CurrentPrefs.DeviceFontName);
    
    pointsize = CurrentPrefs.DevicePointSize;
    devwidth = CurrentPrefs.DeviceWidth;
    devheight = CurrentPrefs.DeviceHeight;
    fgout = CurrentPrefs.FGOutputColor;    
    bgout = CurrentPrefs.BGOutputColor;    
    fgin = CurrentPrefs.FGInputColor;
    bgin = CurrentPrefs.BGInputColor;
    autorefresh = CurrentPrefs.AutoRefresh;
    antialiasing = CurrentPrefs.AntiAlias;
    overrideRdef = CurrentPrefs.OverrideRDefaults;
    buffering = CurrentPrefs.Buffering;
    buffersize = CurrentPrefs.BufferSize;
    globalpackages = CurrentPrefs.GlobalPackages;
    grabstdout = CurrentPrefs.GrabStdout;
    grabstderr = CurrentPrefs.GrabStderr;
    strcpy(bioc, CurrentPrefs.BIOCmirror);
    strcpy(cran, CurrentPrefs.CRANmirror);
       
/* Tab Size */
    value = CFNumberCreate(NULL, kCFNumberIntType, &tabsize); 
    CFPreferencesSetAppValue(tabsizeKey, value, appName);
    CFRelease(value);


/* Font Size */
    value = CFNumberCreate(NULL, kCFNumberIntType, &fontsize); 
    CFPreferencesSetAppValue(fontsizeKey, value, appName);
    CFRelease(value);

/* Console Font Name */
    text = CFStringCreateWithCString(NULL, consolefont, kCFStringEncodingMacRoman);
    CFPreferencesSetAppValue(consolefontKey, text, appName);
    CFRelease(text);


/* Device Point Size */
    value = CFNumberCreate(NULL, kCFNumberIntType, &pointsize); 
    CFPreferencesSetAppValue(devPSizeKey, value, appName);
    CFRelease(value);

/* Device Font Name */
    text = CFStringCreateWithCString(NULL, devicefont, kCFStringEncodingMacRoman);
    CFPreferencesSetAppValue(devicefontKey, text, appName);
    CFRelease(text);


/* Device Width */
    value = CFNumberCreate(NULL, kCFNumberDoubleType, &devwidth); 
    CFPreferencesSetAppValue(devWidthKey, value, appName);
    CFRelease(value);

/* Device Height */
    value = CFNumberCreate(NULL, kCFNumberDoubleType, &devheight); 
    CFPreferencesSetAppValue(devHeightKey, value, appName);
    CFRelease(value);

/* Device Autorefresh */
    value = CFNumberCreate(NULL, kCFNumberIntType, &autorefresh); 
    CFPreferencesSetAppValue(devAutoRefreshKey, value, appName);
    CFRelease(value);

/* Device Antialiasing */
    value = CFNumberCreate(NULL, kCFNumberIntType, &antialiasing); 
    CFPreferencesSetAppValue(devAntialiasingKey, value, appName);
    CFRelease(value);
    
/* Override R Defaults */
    value = CFNumberCreate(NULL, kCFNumberIntType, &overrideRdef); 
    CFPreferencesSetAppValue(devOverrideRDefKey, value, appName);
    CFRelease(value);

/* Buffering */
    value = CFNumberCreate(NULL, kCFNumberIntType, &buffering); 
    CFPreferencesSetAppValue(bufferingKey, value, appName);
    CFRelease(value);

    value = CFNumberCreate(NULL, kCFNumberIntType, &buffersize); 
    CFPreferencesSetAppValue(bufferSizeKey, value, appName);
    CFRelease(value);

    /* CRAN */
    text = CFStringCreateWithCString(NULL, cran, kCFStringEncodingMacRoman);
    CFPreferencesSetAppValue(CRANmirrorKey, text, appName);
    CFRelease(text);

    /* BIOC */
    text = CFStringCreateWithCString(NULL, bioc, kCFStringEncodingMacRoman);
    CFPreferencesSetAppValue(BIOCmirrorKey, text, appName);
    CFRelease(text);

    /* global install of packages */
    value = CFNumberCreate(NULL, kCFNumberIntType, &globalpackages); 
    CFPreferencesSetAppValue(GlobalPackagesKey, value, appName);
    CFRelease(value);

    value = CFNumberCreate(NULL, kCFNumberIntType, &grabstdout); 
    CFPreferencesSetAppValue(GrabStdoutKey, value, appName);
    CFRelease(value);
    
    value = CFNumberCreate(NULL, kCFNumberIntType, &grabstderr); 
    CFPreferencesSetAppValue(GrabStderrKey, value, appName);
    CFRelease(value);

    /* colors */
    color = CFDataCreate (NULL, (UInt8*)&fgout, sizeof(fgout));
    if(color){
     CFPreferencesSetAppValue(outfgKey, color, appName);
     CFRelease(color);
    }
    
    color = CFDataCreate (NULL, (UInt8*)&bgout, sizeof(bgout));
    if(color){
     CFPreferencesSetAppValue(outbgKey, color, appName);
     CFRelease(color);
    }

    color = CFDataCreate (NULL, (UInt8*)&fgin, sizeof(fgin));
    if(color){
     CFPreferencesSetAppValue(infgKey, color, appName);
     CFRelease(color);
    }

    color = CFDataCreate (NULL, (UInt8*)&bgin, sizeof(bgin));
    if(color){
     CFPreferencesSetAppValue(inbgKey, color, appName);
     CFRelease(color);
    }



    (void)CFPreferencesAppSynchronize(appName);
}

// Handler for the prefs tabs
// Switches between the 3 panes we have in this sample
static pascal OSStatus PrefsTabEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
    static UInt16 lastPaneSelected = 1; // static, to keep track of it long term (in a more complex application
                                        // you might store this in a data structure in the window refCon)                                            
    WindowRef theWindow = (WindowRef)inUserData;  // get the windowRef, passed around as userData    
    short controlValue = 0;
    int qq;
    
     
    //  Get the new value of the tab control now that the user has picked it    
    controlValue = GetControlValue( GrabCRef(theWindow,kTabMasterSig,kTabMasterID) );
    // same as last ?
    if ( controlValue != lastPaneSelected )
    {
        // different from last time.
        // Hide the current pane and make the user selected pane the active one
        // our 3 tab pane IDs.  Put a dummy in so we can index without subtracting 1 (this array is zero based, 
        // control values are 1 based).
      int tabList[] = {kDummyValue, kTabMasterID,kTabMasterID+1, kTabMasterID+2, kTabMasterID+3};
                                                                                    
        // hide the current one, and set the new one
        SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[lastPaneSelected]), false, true );
        SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[controlValue]), true, true );    

        // make sure the new configuration is drawn correctly by redrawing the Tab control itself        
        Draw1Control( GrabCRef(theWindow,kTabMasterSig,kTabMasterID) );  
        // and update our tracking
        lastPaneSelected= controlValue;    
    }
    
    return( eventNotHandledErr );
}

static void SetInitialTabState(WindowRef theWindow)
{
  int tabList[] = {kTabMasterID,kTabMasterID+1,kTabMasterID+2,kTabMasterID+3 }; 
    short qq=0;
    // If we just run without setting the initial state, then the tab control
    // will have both (or all) sets of controls overlapping each other.
    // So we'll fix that by making one pane active right off the bat.
    
    // First pass, turn every pane invisible
    for(qq=0;qq<kMaxNumTabs;qq++)
    SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[qq]), false, true );  
    
    // Set the tab control itself to have a value of 1, the first pane of the tab set
    SetControlValue(GrabCRef(theWindow,kTabMasterSig,kTabMasterID),1 );

    // This is the important bit, of course.  We're setting the currently selected pane
    // to be visible, which makes the associated controls in the pane visible.
    SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[0]), true, true );

}

ControlRef GrabCRef(WindowRef theWindow,OSType theSig, SInt32 theNum)
{   ControlID contID;
    ControlRef theRef = NULL;
    contID.signature= theSig;
    contID.id=theNum;
    GetControlByID( theWindow, &contID, &theRef );
    return(theRef);
}

static  OSStatus GenContEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
    ControlRef theCont = NULL;
    ControlID theID;
    WindowRef theWindow = (WindowRef)inUserData;
    RGBColor           	outColor;
    ControlFontStyleRec	controlStyle;
    ControlRef	myControl;
    char cran[255],bioc[255],cmd[600];

     if(FPIsFontPanelVisible())
      return( eventNotHandledErr );

    // Find out which button was clicked, and what it's ID is
    GetEventParameter (inEvent, kEventParamDirectObject, typeControlRef,NULL, sizeof(ControlRef), NULL, &theCont);
    GetControlID(theCont,&theID); 
    // swtich off the ID
 
    switch(theID.id){
        case kApplyPrefsButton:  
            HideWindow(theWindow);         
            GetDialogPrefs();
            RSetColors();
            RSetTab();
            RSetFontSize();
            RSetFont();
	    RSetPackagePrefs();
            RSetPipes();
        break;
        
        case kCancelPrefsButton: 
            HideWindow(theWindow); 
        break;
      
        case kSavePrefsButton: 
         GetDialogPrefs();
         RSetColors();
         RSetTab();
         RSetFontSize();
         RSetFont();
	 RSetPackagePrefs();
         RSetPipes();
         SaveRPrefs();
         HideWindow(theWindow);
        break;

        case kDefaultPrefsButton: 
         CopyPrefs(&DefaultPrefs,&TempPrefs);
         SetUpPrefsWindow(&TempPrefs);
         SetInitialTabState(RPrefsWindow);     
         ShowWindow(RPrefsWindow);
        break;
  
        case kConsoleFontButton:
         isConsoleFont = true;
         CallFontPanel();
        break;

        case kOutputColorButton:
         pickColor(TempPrefs.FGOutputColor, &outColor);
         TempPrefs.FGOutputColor = outColor;
         GetControlByID(RPrefsWindow, &OutputColorID, &myControl);
         controlStyle.foreColor = TempPrefs.FGOutputColor;
         controlStyle.backColor = TempPrefs.BGOutputColor;
         SetControlFontStyle(myControl, &controlStyle);
         Draw1Control(myControl);    
        break;
        
        case kInputColorButton:
         pickColor(TempPrefs.FGInputColor, &outColor);
         TempPrefs.FGInputColor = outColor;
         GetControlByID(RPrefsWindow, &InputColorID, &myControl);
         controlStyle.foreColor = TempPrefs.FGInputColor;
         controlStyle.backColor = TempPrefs.BGInputColor;
         SetControlFontStyle(myControl, &controlStyle);
         Draw1Control(myControl);    
        break;
        
        case kOutputBackButton:
         pickColor(TempPrefs.BGOutputColor, &outColor);
         TempPrefs.BGOutputColor = outColor;
         GetControlByID(RPrefsWindow, &OutputColorID, &myControl);
         controlStyle.foreColor = TempPrefs.FGOutputColor;
         controlStyle.backColor = TempPrefs.BGOutputColor;
         SetControlFontStyle(myControl, &controlStyle);
         Draw1Control(myControl);    
        break;
        
        case kInputBackButton:
         pickColor(TempPrefs.BGInputColor, &outColor);
         TempPrefs.BGInputColor = outColor;
         GetControlByID(RPrefsWindow, &InputColorID, &myControl);
         controlStyle.foreColor = TempPrefs.FGInputColor;
         controlStyle.backColor = TempPrefs.BGInputColor;
         SetControlFontStyle(myControl, &controlStyle);
         Draw1Control(myControl);    
        break;
        
        case kDeviceFontButton:
         isConsoleFont = false;
         CallFontPanel();
        break;
        
        default:
        break;
    }

   return( eventNotHandledErr );
}



void CallFontPanel(void)
{
   DeactivatePrefsWindow();
   MySetFontSelection(); 
   FPShowHideFontPanel();
//   fprintf(stderr,"\n console font=%d", FPShowHideFontPanel());
   
}




OSStatus MySetFontSelection(void)
{
    OSStatus            status = noErr;
    FontSelectionQDStyle  qdInfo;
    FMFontFamilyInstance  fontFamilyInstance;                  
    Str255	fontname;
    SInt16      fontID;
     

    if(isConsoleFont)    
     CopyCStringToPascal(TempPrefs.ConsoleFontName, fontname);
    else
      CopyCStringToPascal(TempPrefs.DeviceFontName, fontname);
    
    GetFNum(fontname,&fontID);

    fontFamilyInstance.fontStyle = 0;
    fontFamilyInstance.fontFamily = fontID;
/*    if(isConsoleFont)
     fprintf(stderr,"\n temp console font=%s, id=%d", TempPrefs.ConsoleFontName, fontID);
    else
    fprintf(stderr,"\n temp device font=%s, id=%d", TempPrefs.DeviceFontName, fontID);
  */  
    instance.fontStyle = fontFamilyInstance.fontStyle;
    instance.fontFamily = fontFamilyInstance.fontFamily;
    
    qdInfo.version = kFontSelectionQDStyleVersionZero;
    qdInfo.instance = fontFamilyInstance;

    if(isConsoleFont)
     qdInfo.size = Long2Fix(TempPrefs.ConsoleFontSize);
    else
     qdInfo.size = Long2Fix(TempPrefs.DevicePointSize);
    
    qdInfo.hasColor = false;

    status = SetFontInfoForSelection(kFontSelectionQDType,
        1, &qdInfo, NULL);
    
    return status;
}

OSStatus MyGetFontSelection (EventRef event)

{
    OSStatus status = noErr;
 
    instance.fontFamily = kInvalidFontFamily;
    instance.fontStyle = normal;
    fontSize = 0;
                                        
    if( (status = GetEventParameter (event, kEventParamFMFontFamily,
                                    typeFMFontFamily, NULL,
                                    sizeof (instance.fontFamily), 
                                    NULL, &(instance.fontFamily))) != noErr)             //2
    return status;            //3

    status = GetEventParameter (event, kEventParamFMFontSize,
                                    typeFMFontSize, NULL,
                                    sizeof( fontSize), NULL, &fontSize);              //5
                                    
    fprintf(stderr,"\n get selection: family=%d, size=%d",  instance.fontFamily, fontSize);                              
    
    return status;
}


void GetDialogPrefs(void)
{
    ControlHandle	controlField;
    CFStringRef		text;
    Size		actualSize;
  
    GetControlByID( RPrefsWindow, &TabSizeID, &controlField );
    GetControlData( controlField, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &text, &actualSize );
    CurrentPrefs.TabSize = CFStringGetIntValue( text );
    CFRelease( text );
    
    GetControlByID( RPrefsWindow, &WidthID, &controlField );
    GetControlData( controlField, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &text, &actualSize );
    CurrentPrefs.DeviceWidth = CFStringGetDoubleValue( text );
    CFRelease( text );
   
    GetControlByID( RPrefsWindow, &HeightID, &controlField );
    GetControlData( controlField, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &text, &actualSize );
    CurrentPrefs.DeviceHeight = CFStringGetDoubleValue( text );
    CFRelease( text );

    GetControlByID( RPrefsWindow, &AutoRefreshID, &controlField );
    CurrentPrefs.AutoRefresh = GetControl32BitValue(controlField);
    
    GetControlByID( RPrefsWindow, &AntiAliasingID, &controlField );
    CurrentPrefs.AntiAlias = GetControl32BitValue(controlField);
 
    GetControlByID( RPrefsWindow, &OverrideRDefBoxID, &controlField );
    CurrentPrefs.OverrideRDefaults = GetControl32BitValue(controlField);

    GetControlByID( RPrefsWindow, &BufferingID, &controlField );
    CurrentPrefs.Buffering = GetControl32BitValue(controlField);
 
    GetControlByID( RPrefsWindow, &BufferSizeID, &controlField );
    CurrentPrefs.BufferSize = GetControl32BitValue(controlField);
    
    CurrentPrefs.FGOutputColor = TempPrefs.FGOutputColor;
    CurrentPrefs.BGOutputColor = TempPrefs.BGOutputColor;    
    CurrentPrefs.FGInputColor = TempPrefs.FGInputColor;
    CurrentPrefs.BGInputColor = TempPrefs.BGInputColor;
        
    strcpy(CurrentPrefs.ConsoleFontName, TempPrefs.ConsoleFontName);
    CurrentPrefs.ConsoleFontSize = TempPrefs.ConsoleFontSize;
    
    strcpy(CurrentPrefs.DeviceFontName, TempPrefs.DeviceFontName);
    CurrentPrefs.DevicePointSize = TempPrefs.DevicePointSize;
    
 
    /* FIXME: need error handling here */
    GetControlByID( RPrefsWindow, &CRANmirrorID, &controlField );
    GetControlData( controlField, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &text, &actualSize );
    CFStringGetCString(text,CurrentPrefs.CRANmirror, 255,  kCFStringEncodingMacRoman);
    CFRelease( text );


    GetControlByID( RPrefsWindow, &BIOCmirrorID, &controlField );
    GetControlData( controlField, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &text, &actualSize );
    CFStringGetCString(text,CurrentPrefs.BIOCmirror, 255,  kCFStringEncodingMacRoman);
    CFRelease( text );

    GetControlByID( RPrefsWindow, &GlobalPackagesID, &controlField );
    CurrentPrefs.GlobalPackages = GetControl32BitValue(controlField);
 
    GetControlByID( RPrefsWindow, &GrabStdoutID, &controlField );
    CurrentPrefs.GrabStdout = GetControl32BitValue(controlField);

    GetControlByID( RPrefsWindow, &GrabStderrID, &controlField );
    CurrentPrefs.GrabStderr = GetControl32BitValue(controlField);
    
}

void pickColor(RGBColor inColour, RGBColor *outColor){
   RGBColor	         blackColour;
   Str255		     prompt;
   Point			 where = {-1,-1};

   CopyCStringToPascal("Choose a text colour:", prompt);
   if (! GetColor(where,prompt,&inColour,outColor) )
      *outColor = inColour;


}

void RSetPipes(void){
    if(CurrentPrefs.GrabStdout==1)
        OpenStdoutPipe();
    else
        CloseStdoutPipe();
            
    if(CurrentPrefs.GrabStderr==1)
        OpenStderrPipe();
    else
        CloseStderrPipe();

}
/* sets options from Packages pane of Preferences */
void RSetPackagePrefs(void){
  char cmd[600];
  static char oldBIOC[255]="http://www.bioconductor.org",  oldCRAN[255]="http://cran.r-project.org";
  
  /* actual work */
  if (strcmp(oldCRAN, CurrentPrefs.CRANmirror) | 
      strcmp(oldBIOC, CurrentPrefs.BIOCmirror)){
    strcpy(oldCRAN, CurrentPrefs.CRANmirror);
    strcpy(oldBIOC, CurrentPrefs.BIOCmirror);
    sprintf(cmd,"options(CRAN='%s',BIOC='%s')",oldCRAN,oldBIOC);
    consolecmd(cmd);
  }

  return;

}


void ActivatePrefsWindow(void)
{
   ActivateControl( GrabCRef(RPrefsWindow,kTabMasterSig,kTabMasterID) ); 
   ActivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kApplyPrefsButton ) ); 
   ActivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kCancelPrefsButton ) ); 
   ActivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig, kDefaultPrefsButton) ); 
   ActivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kSavePrefsButton ) ); 
}

void DeactivatePrefsWindow(void)
{
   DeactivateControl( GrabCRef(RPrefsWindow,kTabMasterSig,kTabMasterID) ); 
   DeactivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kApplyPrefsButton ) ); 
   DeactivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kCancelPrefsButton ) ); 
   DeactivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig, kDefaultPrefsButton) ); 
   DeactivateControl( GrabCRef(RPrefsWindow,kPrefControlsSig,kSavePrefsButton ) ); 
}

#endif /* HAVE_AQUA */

#endif /* __AQUA_PREFS__ */
