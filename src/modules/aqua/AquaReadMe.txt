ReadMe file for building, installing and executing the AQUA - Darwin R GUI.


Building RAqua from Source:

0) You need C and Fortran compilers.  See 
  http://www.economia.unimi.it/R/
 for some recommendations.  

1)
You can build the RAqua GUI either with or without X11 support, and with 
or without Tcl/Tk support.  

Without either X11 or Tcl/Tk:
%  ./configure --enable-R-shlib --with-blas='-framework vecLib' 
   --with-lapack --with-aqua --without-x

With both:
%  ./configure --enable-R-shlib --with-blas='-framework vecLib' 
   --with-lapack 
   --with-aqua
provided that you have built and installed tk and tcl from the unix tree 
of the respective sources.

Then build R with
%  make


Compiling with X11 support requires X11 header files (SDK, as Apple 
calls it) and running with X11 support requires an X server, such as 
Apple's Xquartz server (http://www.apple.com/macosx/x11/download/).  
The X server is shipped with Panther along with the SDK. The latter
is not installed by default.

Compiling with Tcl/Tk requires the X11 implementation of the libraries. 
See
     http://www.economia.unimi.it/R/
for more information on where to get it and how to install it.


2) To install the RAqua GUI type

%  make install-aqua

Unless you have write permission to /Applications you will need an 
administrator password and sudo:

%  sudo make install-aqua


3)  double-click on the StartR icon in /Applications, or drag it to the 
Dock and single-click it.


Installing packages:
Binary packages will be provided by the time RAqua is released.  Source 
packages can be installed but may need some extra stuff:

 - For packages with no C or Fortran, RAqua is sufficient.

 - For packages with C but not Fortran the Apple developer toolkit is 
   sufficient 

Otherwise see  
  http://www.economia.unimi.it/R/
for details.


Creating packages:
The tar command in OS X is missing some features that are used by R CMD 
build.  At the moment the recommended work-around is to install GNU tar 
(eg, as part of fink) and set the TAR environment variable to point to 
GNU tar. Under Panther, GNU tar is the default tar.
Eg
  setenv TAR /sw/bin/tar



About the RAqua GUI:

The GUI has separate input and output windows.  What you type goes into
the small input window, and the output goes in the large window above. 
You can change the colors and font sizes in Preferences.  The output
window is buffered: it may not update until the previous command is
finished. This makes output of large objects much faster. You can turn 
off buffering and adjust the size of the buffer in Preferences.

Help pages display in separate pop-up windows, and HTML help is also 
available in your browser.

The standard graphics driver for RAqua is quartz(), using the Quartz 
PDF-based display engine. It has all the features you would expect in an 
R device driver.  If you compiled with X11 support and have an X 
server running you can also use x11() for the X-Windows display device. 
Of course, all the usual file-based devices are available: PDF(), 
postscript(), xfig(), etc.

Data frames can be displayed and edited with data.entry() and edit(). 

The Workspace menu allows the Workspace to be saved, loaded, cleared or
browsed.  The workspace browser can also be started with the browseEnv()
function.

The Tools window manages the command history and working directory.

The Packages menu allows you to install packages from CRAN and 
Bioconductor, to see what packages are currently installed, and to load 
and unload packages and load data sets.





Notes for developers:

Nearly everything is in src/modules/aqua/. At the moment the main 
exception is the quartz device driver, which is in src/unix/ but may 
move soon.

The Console is based on the Carbon MLTE system. It is  in 
modules/aqua/aquaconsole.c.  There are two TXNObjects, one for the input 
window and one for the output window.  The MLTE engine is documented in 
the Apple Developer Toolkit: 
/Developer/Documentation/Carbon/Text/MultiLingualTextEngine

The package manager, data set manager, install browsers, and workspace 
browser are DataBrowser controls:
http://developer.apple.com/technotes/tn/tn2009.html

Menus, and windows other than the data browsers are constructed from
specifications in modules/aqua/Contents/Resources/main.nib, which can be
edited with the Interface Builder.

Beware of code that assumes Unix systems are X11-based. For example, 
edit.data.frame() used to assume that a Unix system with no DISPLAY 
variable must not have a GUI. Check .Platform$GUI=="AQUA" in interpreted 
code or  !strcmp(R_GUIType,"AQUA") in C.

There's a lot of coding based on static variables in the aqua files. If 
you can't find a local declaration for a variable look at the top of the 
file.


This version: nov 19th 2003
	
