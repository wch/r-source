ReadMe file for build, install and execute the AQUA - Darwin R


The final installation should have the following structure

1)
You can build the RAqua GUI either with or without X11 support

./configure --with-aqua --without-x
or
./configure --with-aqua 

Compiling with X11 support requires X11 header files and running with X11 
support requires an X server, such as Apple's X server. X11 support is 
only partial at the moment; full support awaits changes in R's event 
handling.


2) To install the RAqua GUI type

  make install-aqua

Unless you have write permission to /Applications you will need an 
administrator password and sudo:
  sudo make install-aqua


3)  double-click on the StartR icon in /Applications, or drag it to the Dock.


About the RAqua GUI:

The GUI has separate input and output windows.  What you type goes into
the small input window, and the output goes in the large window above. 
You can change the colors and font sizes in Preferences.  The output
window is buffered: it may not update until the previous command is
finished. This makes output of large objects much faster.  Eventually you
will be able to turn off buffering if you want each line of output
displayed as it occurs during a long computation. 

Help pages display in separate pop-=up windows, and HTML help is also 
available in your browser.

The standard graphics driver for RAqua is quartz(), using the Quartz 
PDF-based display engine. It has all the features you would expect in an 
R device driver.  If you compiled with X11 support and have an X 
server running you can also use x11() for the X-Windows display device. 
x11() windows can be moved but the contents will not be redrawn if the 
window is resized: create the window at the size you want.  Of course, 
all the usual file-based devices are available: PDF(), postscript(), 
xfig(), etc.

Data frames can be displayed and edited with data.entry() and edit(). 

The Workspace menu allows the Workspace to be saved, loaded, or browsed.  
The workspace browser can also be started with the browseEnv() function.

The Tools window manages the command history and working directory.

The Packages menu allows you to install packages from CRAN and 
Bioconductor, and to see what packages are currently installed.



Notes for developers:

The Console is based on the Carbon MLTE system. It is  in 
modules/aqua/aquaconsole.c.  There are two TXNObjects, one for the input 
window and one for the output window.  The MLTE engine is documented in 
the Apple Developer Toolkit: 
/Developer/Documentation/Carbon/Text/MultiLingualTextEngine


Beware of code that assumes Unix systems are X11-based. For example, 
edit.data.frame() used to assume that a Unix system with no DISPLAY 
variable must not have a GUI. 



