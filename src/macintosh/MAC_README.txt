Macintosh porting of R
======================

In this directory you can find all the files needed
to compile a specific version of R for MacOS.
The original code for MacOS was written by Ross Ihaka and
Wing Kwong (Tiki) WAN. Most of the files have been
modified or simply updated to work with new version of R
sources (up to 1.2.0) and Waste Text Engine (c) 1993-2000 Marco
Piovanelli.

Building R
==========
The R binary can be compiled using Code Worrior Pro 5 by simply
opening the file "R.mcp" in the "cwpro" directory.
Once you have built the R application you should copy the "R"
binary and "WasteLib" from "src/macintosh/bin" in the folder 
where you put also the "library" directory (with all 
packages built), the "afm", "demos" and "tests"
directories. The distribution folder should appear as

afm/		(dir)
demo/		(dir)
library/	(dir)
R		(file)
tests/		(dir)
WasteLib	(file)

Before building R :
===================
1) Please rename the file "src/include/config.mac.h" to 
"src/include/config.h" and copy it over
"src/include/macintosh/Rconfig.h" 
before you build the R application.
3) if you do not use F2C unzip the archive
"src/appl/mac.sit" and put the 26 "*.c" files in
the directory "src/appl"

Building packages
=================
To build packages you should refere to the respective
"src/library/src/macintosh.sit" archive where you can find a 
CW project.
Once you have built a package you should move the resulting file
to the "libs" directory in the "library/package" folder.

There is still no "makefile" to build the R application,
configure it, build help and doc files etc. You can take the
help and examples files from the Windows version by simply
copying the whole directories in their respective counterparts
of the R distribution for MacOS.

Most of the files in the scr/macintosh directory are
only partially commented. This will change in the near 
future. The GUI sources are adapted from the WasteDemo
application from the Waste Text Engine distribution. 
Thanks to Marco Piovanelli for providing these sources
for free.

These sources are made free so that Macintosh/R developers
can help us fixing bugs and complete a full porting.

Please remember that it is still a Developer version.

For any comments and feedback please contact me at:

stefano.iacus@unimi.it


Milano, Jan 7 2001

           Stefano M. Iacus




