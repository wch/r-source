This directory contains an extended version of GraphApp
(http://www.cs.usyd.edu.au/~loki/GraphApp) version 2.45
for MsWindows). Extensions and changes are copyrighted by
Guido Masarotto (guido@stat.unipd.it) and Brian D. Ripley
(ripley@stats.ox.ac.uk).

The COPYLIB.TXT was a copy of the GNU Library General Public License,
as in file ../../../share/licenses/LGPL-2

Extesion include:
(1) Better support for MDI applications (MDI clients can have
    menus, toolbar and status bar which are displayed, in the
    standard way, on the MDI frame).
(2) Thread-safe drawing operation (essentially all the
    operation defined in drawing.c but without any reference
    to current[state,window]. In addition, there is support
    for line styles (in the fashion needed by R) and rotated fonts).
(3) Popup menus.
(4) Windows printer and metafiles.
(5) Tooltips.
(6) bitmap -> image conversion.
(7) Win32 Api memory allocation functions (C library free/realloc
    do not release memory!!).
(8) Possibility to specify color by name.
(9) A simple console/pager widget.

All new functions are defined in ga.h; include that file not graphapp.h.


ORIGINAL README FILE FOR GRAPHAPP
---------------------------------

This Zip file contains the following files:

COPYLIB.TXT  - Describes the GNU General Public License for library files.

You should read this file so you know under what terms you are making
use of the GraphApp library. Basically the license says you can freely
use and modify the library, but any modifications made to the library
must be made available as source code to anyone who wants them. It does
not restrict your use of the library in an unmodified form to produce
programs for profit.

pizza.c      - A simple example GraphApp program.
example.prj  - A Borland C++ 3.11 project file for making programs.
example.dsk  - An associated Borland C++ 3.11 project desktop file.
graphapp.prj - A Borland C++ 3.11 project file for building GraphApp.
graphapp.dsk - An associated Borland C++ 3.11 project desktop file.

You should ignore the Borland C++ 3.11 project files if you are not
using that version of the Borland C compiler. I'm not sure they'll
work with any other version of Borland C or with any other compiler.

graphapp.h   - The GraphApp header file - use this in your projects.
               Better still, copy it into your compiler's include
               directory (e.g. C:\BORLANDC\INCLUDE)
str.h        - A simple C++ string class - include it in your
               programs if you want, or else use the strings included
               with the standard templates library (STL).
internal.h   - Used in building the library - do not include it
               in your projects.
graphapp.c   - Compiling this file will compile the entire library.
*.c          - The remaining C files are all part of the GraphApp
               library source code. Place these in a directory called
               SRCWIN and leave them alone unless you want to build
               a new version of the library.
