*************************** Motivation **********************************

Many interactive programs read input line by line, but would like to
provide line editing and history functionality to the end-user that 
runs the program.

The input-edit package provides that functionality.  As far as the 
programmer is concerned, the program only asks for the next line
of input. However, until the user presses the RETURN key they can use
emacs-style line editing commands and can traverse the history of lines
previously typed.

Other packages, such as GNU's readline, have greater capability but are
also substantially larger.  Input-edit is small, since it uses neither
stdio nor any termcap features, and is also quite portable.  It only uses
\b to backspace and \007 to ring the bell on errors.  Since it cannot
edit multiple lines it scrolls long lines left and right on the same line.

Input edit uses classic (not ANSI) C, and should run on any Unix 
system (BSD, SYSV or POSIX), PC's under DOS with MSC, TurboC or djgpp,  
PC's under OS/2 with gcc (EMX), or Vax/VMS.  Porting the package to new 
systems basicaly requires code to read a character when it is typed without 
echoing it, everything else should be OK.

I have run the package on:

	DECstation 5000, Ultrix 4.3 with cc 2.1 and gcc 2.3.3
	Sun Sparc 2, SunOS 4.1.1, with cc
	SGI Iris, IRIX System V.3, with cc
	PC using DOS with MSC

The description below is broken into two parts, the end-user (editing) 
interface and the programmer interface.  Send bug reports, fixes and 
enhancements to:

Chris Thewalt (thewalt@ce.berkeley.edu)   
5/3/93

Thanks to the following people who have provided enhancements and fixes:
  Ron Ueberschaer, Christoph Keller, Scott Schwartz, Steven List,
  DaviD W. Sanderson, Goran Bostrom, Michael Gleason, Glenn Kasten,
  Edin Hodzic, Eric J Bivona, Kai Uwe Rommel, Danny Quah, Ulrich Betzler

PS: I don't have, and don't want to add, a vi mode, sorry.

************************** End-User Interface ***************************

Entering printable keys generally inserts new text into the buffer (unless
in overwrite mode, see below).  Other special keys can be used to modify
the text in the buffer.  In the description of the keys below, ^n means
Control-n, or holding the CONTROL key down while pressing "n".  Errors
will ring the terminal bell.

^A/^E	: Move cursor to beginning/end of the line.
^F/^B   : Move cursor forward/backward one character.
ESC-F	: Move cursor forward one word.
ESC-B   : Move cursor backward one word.
^D	: Delete the character under the cursor.
^H, DEL : Delete the character to the left of the cursor.
^K	: Kill from the cursor to the end of line.
^L	: Redraw current line.
^O	: Toggle overwrite/insert mode. Initially in insert mode. Text
	  added in overwrite mode (including yanks) overwrite
	  existing text, while insert mode does not overwrite.
^P/^N   : Move to previous/next item on history list.
^R/^S   : Perform incremental reverse/forward search for string on
	  the history list.  Typing normal characters adds to the current
	  search string and searches for a match. Typing ^R/^S marks
	  the start of a new search, and moves on to the next match.
	  Typing ^H or DEL deletes the last character from the search 
	  string, and searches from the starting location of the last search.  
	  Therefore, repeated DEL's appear to unwind to the match nearest 
	  the point at which the last ^R or ^S was typed.  If DEL is 
	  repeated until the search string is empty the search location 
	  begins from the start of the history list.  Typing ESC or 
	  any other editing character accepts the current match and 
	  loads it into the buffer, terminating the search.
^T	: Toggle the characters under and to the left of the cursor.
^U      : Deletes the entire line
^Y	: Yank previously killed text back at current location.  Note that
	  this will overwrite or insert, depending on the current mode.
TAB	: By default adds spaces to buffer to get to next TAB stop 
	  (just after every 8th column), although this may be rebound by the 
	  programmer, as described below.
NL, CR  : returns current buffer to the program.

DOS and ANSI terminal arrow key sequences are recognized, and act like:

  up    : same as ^P
  down  : same as ^N
  left  : same as ^B
  right : same as ^F

************************** Programmer Interface ***************************

The programmer accesses input-edit through these functions, and optionally
through three additional function pointer hooks.  The four functions are:

char *getline(char *prompt)

	Prints the prompt and allows the user to edit the current line. A
	pointer to the line is returned when the user finishes by
	typing a newline or a return.  Unlike GNU readline, the returned
	pointer points to a static buffer, so it should not be free'd, and
	the buffer contains the newline character.  The user enters an
	end-of-file by typing ^D on an empty line, in which case the
	first character of the returned buffer is '\0'.  Getline never
	returns a NULL pointer.  The getline functions sets terminal modes
	needed to make it work, and resets them before returning to the
	caller.  The getline function also looks for characters that would
	generate a signal, and resets the terminal modes before raising the
	signal condition.  If the signal handler returns to getline, 
	the screen is automatically redrawn and editing can continue.
	Getline now requires both the input and output stream be connected
	to the terminal (not redirected) so the main program should check
	to make sure this is true.  If input or output have been redirected
	the main program should use buffered IO (stdio) rather than
	the slow 1 character read()s that getline uses.

void gl_setwidth(int width)

        Set the width of the terminal to the specified width. The default
	width is 80 characters, so this function need only be called if the
	width of the terminal is not 80.  Since horizontal scrolling is
	controlled by this parameter it is important to get it right.

void gl_histadd(char *buf)

	The gl_histadd function checks to see if the buf is not empty or
	whitespace, and also checks to make sure it is different than
	the last saved buffer to avoid repeats on the history list.
	If the buf is a new non-blank string a copy is made and saved on
	the history list, so the caller can re-use the specified buf.

void gl_strwidth(size_t (*func)())
	The gl_strwidth function allows the caller to supply a pointer to 
	a prompt width calculation function (strlen by default). This
	allows the caller to embed escape sequences in the prompt and then
	tell getline how many screen spaces the prompt will take up.

The main loop in testgl.c, included in this directory, shows how the
input-edit package can be used:

extern char *getline();
extern void  gl_histadd();
main()
{
    char *p;
    do {
	p = getline("PROMPT>>>> ");
	gl_histadd(p);
	fputs(p, stdout);
    } while (*p != 0);
}

In order to allow the main program to have additional access to the buffer,
to implement things such as completion or auto-indent modes, three
function pointers can be bound to user functions to modify the buffer as
described below.  By default gl_in_hook and gl_out_hook are set to NULL,
and gl_tab_hook is bound to a function that inserts spaces until the next
logical tab stop is reached.  The user can reassign any of these pointers
to other functions.  Each of the functions bound to these hooks receives
the current buffer as the first argument, and must return the location of
the leftmost change made in the buffer.  If the buffer isn't modified the
functions should return -1.  When the hook function returns the screen is
updated to reflect any changes made by the user function.

int (*gl_in_hook)(char *buf)

	If gl_in_hook is non-NULL the function is called each time a new 
	buffer is loaded. It is called when getline is entered, with an
	empty buffer, it is called each time a new buffer is loaded from
	the history with ^P or ^N, and it is called when an incremental
	search string is accepted (when the search is terminated). The
	buffer can be modified and will be redrawn upon return to getline(). 

int (*gl_out_hook)(char *buf)

	If gl_out_hook is non-NULL it is called when a line has been
	completed by the user entering a newline or return. The buffer
	handed to the hook does not yet have the newline appended. If the
	buffer is modified the screen is redrawn before getline returns the
	buffer to the caller.

int (*gl_tab_hook)(char *buf, int prompt_width, int *cursor_loc)

	If gl_tab_hook is non-NULL, it is called whenever a tab is typed.
	In addition to receiving the buffer, the current prompt width is
	given (needed to do tabbing right) and a pointer to the cursor
	offset is given, where a 0 offset means the first character in the
	line.  Not only does the cursor_loc tell the programmer where the
	TAB was received, but it can be reset so that the cursor will end
	up at the specified location after the screen is redrawn.
