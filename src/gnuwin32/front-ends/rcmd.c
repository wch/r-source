/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-3  R Development Core Team
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

#define NONAMELESSUNION
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <Rversion.h>

extern char *getRHOME(); /* in ../rhome.c */

static int pwait(HANDLE p)
{
    DWORD ret;

    WaitForSingleObject(p, INFINITE);
    GetExitCodeProcess(p, &ret);
    return ret;
}

int main (int argc, char **argv)
{
    /* tasks:
       find R_HOME
       set PATH to include R_HOME\bin
       set PERL5LIB to %R_HOME%/share/perl;%Perl5LIB%
       set TEXINPUTS to %R_HOME%/share/texmf;%TEXINPUTS%
       launch %R_HOME%\bin\$*
     */
    int i, iused, res, status = 0;
    char *RHome, PERL5LIB[MAX_PATH], TEXINPUTS[MAX_PATH], PATH[10000], 
	RHOME[MAX_PATH], *p, cmd[10000], Rversion[25];

    if (argc > 1 && strcmp(argv[1], "BATCH") == 0) {
	/* handle Rcmd BATCH internally */
	char infile[MAX_PATH], outfile[MAX_PATH], cmd[MAX_PATH];
	DWORD ret;
	SECURITY_ATTRIBUTES sa;
	PROCESS_INFORMATION pi;
	STARTUPINFO si;
	HANDLE hIN = INVALID_HANDLE_VALUE, hOUT = INVALID_HANDLE_VALUE;
	

	/* process the command line */
	sprintf(cmd, "%s/bin/Rterm.exe --restore --save", getRHOME());

	for(i = 2, iused = 1; i < argc; i++) {
	    if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
		fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s\n",
"Usage: Rcmd BATCH [options] infile [outfile]\n\n",
"Run R non-interactively with input from infile and place output (stdout\n",
"and stderr) to another file.  If not given, the name of the output file\n",
"is the one of the input file, with a possible '.R' extension stripped,\n",
"and '.Rout' appended.\n\n",
"Options:\n"
"  -h, --help		print short help message and exit\n",
"  -v, --version		print version info and exit\n",
"  --			end processing of options\n\n",
"Further arguments starting with a '-' are considered as options as long\n",
"as '--' was not encountered, and are passed on to the R process, which\n",
"by default is started with '--restore --save'.\n\n",
"Report bugs to <r-bugs@r-project.org>.");
		exit(0);
	    }
	    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "-version")) {
		fprintf(stderr, "BATCH %s\n%s%s%s\n", "1.1",
"Copyright (C) 1997-2003 R Core Development Team.\n",
"This is free software; see the GNU General Public Licence version 2\n",
"or later for copying conditions.  There is NO warranty.");
		exit(0);
	    }
	    if (!strcmp(argv[i], "--")) {
		iused = i;
		break;
	    }
	    if (argv[i][0] == '-') {
		strcat(cmd, " ");
		strcat(cmd, argv[i]);
		iused = i;
	    } else break;
	}
	if (iused+1 < argc) 
	    strcpy(infile, argv[iused+1]);
	else {
	    fprintf(stderr, "no input file\n");
	    exit(1);
	}
	if (iused+2 < argc)
	    strcpy(outfile, argv[iused+2]);
	else {
	    int len = strlen(infile);
	    strcpy(outfile, infile);
	    if (!strcmp(outfile+len-2, ".R")) strcat(outfile, "out");
	    else strcat(outfile, ".Rout");
	}
	/* fprintf(stderr, "%s->%s\n", infile, outfile);
	   fprintf(stderr, "%s\n", cmd); */
	
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;

	hIN = CreateFile(infile, GENERIC_READ, FILE_SHARE_READ, 
			 &sa, OPEN_EXISTING, 0, NULL);
	if (hIN == INVALID_HANDLE_VALUE) {
	    fprintf(stderr, "unable to open input file\n");
	    exit(1);
	}
	hOUT = CreateFile(outfile, GENERIC_WRITE, FILE_SHARE_READ,
			  &sa, CREATE_ALWAYS, 0, NULL);
	if (hOUT == INVALID_HANDLE_VALUE) {
	    fprintf(stderr, "unable to open output file\n");
	    exit(2);
	}
	SetStdHandle(STD_INPUT_HANDLE, hIN);
	SetStdHandle(STD_OUTPUT_HANDLE, hOUT);
	SetStdHandle(STD_ERROR_HANDLE, hOUT);
	si.cb = sizeof(si);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_SHOWDEFAULT;
	ret = CreateProcess(0, cmd, &sa, &sa, TRUE, 0, NULL, NULL, &si, &pi);
	CloseHandle(hIN);
	CloseHandle(hOUT);
	if (!ret) {
	    fprintf(stderr, "unable to run Rterm.exe\n");
	    exit(3);
	}
	CloseHandle(pi.hThread);
	exit(pwait(pi.hProcess));
    } else {
	RHome = getRHOME();
	strcpy(RHOME, "R_HOME=");
	strcat(RHOME, RHome);
	for (p = RHOME; *p; p++) if (*p == '\\') *p = '/';
	putenv(RHOME);

	sprintf(Rversion, "R_VERSION=%s.%s", R_MAJOR, R_MINOR);
	putenv(Rversion);
   
	putenv("R_CMD=Rcmd");
	putenv("R_OSTYPE=windows");

	strcpy(PATH, "PATH=");
	strcat(PATH, RHome); strcat(PATH, "\\bin;");
	strcat(PATH, getenv("PATH"));
	putenv(PATH);

	if ( (p = getenv("TMPDIR")) && strlen(p)) {
	    /* TMPDIR is already set */
	} else {
	    putenv("TMPDIR=c:/TEMP");
	}

	strcpy(PERL5LIB, "PERL5LIB=");
	strcat(PERL5LIB, RHome); strcat(PERL5LIB, "\\share\\perl;");
	if ( (p = getenv("PERL5LIB")) ) strcat(PERL5LIB, p);
	putenv(PERL5LIB);

        strcpy(TEXINPUTS, "TEXINPUTS=");
        strcat(TEXINPUTS, RHome); strcat(TEXINPUTS, "\\share\\texmf;");
        if ( (p = getenv("TEXINPUTS")) ) strcat(TEXINPUTS, p);
        putenv(TEXINPUTS);

	if (argc > 1) {
	    p = argv[1];
	    if (strcmp(".sh", p + strlen(p) - 3) == 0) {
		strcpy(cmd, "sh "); strcat(cmd, RHome); strcat(cmd, "/bin/");
	    } else if (strcmp(".bat", p + strlen(p) - 4) == 0) strcpy(cmd, "");
	    else if (strcmp(".exe", p + strlen(p) - 4) == 0) strcpy(cmd, "");
	    else {
		strcpy(cmd, "perl "); strcat(cmd, RHome); strcat(cmd, "/bin/");
	    }
	    for (i = 1; i < argc; i++){
		if (i > 1) strcat(cmd, " ");
		if (strlen(cmd) + strlen(argv[i]) > 9900) {
		    fprintf(stderr, "command line too long\n");
		    exit(27);
		}
		strcat(cmd, argv[i]);
	    }
/*	    printf("cmd is %s\n", cmd); */
	    res = system(cmd);
	    if (res) status = 1;
	}
	exit(status);
    }
}
