/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-4  R Development Core Team
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

void rcmdusage (char *RCMD)
{
    fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s",
	    "where 'command' is one of:\n",
	    "  INSTALL  Install add-on packages.\n",
	    "  REMOVE   Remove add-on packages.\n",
	    "  SHLIB    Make a DLL for use with dyn.load.\n",
	    "  BATCH    Run R in batch mode.\n",
	    "  build    Build add-on packages.\n",
	    "  check    Check add-on packages.\n",
	    "  Rprof    Post process R profiling files.\n",
	    "  Rdconv   Convert Rd format to various other formats, including html, Nroff,\n",
	    "           LaTeX, plain text, and S documentation format.\n",
	    "  Rd2dvi.sh Convert Rd format to DVI/PDF.\n",
	    "  Rd2txt   Convert Rd format to text.\n",
	    "  Sd2Rd    Convert S documentation to Rd format.\n");

    fprintf(stderr, "\n%s%s%s%s",
	    "Use\n  ", RCMD, " command --help\n",
	    "for usage information for each command.\n\n");    
}


int rcmdfn (int cmdarg, int argc, char **argv)
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
    char RCMD[] = "R CMD";
    int len = strlen(argv[0]);
    
    if(!strncmp(argv[0]+len-4, "Rcmd", 4) || 
       !strncmp(argv[0]+len-4, "rcmd", 4) ||
       !strncmp(argv[0]+len-8, "Rcmd.exe", 8) || 
       !strncmp(argv[0]+len-8, "rcmd.exe", 8)) 
	strcpy(RCMD, "Rcmd");


    if (argc <= cmdarg) {
	fprintf(stderr, "%s%s%s", "Usage: ", RCMD, " command args\n\n");
	rcmdusage(RCMD);
	return(0);
    }
    if (argc == cmdarg+1 && 
	(!strcmp(argv[cmdarg], "--help") || !strcmp(argv[cmdarg], "-h"))
	) {
	/* need to cover Rcmd --help, R CMD --help and R --help, 
	   as well as -h versions.
	 */
	if(cmdarg == 2 || (cmdarg == 1 && strcmp(RCMD, "Rcmd")) == 0) {
	    fprintf(stderr, "%s%s%s", "Usage: ", RCMD, " command args\n\n");
	    rcmdusage(RCMD);
	    return(0);
	}
	/* R --help */
	sprintf(cmd, "%s/bin/Rterm.exe --help", getRHOME());
	system(cmd);
	fprintf(stderr, "%s", "\n\nOr: R CMD command args\n\n");
	rcmdusage(RCMD);
	return(0);
    }

    if (cmdarg > 0 && argc > cmdarg && strcmp(argv[cmdarg], "BATCH") == 0) {
	/* handle Rcmd BATCH internally */
	char infile[MAX_PATH], outfile[MAX_PATH], cmd[MAX_PATH];
	DWORD ret;
	SECURITY_ATTRIBUTES sa;
	PROCESS_INFORMATION pi;
	STARTUPINFO si;
	HANDLE hIN = INVALID_HANDLE_VALUE, hOUT = INVALID_HANDLE_VALUE;


	/* process the command line */
	sprintf(cmd, "%s/bin/Rterm.exe --restore --save", getRHOME());

	for(i = cmdarg + 1, iused = cmdarg; i < argc; i++) {
	    if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
		fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
"Usage: ", RCMD, " BATCH [options] infile [outfile]\n\n",
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
		return(0);
	    }
	    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "-version")) {
		fprintf(stderr, "BATCH %s\n%s%s%s\n", "1.2",
"Copyright (C) 1997-2004 R Core Development Team.\n",
"This is free software; see the GNU General Public Licence version 2\n",
"or later for copying conditions.  There is NO warranty.");
		return(0);
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
	    return(1);
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
	    return(1);
	}
	hOUT = CreateFile(outfile, GENERIC_WRITE, FILE_SHARE_READ,
			  &sa, CREATE_ALWAYS, 0, NULL);
	if (hOUT == INVALID_HANDLE_VALUE) {
	    fprintf(stderr, "unable to open output file\n");
	    return(2);
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
	    return(3);
	}
	CloseHandle(pi.hThread);
	return(pwait(pi.hProcess));
    } else {
	RHome = getRHOME();
	if (argc > cmdarg+1 && strcmp(argv[cmdarg+1], "RHOME") == 0) {
	    fprintf(stdout, "%s", RHome);
	    return(0);
	}
	strcpy(RHOME, "R_HOME=");
	strcat(RHOME, RHome);
	for (p = RHOME; *p; p++) if (*p == '\\') *p = '/';
	putenv(RHOME);

	sprintf(Rversion, "R_VERSION=%s.%s", R_MAJOR, R_MINOR);
	putenv(Rversion);

	putenv("R_CMD=R CMD");
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

	if (cmdarg > 0 && argc > cmdarg) {
	    p = argv[cmdarg];
	    if (strcmp(p, "Rd2dvi") == 0) {
		strcat(cmd, "sh "); 
		strcat(cmd, RHome); strcat(cmd, "/bin/Rd2dvi.sh");
	    } else {
		if (!strcmp(".sh", p + strlen(p) - 3)) {
		    strcpy(cmd, "sh "); 
		    strcat(cmd, RHome); strcat(cmd, "/bin/");
		} else if (!strcmp(".bat", p + strlen(p) - 4)) strcpy(cmd, "");
		else if (!strcmp(".exe", p + strlen(p) - 4)) strcpy(cmd, "");
		else {
		    strcpy(cmd, "perl ");
		    strcat(cmd, RHome); strcat(cmd, "/bin/");
		}
		strcat(cmd, p);
	    }
	} else
	    sprintf(cmd, "%s/bin/Rterm.exe", getRHOME());

	for (i = cmdarg + 1; i < argc; i++){
	    strcat(cmd, " ");
	    if (strlen(cmd) + strlen(argv[i]) > 9900) {
		fprintf(stderr, "command line too long\n");
		return(27);
	    }
	    if(strchr(argv[i], ' ')) {
		strcat(cmd, "\"");
		strcat(cmd, argv[i]);
		strcat(cmd, "\"");
	    } else strcat(cmd, argv[i]);
	}
	/* printf("cmd is %s\n", cmd); */
	res = system(cmd);
	if (res) status = 1;
    }
    return(status);
 }
