/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-10  R Development Core Team
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
 *  along with this program; if not,  a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdlib.h> /* for exit */
#include <stdio.h>
#include <Rversion.h>

#ifndef BINDIR
# define BINDIR "bin"
#endif

extern char *getRHOME(void), *getRUser(void); /* in ../rhome.c */

void R_Suicide(char *s) /* for use in ../rhome.o */
{
    fprintf(stderr, "FATAL ERROR:%s\n", s);
    exit(2);
}


static int pwait(HANDLE p)
{
    DWORD ret;

    WaitForSingleObject(p, INFINITE);
    GetExitCodeProcess(p, &ret);
    return ret;
}

# include <sys/stat.h>

#if !defined(S_IFDIR) && defined(__S_IFDIR)
# define S_IFDIR __S_IFDIR
#endif

static int isDir(char *path)
{
    struct stat sb;
    int isdir = 0;
    if(path[0] && stat(path, &sb) == 0) 
	isdir = (sb.st_mode & S_IFDIR) > 0;
    return isdir;
}


void rcmdusage (char *RCMD)
{
    fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
	    "where 'command' is one of:\n",
	    "  INSTALL  Install add-on packages.\n",
	    "  REMOVE   Remove add-on packages.\n",
	    "  SHLIB    Make a DLL for use with dyn.load.\n",
	    "  BATCH    Run R in batch mode.\n",
	    "  build    Build add-on packages.\n",
	    "  check    Check add-on packages.\n",
	    "  Rprof    Post process R profiling files.\n",
	    "  Rdconv   Convert Rd format to various other formats.\n",
	    "  Rdiff    difference R output files.\n",
	    "  Rd2dvi   Convert Rd format to DVI.\n",
	    "  Rd2pdf   Convert Rd format to PDF.\n",
	    "  Rd2txt   Convert Rd format to pretty text.\n",
	    "  Sd2Rd    Convert S documentation to Rd format.\n",
	    "  Stangle  Extract S/R code from Sweave documentation.\n",
	    "  Sweave   Process Sweave documentation.\n",
	    "  config   Obtain configuration information about R.\n"
	    "  open     Open a file via Windows file associations.\n"
	    );

    fprintf(stderr, "\n%s%s%s%s",
	    "Use\n  ", RCMD, " command --help\n",
	    "for usage information for each command.\n\n");
}

extern int process_Renviron(const char *filename);
#define CMD_LEN 10000
int rcmdfn (int cmdarg, int argc, char **argv)
{
    /* tasks:
       find R_HOME, set as env variable (with / as separator)
       set PATH to include R_HOME\bin
       set TMPDIR if unset
       set HOME if unset
       set R_CMD (depends on how this was launched), R_VERSION
       read R_HOME\etc\Rcmd_environ
       launch %R_HOME%\bin\$*
     */
    int i, iused, status = 0;
    char *RHome, BUFFER[10000],
	RHOME[MAX_PATH], *p, cmd[CMD_LEN], Rversion[25], HOME[MAX_PATH + 10];
    char RCMD[] = "R CMD";
    int len = strlen(argv[0]);
    char env_path[MAX_PATH];
    int timing = 1;

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
	snprintf(cmd, CMD_LEN, "%s/%s/Rterm.exe --help", getRHOME(), BINDIR);
	system(cmd);
	fprintf(stderr, "%s", "\n\nOr: R CMD command args\n\n");
	rcmdusage(RCMD);
	return(0);
    }

    if (cmdarg > 0 && argc > cmdarg && 
	strcmp(argv[cmdarg], "BATCH") == 0) {
	/* handle Rcmd BATCH internally */
	char infile[MAX_PATH], outfile[MAX_PATH], *p, cmd_extra[CMD_LEN];
	DWORD ret;
	SECURITY_ATTRIBUTES sa;
	PROCESS_INFORMATION pi;
	STARTUPINFO si;
	HANDLE hOUT = INVALID_HANDLE_VALUE;

	/* process the command line */
	cmd_extra[0] = '\0';
	if((p = getenv("R_BATCH_OPTIONS")) && strlen(p)) {
	    if(1+strlen(p) >= CMD_LEN) {
		fprintf(stderr, "command line too long\n");
		return(27);
	    }
	    strcat(cmd_extra, " ");
	    strcat(cmd_extra, p);
	}

	for(i = cmdarg + 1, iused = cmdarg; i < argc; i++) {
	    if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
		fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n",
"Usage: ", RCMD, " BATCH [options] infile [outfile]\n\n",
"Run R non-interactively with input from infile and place output (stdout\n",
"and stderr) to another file.  If not given, the name of the output file\n",
"is the one of the input file, with a possible '.R' extension stripped,\n",
"and '.Rout' appended.\n\n",
"Options:\n"
"  -h, --help		print short help message and exit\n",
"  -v, --version		print version info and exit\n",
"  --no-timing		do not report the timings\n",
"  --			end processing of options\n\n",
"Further arguments starting with a '-' are considered as options as long\n",
"as '--' was not encountered, and are passed on to the R process, which\n",
"by default is started with '--restore --save'.\n\n",
"Report bugs to <r-bugs@r-project.org>.");
		return(0);
	    }
	    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--version")) {
		fprintf(stderr, "BATCH %s\n%s%s%s\n", "1.2",
"Copyright (C) 1997-2004 R Core Development Team.\n",
"This is free software; see the GNU General Public Licence version 2\n",
"or later for copying conditions.  There is NO warranty.");
		return(0);
	    }
	    if (!strcmp(argv[i], "--no-timing")) {
		timing = 0;
		iused = i;
		continue;
	    }

	    if (!strcmp(argv[i], "--")) {
		iused = i;
		break;
	    }
	    if (argv[i][0] == '-') {
		if (strlen(cmd_extra) + strlen(argv[i]) > 9900) {
		    fprintf(stderr, "command line too long\n");
		    return(27);
		}
		strcat(cmd_extra, " ");
		strcat(cmd_extra, argv[i]);
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

	snprintf(cmd, CMD_LEN, "%s/%s/Rterm.exe -f \"%s\" --restore --save",
		 getRHOME(), BINDIR, infile);
	if(strlen(cmd) + strlen(cmd_extra) >= CMD_LEN) {
	    fprintf(stderr, "command line too long\n");
	    return(27);
	}
	strcat(cmd, cmd_extra);
	if(timing) 
	    putenv("R_BATCH=1234"); 
	/* to get ,Last.sys run: see profile/Common.R */

	/* fprintf(stderr, "%s->%s\n", infile, outfile);
	   fprintf(stderr, "%s\n", cmd); */

	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;

	hOUT = CreateFile(outfile, GENERIC_WRITE, FILE_SHARE_READ,
			  &sa, CREATE_ALWAYS, 0, NULL);
	if (hOUT == INVALID_HANDLE_VALUE) {
	    fprintf(stderr, "unable to open output file\n");
	    return(2);
	}
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
	CloseHandle(hOUT);
	if (!ret) {
	    fprintf(stderr, "unable to run Rterm.exe\n");
	    return(3);
	}
	CloseHandle(pi.hThread);
	return(pwait(pi.hProcess));
    } else if (cmdarg > 0 && argc > cmdarg && 
	      strcmp(argv[cmdarg], "INSTALL") == 0) {
	/* handle Rcmd INSTALL internally */
	snprintf(cmd, CMD_LEN, 
		 "%s/%s/Rterm.exe -e tools:::.install_packages() R_DEFAULT_PACKAGES= LC_COLLATE=C --no-restore --slave --args ",
		 getRHOME(), BINDIR);
	for (i = cmdarg + 1; i < argc; i++) {
	    strcat(cmd, "nextArg");
	    if (strlen(cmd) + strlen(argv[i]) > 9900) {
		fprintf(stderr, "command line too long\n");
		return(27);
	    }
	    strcat(cmd, argv[i]);
	}
	status = system(cmd);
	return(status);
    } else if (cmdarg > 0 && argc > cmdarg && 
	      strcmp(argv[cmdarg], "REMOVE") == 0) {
	/* handle Rcmd REMOVE internally */
	snprintf(cmd, CMD_LEN, 
		 "%s/%s/Rterm.exe -f \"%s/share/R/REMOVE.R\" R_DEFAULT_PACKAGES=NULL --slave --args",
		 getRHOME(), BINDIR, getRHOME());
	for (i = cmdarg + 1; i < argc; i++){
	    strcat(cmd, " ");
	    if (strlen(cmd) + strlen(argv[i]) > 9900) {
		fprintf(stderr, "command line too long\n");
		return(27);
	    }
	    /* Library names could contain spaces */
	    if(strchr(argv[i], ' ')) {
		strcat(cmd, "\"");
		strcat(cmd, argv[i]);
		strcat(cmd, "\"");
	    } else strcat(cmd, argv[i]);
	}
	status = system(cmd);
	return(status);
    } else {
	RHome = getRHOME();
	if (argc > cmdarg+1 && 
	    strcmp(argv[cmdarg+1], "RHOME") == 0) {
	    fprintf(stdout, "%s", RHome);
	    return(0);
	}
	strcpy(RHOME, "R_HOME=");
	strcat(RHOME, RHome);
	for (p = RHOME; *p; p++) if (*p == '\\') *p = '/';
	putenv(RHOME);

	snprintf(Rversion, 25, "R_VERSION=%s.%s", R_MAJOR, R_MINOR);
	putenv(Rversion);

	putenv("R_CMD=R CMD");

	strcpy(BUFFER, "PATH=");
	strcat(BUFFER, RHome);
	strcat(BUFFER, "\\"); 
	strcat(BUFFER, BINDIR);
	strcat(BUFFER, ";"); 
	strcat(BUFFER, getenv("PATH"));
	putenv(BUFFER);

	strcpy(BUFFER, "BINDIR=");
	strcat(BUFFER, BINDIR);
	putenv(BUFFER);

	if ( (p = getenv("TMPDIR")) && isDir(p)) {
	    /* TMPDIR is already set */
	} else {
	    if ( (p = getenv("TEMP")) && isDir(p)) {
		strcpy(BUFFER, "TMPDIR=");
		strcat(BUFFER, p);
		putenv(BUFFER);
	    } else if ( (p = getenv("TMP")) && isDir(p)) {
		strcpy(BUFFER, "TMPDIR=");
		strcat(BUFFER, p);
		putenv(BUFFER);
	    } else {
		strcpy(BUFFER, "TMPDIR=");
		strcat(BUFFER, getRUser());
		putenv(BUFFER);
	    }
	}

	if( !getenv("HOME") ) {
	    strcpy(HOME, "HOME=");
	    strcat(HOME, getRUser());
	    putenv(HOME);
	}

	strcpy(env_path, RHome); strcat(env_path, "/etc/rcmd_environ");
	process_Renviron(env_path);

	if (cmdarg > 0 && argc > cmdarg) {
	    p = argv[cmdarg];
	    if (strcmp(p, "Rd2dvi") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Rd2dvi.sh", RHome);
	    } else if (strcmp(p, "Rdiff") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Rdiff.sh", RHome);
	    } else if (strcmp(p, "Sweave") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Sweave.sh", RHome);
	    } else if (strcmp(p, "Stangle") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Stangle.sh", RHome);
	    } else if (strcmp(p, "rtags") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/rtags.sh", RHome);
	    } else if (strcmp(p, "config") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/config.sh", RHome);
	    } else if (strcmp(p, "SHLIB") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/SHLIB.sh", RHome);
	    } else if (strcmp(p, "Rdconv") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Rdconv.sh", RHome);
	    } else if (strcmp(p, "Rd2txt") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Rdconv.sh -t txt", RHome);
	    } else if (strcmp(p, "Rd2pdf") == 0) {
		snprintf(cmd, CMD_LEN, "sh %s/bin/Rd2dvi.sh --pdf", RHome);
		strcpy(cmd, "sh ");
		strcat(cmd, RHome); strcat(cmd, "/bin/Rd2dvi.sh --pdf");
	    } else if (strcmp(p, "build") == 0) {
		snprintf(cmd, CMD_LEN, "perl %s/bin/build.pl", RHome);
	    } else if (strcmp(p, "check") == 0) {
		snprintf(cmd, CMD_LEN, "perl %s/bin/check.pl", RHome);
	    } else if (strcmp(p, "Rprof") == 0) {
		snprintf(cmd, CMD_LEN, "perl %s/bin/Rprof.pl", RHome);
	    } else if (strcmp(p, "Sd2Rd") == 0) {
		snprintf(cmd, CMD_LEN, "perl %s/bin/Sd2Rd.pl", RHome);
	    } else if (strcmp(p, "open") == 0) {
		snprintf(cmd, CMD_LEN, "%s/%s/open.exe", RHome, BINDIR);
	    } else {
		/* RHOME/BINDIR is first in the path, so looks there first */
		if (!strcmp(".sh", p + strlen(p) - 3)) strcpy(cmd, "sh ");
		else if (!strcmp(".pl", p + strlen(p) - 3)) strcpy(cmd, "perl ");
		else strcpy(cmd, "");
		strcat(cmd, p);
	    }
	} else
	    snprintf(cmd, CMD_LEN, "%s/%s/Rterm.exe", getRHOME(), BINDIR);

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
	status = system(cmd);
    }
    return(status);
 }
