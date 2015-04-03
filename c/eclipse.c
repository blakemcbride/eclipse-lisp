/*********************************************************************
  This data and information is proprietary to, and a valuable trade
  secret of, Elwood Corporation.  It is given in confidence by Elwood,
  and may only be used as permitted under the license agreement under
  which it has been distributed and in no other way.  No disclosure of
  this information shall be made to any person or organization without
  the prior consent of Elwood Corporation. 

  (c) Copyright 1997, 1998 by Elwood Corp.  All rights reserved.

  main() for Eclipse Common Lisp top-level Lisp listener.

    Handles command line argument parsing and initialization of Lisp 
    system.  Then calls clLispTopLevel() as the priniciple operation.
    Non-zero status is returned if clLispTopLevel() returns an error
    status. 

    To rebuild a new Eclipse top-level Lisp listener using this file,
    compile this file and link the result with the Eclipse libraries,
    the usual C and math libraries, and any platform specific
    libraries noted in the "Release Notes: Platform Specific"
    documentation.   For example, on most Unix systems, if this file
    is named eclipse.c:
       cc eclipse.o -o eclipse -leclipse -leclipsed -lm -ldl

  Written by HRS.
 *********************************************************************/
#include <stdio.h>
#include <eclipse.h>

extern void clInit __P((unsigned)), clInitD __P((void));
extern clObject clLispTopLevel(clProto),
  clCharpSimpleBaseString __P((const char *));

extern int clPROGRESS_FLAG;	/* Temporary.  See below. */

int main __P((int, char **));

/* For command line processing, and not part of Eclipse itself. */
#ifdef _WIN32	/* Doesn't supply getopt() in library. */
#include <string.h>
int optind = 1;
char *optarg;
int getopt(int argc, char * const *argv, const char *optstring) {
  char *arg, *optchar;
  if (optind >= argc) return(EOF);
  arg = argv[optind++];
  /* Accept DOS or Unix option/switch markers. */
  if ((arg[0] == '-') || (arg[0] == '/')) { 
    /* "--" means end of options, ":xxx" is illegal */
    if (strchr("-:", arg[1])) return(EOF);
    if (NULL == (optchar = strchr(optstring, arg[1]))) return(EOF);
    if (':' == optchar[1]) {	/* an option with an argument */
      if (arg[2] != '\0') optarg = arg+2;
      else optarg = argv[optind++];
    }
    return((int) *optchar);
  }
  return(EOF);
}
#else
#  include <unistd.h>
#endif

int 
main(argc, argv)
     int argc;
     char **argv;
{
  int silentp = 0, initp = 1;
  char *init_file = NULL;

  /* ARGUMENT PARSING.						*
   * This is application specific, and none of it has any	*
   * interaction with the internal workings of Eclipse.   	*/
  {
    int flag, error_flag = 0, versionp = 0;

    while ((flag = getopt(argc, argv, "vsni:")) != EOF)
      switch (flag) {
      case 's':
	silentp = 1;
	break;
      case 'n':
	initp = 0;
	break;
      case 'i':
	init_file = optarg;
	break;
      case 'v':
	versionp = 1;
	break;
      default:
	error_flag++;
      } 
    if (error_flag || (optind < argc)) {
      (void) fprintf(stderr, "usage: %s [-vsn [-i initfile]\n", argv[0]);
      (void) fprintf(stderr, "  -v print version and exit.\n");
      (void) fprintf(stderr, "  -s silent mode.\n");
      (void) fprintf(stderr, "  -n do not load init file.\n");
      (void) fprintf(stderr, "  -i use specified init file instead of searching.\n");
      return(error_flag);
    }
    if (versionp) {
      (void) puts("Eclipse Common Lisp " clEclipseVersion());
      (void) puts("Copyright (c) 1997, 1998 Elwood Corporation, Inc.");
      return(0);
    }
  }

  /* INITIALIZATION.						*
   * For the Eclipse top-level, we need only call clInit(0).	*
   * Other applications might call other initializations, too. 	*/

  /* Soon startup will be fast enough that progress reports are
     unnecessary, so it isn't worth changing the interface to
     clInit().  That's why we temporarilly use this global variable to
     control the progress reports printed by clInit(). */
  clPROGRESS_FLAG = !silentp;

  clInit(0);
  clInitD();
  /* Init-functions for Eclipse-generated C files go here. 	*/

  /* PRINCIPLE OPERATIONS					*
   * For the Eclipse top-level, we need only call the 		*
   * lisp-top-level function. 					*
   * A non-nil return value indicates that the EXIT restart was	*
   * used with a non-nil status (i.e. someone decided there was	*
   * an error). 						*/

  /* N.B.: In some operating systems, a true (i.e. non-zero) 	*
   * return value from main() indicates an error status.	*/
  return(clTrue(clLispTopLevel(clTest(!silentp),
			       (!initp ? clNIL :
				(!init_file ? clT :
				 clCharpSimpleBaseString(init_file))),
			       clEOA)));
}
