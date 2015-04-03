#ifdef linux
#  define __STDC_VERSION__ 1	/* claims conformance, but isn't */
#endif

#if (defined __STDC_VERSION__) && __STDC_VERSION__ == 199409L 
#  if (defined hpux)
#    include <wchar.h>		/* wrong ANSI header file */
#else
#    include <wctype.h>
#  endif
#  ifdef _WIN32	/* Fix non-ANSI windows implementation */
#    define iswprint(x) (iswprint(x) && (x != '\t'))
#  endif
#else
#  include <ctype.h>
#  define iswalpha(x) isalpha(x)
#  define iswalnum(x) isalnum(x)
#  define iswprint(x) isprint(x)
#  define iswupper(x) isupper(x)
#  define iswlower(x) islower(x)
#  define towupper(x) toupper(x)
#  define towlower(x) tolower(x)
#endif
