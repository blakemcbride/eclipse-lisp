/* Where supported by the operating system, the Eclipse run time
   library (eg. libeclipse.a) is defined to use wide character
   functions defined by ANSI/ISO C.  On some operating systems, such
   as Solaris 2.6, this places a REFERENCE to the wide character
   function definitions in the library, but not the code itself.  In
   such cases, an application using the library may not run on an
   earlier version of the operating system which does not provide
   these ANSI/ISO wide character definitions.

   Note that the eclipse program (i.e. the Eclipse top-level Lisp
   listener) is an example of an application that is linked to the
   Eclipse run time library. 

   If you don't really need the wide definitions (i.e. you are just
   working with ASCII characters), then one solution is to use this
   file (chars.c), which defines "dummy" definitions of the wide
   character utilities which simply call the non-wide equivalents.
   Compile the file and link the result (chars.o) with any application
   using the Eclipse run time library.  The easiest way to insure that
   all applications are so linked is to simply add chars.o to the
   Eclipse library -- eg. with 'ar -rv libeclipse.a chars.o'

   For example, to make a new version of the eclipse top-level Lisp
   listener which uses the non-wide utilities on all operating system
   versions, follow the directions above, and then relink eclipse.o
   (where the eclipse main() is defined) with the (now modified)
   Eclipse libraries -- eg. with 
      cc eclipse.o -o eclipse -leclipse -leclipsed -lm -ldl

   The location of Eclipse files is documented in
     "Eclipse Introduction: Installation"
   The Eclipse libraries are documented in 
     "Eclipse C: The Eclipse Libraries"
   The main() in eclipse.c is documented in
     "Eclipse Applications: Top-Level main() Example"

   Alternatively, you can contact your operating system provider and
   ask how the ANSI/ISO C wide character definitions can be provided
   at run time (presumably by pushing the path to some compatibility
   library onto some environment variable).  */

#include <wchar.h>

#ifndef __P
#  ifdef __STDC__
#    define __P(protos) protos
#  else
#    define __P(protos) ()
#  endif
#endif

#ifdef __STDC__
  typedef int char_arg_t;
#else
  typedef char char_arg_t;
#endif

#define CharRedef1(newname, oldname, rtype)	\
rtype newname __P((wint_t));			\
rtype newname(c) wint_t c; { return(oldname((char_arg_t) c)); }

#define CharRedef(newname, oldname) CharRedef1(newname, oldname, int) 

CharRedef(iswalpha, isalpha)
CharRedef(iswalnum, isalnum)
CharRedef(iswprint, isprint)
CharRedef(iswupper, isupper)
CharRedef(iswlower, islower)
CharRedef1(towupper, toupper, wint_t)
CharRedef1(towlower, tolower, wint_t)

/* Note: Harbison & Steele document the argument for isprint to be
   backwards from what is defined above for char_arg_t:
   isprint(char c) in STDC, and isprint(int c) in traditional C.
   I think this is just a misprint. -HRS*/
