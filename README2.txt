
Eclipse Common Lisp

General info

Eclipse functions as an ANSI Common Lisp interpreter.  It can also
compile Lisp to C, and then link in the C code like you would link
a C program.

The system was primarily written in Lisp and was capable of self
hosting.  All the lisp files for the system are in the "lisp" directory.

The "bin" directory used to contain executables for various systems.
The only one still present is the one for Windows.

Pretty good documentation is located under the "doc" directory.
See:  doc/eclipse/eclipse.htm

The system did work on Mac and Windows in the past.  I have only
been testing on a 64 bit Linux machine.  Be sure to see ISSUES.txt

The system depends upon the Boehm-Demers-Weiser conservative garbage
collector.  If you don't have it installed on your system, get, build,
and install it first.  It's at: http://www.hboehm.info/gc

Converting between 32 & 64 bit machines is done in eclipse.h
See the typedef for clWord.


To build:

	cd c
	make

You will end up with:

	eclipse
	libeclipse.a
	libeclipsed.a

Use :exit to exit the system.

In terms of the Lisp compiler, the documentation talks about two formats;
to a binary format, and to C.  The binary format was never completed,
but the compile to C was.  For example:

Let's say we have myfile.lisp containing:

(defun abc (a b)
	(+ a b))

From eclipse we type:

(compile-file "myfile" 'output-format :c)

We end up with myfile.c

myfile.c will contain a function named "usrMyfile" that initializes
the compiled function into the system and must be called from within
eclipse.  Add a call to that function from with main() in eclipse.c
There is a comment in eclipse.c that says:

  /* Init-functions for Eclipse-generated C files go here. 	*/

Put calls to each file's initialization function in there.  In our example,
I will add the line:

	usrMyfile();

Add myfile.c to the USERSOURCE variable in the makefile.  Type:

	make

This will build "eclipse" with the compiled file part of the system.

I think if we can get past the issues listed in ISSUES.txt, the next
step would be to have the system re-gen itself (although, I believe
the system can also be re-gen'd with another Common Lisp system too).
After that, the system should be pretty nice.

Blake McBride

