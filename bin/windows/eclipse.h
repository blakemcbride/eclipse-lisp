/*********************************************************************
 * This data and information is proprietary to, and a valuable trade
 * secret of, Elwood Corporation.  It is given in confidence by
 * Elwood, and may only be used as permitted under the license
 * agreement under which it has been distributed and in no other way.
 * No disclosure of this information shall be made to any person or
 * organization without the prior consent of Elwood Corporation.
 *
 * (c) Copyright 1995, 1997, 1998 by Elwood Corp.  
 * All rights reserved.  
 * This copyright notice applies to all Elwood Eclipse source code.
 *
 * Written by HRS.
 *
 * N.B.  This is not Lisp!  Similarities between the names of
 * operators defined here and those in Lisp are only to remind us of
 * how the operators are used.  They do not simply indicate a C SYNTAX
 * for a Lisp construction.  Nothing in here should be assumed to have
 * the same SEMANTICS as a Lisp operation with a similar name.
 *
 * To call most of the functions in the library from C, you need to
 * understand only the following:
 *
 * 1. Naming convention.  This is described in a separate document,
 *    but the most important thing is that the C functions
 *    corresponding to Lisp functions are prefixed with "cl", the
 *    first letter of each word in the Lisp name is capitalized, and
 *    the dashes are removed.  For example: CONS => clCons,
 *    SYMBOL-VALUE => clSymbolValue
 *
 * 2. The arguments to the C functions are the same as the arguments
 *    to the Lisp functions, including optional and named arguments.
 *    The only difference is that the arguments must be terminated
 *    with the symbol clEOA (end-of-arguments).  For example, 
 *       (LIST A B C) => clList(a, b, c, clEOA).
 *
 *    Some functions which take a fixed number of arguments and
 *    return a single value also have an "internal" C function
 *    which does not require clEOA, does no argument checking
 *    (including type checking), and does not produce multiple values.
 *    The name of the internal version begins with an underscore.  For
 *    example, (FOO A) => clFoo(a, clEOA) or _clFoo(a).  The internal
 *    utility may actually be implemented as a function-like C macro
 *    rather than as a C function.
 *
 * 3. All arguments and return values to a library Lisp function are
 *    of a single C data type: clObject.  Functions are provided for
 *    creating various Lisp clObjects from C primitives such as int
 *    and float, and for extracting C primitives from some Lisp
 *    clObjects.
 *
 *    The internal versions of some functions (i.e. the _versions), do
 *    take C primitives directly.
 *
 * To write "Lisp functions" in C, or to use Lisp constructs such as
 * closures, multiple values, the dynamic environment or nonlocal
 * exits, you need to read more about the naming convention, and about
 * the special constructs defined in this file.
 *
 * Included are:
 *    PART I: DATA REPRESENTATION
 *    PART II: FUNCTION CALLS
 *    PART III: EXITS AND THE DYNAMIC ENVIRONMENT
 *********************************************************************/
#ifndef cl_ECLIPSE

#define cl_ECLIPSE
#define clEclipseVersion() "1.1"

#if (defined sun)
#  if (defined __SVR4) 
#    define Solaris	1
#  else
#    define SunOS4	1
#  endif
#endif

/* Microsoft conforms but they don't define the macro to indicate so. */
#if (defined _WIN32) && !(defined __STDC_VERSION__)
#  define __STDC_VERSION__ 199409L
#endif

/* As far as we are concerened, c++ implies standard C. 
   "Real" standard C should define __STDC__ to be 1, but Eclipse cares
   only whether it is defined at all. */
#if !(defined __STDC__) && \
    ((defined __cplusplus) || (defined __STDC_VERSION__))
#  define __STDC__ 2
#endif

#include <stddef.h>		/* wchar_t and other types */
#include <limits.h>		/* CHAR_BIT */
#include <setjmp.h>		/* control structure */

/* To make Eclipse behave as a double-word aligned system on
 * architectures that are normally only word aligned, one should
 * compile everything (eclipse and gc) with CFLAGS including
 * "-malign-double -DALIGN_DOUBLE".  (IWBNI gcc defined ALIGN_DOUBLE
 * for you when you specify -malign-double!) We have successfully
 * tested this on linux, and it achieves the following:
 
 * 1. Make sure cl_WORD_ALIGNED is not defined here.  This allows
 * Eclipse to use the low order three bits for pointer tagging,
 * instead of just two bits.

 * 2. Make sure the garbage collector always allocates double word
 * aligned heap objects.  For the PPCR garbage collector currently
 * used, this is done by making sure ALIGN_DOUBLE is defined (for
 * example, in gc/config.h).  By default, it is NOT defined for
 * single-word aligned systems.
 * 
 * 3. Make sure stack allocated and static (i.e. C compile-time
 * literal) clObjects will also be double word aligned.  (Eclipse user
 * code does not currently create any of these, but the Eclipse
 * library itself DOES use static clObjects.)  All Eclipse clObjects
 * begin with a clTagCell, which in turn, begins with a union
 * involving a double.  Thus we will get double word alignment IFF the
 * compiler arranges for double-word alignment of double floats.  Note
 * that on platforms for which gcc normally uses single-word
 * alignment, gcc provides the -malign-double compiler switch. */

#ifndef ALIGN_DOUBLE
/* List here any systems which are, by default, single word aligned. */
#  if (defined linux) || (defined _WIN32)
#    define cl_WORD_ALIGNED
#  endif
#endif

/*********************************************************************
 * GENERAL UTILITIES
 *********************************************************************/
/* This file is included in all generated files, and it is also used
 * in a system file which defines internal variables and low level
 * routines. If and only if it being used in the system definitions
 * file, the parameter cl_ECLIPSE_DEFINITIONS is defined.
 */

/* The clPaste2() macro defines a portable way to make a single C
 * identifier out of two.  K&R compilers usually allow "pasting" or
 * concatenation by separating components with comment markers.  On
 * ANSI systems, we use the ## pasting operarator.  WARNING: It is
 * very important that there be no space before or after the comma
 * when calling clPaste2(arg1,arg2).  */

#if __STDC__
#  define clPaste2(x,y)		x ## y
#  define clPaste3(x,y,z)	x ## y ## z
#else
#  define clPaste2(x,y)		x/**/y
#  define clPaste3(x,y,z)	x/**/y/**/z
#endif

/* For all our efforts to be compatible, linux goes ahead and defines
   __P incorrectly within sys/cdefs.h, used by limits.h and setjmp.h. */
#if (defined linux) && (defined __P)
#  undef __P
#endif

/* For declaring ANSI prototypes in a K&R-safe way.  Usage:
     return_t FunctionName __P((arg1_t, arg2_t, ...)); 
   Note space between FunctionName and __P, and double parentheses. */
#ifndef __P
#  if __STDC__
#    define __P(protos) protos
#  else
#    define __P(protos) ()
#  endif
#endif

/* Some "optimized" compilers go behind your back and put volatile
 * variables in registers despite the later presence of setjmp() in
 * the same code block.  This is wrong.  The following definitions may
 * be useful for working around such C compiler bugs.  (The Eclipse
 * compiler does not currently generate any of these.)
 *
 * clVolatile() might need to be a function call on some machines to
 * keep a compiler from optimizing away all but the last of a
 * sequential series of clVolatile() references.  
 */

#if (defined sysV68)
#  define clVolatile(var) 	cl_volatile_pointer = (char *) &var
#  ifndef cl_ECLIPSE_DEFINITIONS
     extern
#  endif
   char *cl_volatile_pointer;
#else
#  define clVolatile(var)
#endif

#if !(defined __STDC__)
#  ifndef volatile
#    define volatile		
#  endif
#  ifndef const
#    define const
#  endif
#endif

/* n and d are integral C types, n!=0. */
#define clCeil(n, d)		(1 + (((n)-1) / (d)))

/* Systems which do not define _setjmp/_longjmp... */
#if (defined sysV68) || (defined Solaris) || (defined _WIN32)
#  define _setjmp(buf)		setjmp(buf)
#  define _longjmp(buf, val)	longjmp(buf, val)
#endif
  
/*********************************************************************
 *********************************************************************
 *		     PART I: DATA REPRESENTATION
 *		     ---------------------------
 *
 * Most Lisp data is kept in C structures which we generically call
 * cells.  A Lisp clObject is usually a pointer to one of these cells.
 * Depending on whether data is word aligned (4-byte) or double word
 * aligned (8-byte), the two or three low order bits of these pointers
 * will always be clear. 
 *
 * The first member in most cells is a data tag which identifies
 * the type of the object.  We say such data types are DATA-TAGGED
 * because they carry a type-identifying tag in a known place in the
 * data cell.  (The current implementatin uses clObjects -- class
 * metaobjects -- as data tags.)  
 *
 * There are some data types which do not have a data tag.  We
 * distinguish these objects by using the low two or three bits in the
 * pointer as a tag.  We call these POINTER-TAGGED data types.  We can
 * determine the type of pointer-tagged data without dereferencing a
 * pointer, which can be an advantage.  On the other hand, to examine
 * the actual data, we must mask off or otherwise suppress the tag
 * bits.  (The means of "otherwise suppressing" the tag bits is a
 * programming issue, not generally a performance one.)  Sometimes
 * pointer-tagged data representations are used when the non-tag data
 * occupies exactly 8 bytes and we don't want the allocated memory for
 * the object to increase to 12 or 16 bytes because of the tag.
 *
 * Some Lisp data types such as fixnums and characters are small
 * enough that, together with the three tag bits, they can fit in the
 * same size clWord as is used for pointers to cells.  These are
 * called IMMEDIATE DATA, as the data is found directly in the clWord
 * and no pointer need be followed.  Again, the low two or three bits
 * are used to distinguish these from other kinds of data.  Some
 * immediate data may have additional tag bits which distinguish
 * between different kinds of immediate data.
 *
 * Thus all Lisp clObjects are a machine clWord which, depending on
 * the tag bits, may be interpreted as either immediate data of some
 * kind, a pointer-tagged pointer to a cell, or a pointer to a
 * data-tagged cell.
 *********************************************************************/

/* When an object cell WITHOUT a data tag occupies a multiple of the
 * machine alignment size, then including a data tag would make its
 * size jump to the next multiple.  We want to avoid this on
 * double-word aligned systems, so these system use pointer tagging
 * for cons and double float cells.  
 */
#ifdef cl_WORD_ALIGNED
#  define cl_POINTER_TAG_BITS	2 /* single word-aligned systems */
#else
#  define cl_POINTER_TAG_BITS	3 /* double word-aligned systems */
#endif

typedef enum {
  cl_DATA_TAG = 	0,				/* 000 */
  cl_EVEN_FIXNUM_TAG = 	1,				/* 001 */
  cl_RESERVED_TAG,	/* cl_SHORT_FLOAT */		/* 010 */
  cl_SIMPLE_CHARACTER_TAG,				/* 011 */
  cl_CONS_TAG = 		cl_DATA_TAG + 4,	/* 100 */
  cl_ODD_FIXNUM_TAG = 		cl_EVEN_FIXNUM_TAG + 4,	/* 101 */
  cl_DOUBLE_FLOAT_TAG,					/* 110 */
  cl_RESERVED2_TAG = 	cl_SIMPLE_CHARACTER_TAG + 4,	/* 111 */
  cl_MAX_TAG
  } clPointerTagCell; 

/* Without the cast, Microsoft compilers break when n_bits is an 
   expression involving the sizeof operator, and the result is 
   inverted (~) and compared (<=) to an int. */
#define clBitMask(n_bits) 	(~(~0 << ((int) n_bits)))
#define cl_TAG_MASK		clBitMask(cl_POINTER_TAG_BITS)

typedef int clWord;		/* Integer big enough to hold a pointer */
typedef const char *clCharp;	/* (char *) in an easy to Lisp form */

/*********************************************************************
 * Object definition.  
 *
 * The cl_CTYPECHECKS version allows for very strict compiler/lint type
 * checking of pointer casts.  It defines clObject as a union of
 * types.  Some utilities which lint has trouble with are packaged
 * into functions.  
 *
 * N.B.: compiling with cl_CTYPECHECKS defined does not currently work!
 *
 * Unfortunately, some linters complain about the casts introduced by
 * va_start(), which we use in BeginParse(), and which can't be
 * abstracted into a function.  However, the LINT version turns these
 * into no-ops, which of course, won't compile into anything useful,
 * but should lint cleanly.
 *
 * The non-cl_CTYPECHECKS version defines clObject as a pointer to an
 * object cell and makes frequent uses of casts which some versions of
 * lint warn about, though all C compilers support.
 *
 * For MOST, but NOT ALL C compilers, the machine representation of
 * clObjects is the same in both cases.  
 *
 * N.B.: Not all compilers on the same platform return unions the same
 * way.  In particular, gcc may return cl_CTYPECHECKS clObjects
 * differently than PCC does.  Therefore, when compiling with
 * cl_CTYPECHECKS, all Eclipse files, including both user and system
 * code, must be compiled with the same compiler.  (In the case of
 * gcc, the -fpcc-struct-return compiler option can be specified to
 * force PCC compatible clObject returns.)
 *
 * Operations:
 *  clObjectTag(obj:clObject) => clPointerTagCell
 *   EFFECTS: Access the complete clObject as a tag, for reading or
 *     writing. 
 *
 *  clObjectWord(obj:clObject) => clWord
 *   EFFECTS: Access the complete clObject as a clWord, for reading or
 *     writing.  
 *
 * clObjectAddress(obj:clObject) => clObjectCell *
 *   EFFECTS: Access the complete clObject as a pointer to an
 *     clObjectCell, for reading or writing.  Note that the pointer
 *     will not actually be valid if obj is an immediate or pointer
 *     tagged object.
 *
 *  clSetq(place:clObject, value: clObject) => x:clObject
 *   REQUIRES: place is a writable clObject location (variable, struct
 *     member, etc.).
 *   EFFECTS: Stores value in place and returns it.  
 *   RATIONALE: Using the cl_CTYPECHECKS representation, place = value;
 *     will not work.  Also, use of clSetq() emphasizes that the place
 *     being written to is an clObject location (as opposed to an int
 *     or something else), and makes C code look more like Lisp code.
 *     Note, however, that lisp SETQ takes any number of place/value
 *     pairs, whereas clSetq() takes only one pair.
 *
 *  clMakeHeapObject(size:unsigned) => clObject
 *   EFFECTS: Returns an uninitialized heap object of the specified
 *     size. size might be computed, for example, as
 *     sizeof(clSymbolCell).  
 *********************************************************************/
#ifdef LINT
#  if !(defined cl_CTYPECHECKS)
#     define cl_CTYPECHECKS
#  endif
#endif

#ifdef cl_CTYPECHECKS
   typedef union {
     union clObjectCell *address;
     clPointerTagCell	tag;
     clWord	word;
   }			clObject;

#  define clObjectTag(obj)	((obj).tag)
#  define clObjectWord(obj)	((obj).word)
#  define clObjectAddress(obj)	((obj).address)
#  define clSetq(obj_var, val) 	_clSetq(&(obj_var), (val))
   clObject _clSetq __P((clObject *, clObject)),
     clWordObject __P((clWord)),
     clAddressObject __P((clObjectCell *)),
     clStandardInstancepObject __P((clStandardInstanceCell *)),
     clWrapperpObject __P((clWrapperCell *)),
     clSymbolpObject __P((clSymbolCell *)); 

#else  /* no CTYPECHECKS */
   typedef union clObjectCell *	clObject;
#  define clObjectTag(obj)	((clPointerTagCell) (obj))
#  define clObjectWord(obj)	((clWord) (obj))
#  define clObjectAddress(obj)	(obj)
#  define clWordObject(word)	((clObject) (word))
#  define clAddressObject(addr)	((clObject) (addr))
#  define clSetq(obj_var, val) 	((obj_var)=(val))
#  define clStandardInstancepObject(pp) ((clObject) (pp))
#  define clWrapperpObject(pp) 		((clObject) (pp))
#  define clSymbolpObject(pp) 		((clObject) (pp))

#endif				/* cl_CTYPECHECKS */

#ifdef __STDC__
#  define clProto		clObject, ...
#else
#  define clProto		
#endif

/*********************************************************************
 * INTERMEDIATE TYPES
 *
 * These might be represented as either clObjects or as some C
 * primitive.  Their exact representation is subject to change.
 * 
 * An clIndex is an unsigned integral value large enough to represent
 * the length of the longest sequence.
 *
 * A clSlots is an array of clObjects.
 * 
 * Implementation:
 *  For now, both clIndex and all USES of slots are as clObjects.
 *
 * Operations:
 *
 * clIndexFixnum(i:clIndex) => clObject
 *   EFFECTS: Returns a fixnum representing i.
 *
 * clFixnumIndex(obj:clObject) => int
 *   REQUIRES: obj must be a fixnum.
 *   EFFECTS: returns the clIndex value of obj.
 *********************************************************************/

typedef clObject clIndex;
#define clIntIndex(ii)		clToImmediate(ii, cl_FIXNUM_TAG)
#define clIndexInt(index)	clFromImmediate(index, unsigned)

#define clISetq(place, index)	clSetq(place, index)
#define _clIEq(index1, index2)	_clEq(index1, index2)

#define clIndexFixnum(index)	(index)
#define clFixnumIndex(fixnum)	(fixnum)

/* This operates on fixnums now, but it should use indices. */
#define clMergeHashCodes(x, y)	\
  clIntFixnum(clFixnumInt(x) + clFixnumInt(y))

typedef clObject *	clSlots;
#define clObjectSlots(slotobj)	(&(clObjectAddress(slotobj)->slots))
#define clSlotRef(slotobj, ii) 	(clObjectSlots(slotobj)[ii])
#define clGetSlot(slots, index)	clSlotRef(slots, clIndexInt(index))
#define clSetSlot(slots, index, val)	\
  clSetq(clSlotRef(slots, clIndexInt(index)), val)

/*********************************************************************
 * IMMEDIATE TYPES 
 *
 * All immediate types store the data directly in the clObject (i.e.
 * directly in a clWord).  
 *********************************************************************/

#define clObjectImmediateTag(obj, n_bits)	\
  (clObjectWord(obj) & clBitMask(n_bits))
#define clCheck2Bits(obj, tag)	(clObjectImmediateTag(obj, 2)==(tag))
#define clCheck3Bits(obj, tag)	(clObjectImmediateTag(obj, 3)==(tag))

/* Note that how these are done can depend on type.  Ex. whether shift
   is logical or arithmetic (sign extending) */
#define clToImmediate(val, tag)		clWordObject((clWord) ((val) << 2) | tag)
#define clFromImmediate(immed, type)	(((type) clObjectWord(immed)) >> 2)

/*********************************************************************
 * FIXNUM
 *
 * Implementation: Fixnums use all but the 2 low order bits for the
 * data.  Data is shifted 2 bits to make room for the tag.  
 * Lets say that cl_FIXNUM_TAG is #b01.  Note that:
 *   - an even fixnum will end in #b001 (cl_EVEN_FIXNUM_TAG), while 
 *   - an  odd fixnum will end in #b101 (cl_ODD_FIXNUM_TAG).
 * REQUIRES: 
 *    No other object can end in two bits matching cl_FIXNUM_TAG.
 *      A. No other immediate tags end in cl_FIXNUM_TAG.
 *         cl_ODD_FIXNUM_TAG=5 (#b101), keeps that tag locked out from
 *         accidental use by others types.
 *      B. No other pointer tagged data tags in cl_FIXNUM_TAG.
 * 	   (Regardless of cl_WORD_ALIGNED.)
 *      C. Data tagged objects have a pointer that ends in
 * 	   cl_DATA_TAG != cl_FIXNUM_TAG.
 *
 * Operations:
 *
 * _clFixnump(obj:clObject) => int
 *   EFFECTS: Returns non-zero if obj is fixnum (even or odd) and zero
 *     otherwise. 
 *
 * clIntFixnum(i:int) => clObject
 *   REQUIRES: i must be small enough in magnitude to fit
 *    (sizeof(clWord) * CHAR_BIT) - 2 bits.
 *   EFFECTS: Returns a fixnum representing i.
 *
 * clFixnumInt(obj:clObject) => int
 *   REQUIRES: obj must be a fixnum.
 *   EFFECTS: returns the int value of obj.
 *********************************************************************/

#define cl_FIXNUM_TAG		((int) cl_EVEN_FIXNUM_TAG)
#define _clFixnump(obj)		clCheck2Bits(obj, cl_FIXNUM_TAG)

#define clIntFixnum(ii)  	clToImmediate(ii, cl_FIXNUM_TAG)
#define clFixnumInt(obj)	clFromImmediate(obj, int)
#define clIntegerInt(obj) 	(_clFixnump(obj) ? clFixnumInt(obj) : clXintInt(obj))
int clXintInt(clProto);


#define clRemFixnum(f1, f2)		\
   clIntFixnum(clFixnumInt(f1) % clFixnumInt(f2))
#define clTruncateFixnum(f1, f2)	\
   clIntFixnum(clFixnumInt(f1) / clFixnumInt(f2))
#define clLogandFixnum(f1, f2)	\
   clIntFixnum(clFixnumInt(f1) & clFixnumInt(f2))
#define clLogxorFixnum(f1, f2)	\
   clIntFixnum(clFixnumInt(f1) ^ clFixnumInt(f2))
#define clLogiorFixnum(f1, f2)	\
   clIntFixnum(clFixnumInt(f1) | clFixnumInt(f2))

#define clLognotFixnum(f)	clIntFixnum(~clFixnumInt(f))
#define clLogbitpFixnum(index, fixnum)	\
     clTest((1<<clFixnumInt(index)) & clFixnumInt(fixnum))
#define clLogtestFixnum(f1, f2)	clTest(clFixnumInt(f1) & clFixnumInt(f2))
#define clSignpDigit(digit) clTest(clFixnumInt(digit) & (1<<(clDigitBits-1)))


/*********************************************************************
 * CHARACTER
 *
 * Implementation: Characters canu use as many as all but the 3 low order bits for the
 * data.  The actual maximum number of bits used is cl_CHARACTER_BITS.
 *
 * REQUIRES: 
 *    No other object can end in two bits matching cl_SIMPLE_CHARACTER_TAG.
 *      A. No other immediate tags end in cl_SIMPLE_CHARACTER_TAG.
 *         cl_RESERVED2_TAG, keeps that tag locked out from
 *         accidental use by others types.
 * 	 (Regardless of cl_WORD_ALIGNED.)
 *      B. Data tagged objects have a pointer that ends in
 * 	   cl_DATA_TAG != cl_SIMPLE_CHARACTER_TAG.
 *
 * Operations:
 *   
 * _clCharacterp(obj:clObject) => int
 *   EFFECTS: Returns non-zero if obj is a character, and 0 otherwise.
 *
 * clIntCharacter(c:int) => clObject
 *   REQUIRES: c is small enough to fit cl_CHARACTER_BITS.
 *   EFFECTS: Returns a character representing c.
 *
 * clCharacterInt(obj:clObject) => int
 *   REQUIRES: obj must be a character
 *   EFFECTS: returns the int representing obj.  
 *
 *********************************************************************/
#define cl_CHARACTER_BITS	((sizeof(clWord) * CHAR_BIT) - 2)
#define cl_CHARACTER_TAG	((int) cl_SIMPLE_CHARACTER_TAG)
#define _clCharacterp(obj)	clCheck2Bits(obj, cl_CHARACTER_TAG)

#define clCharacterInt(obj)	((int) clFromImmediate(obj, unsigned))
#define clCharacterWint(obj)	((wint_t) clCharacterInt(obj))
#define clCharacterChar(obj)	((char) clCharacterInt(obj))
#define clCharacterWchar(obj)	((wchar_t) clCharacterInt(obj))

#define clIntCharacter(c_char)  clToImmediate(c_char, cl_CHARACTER_TAG)
#define clWintCharacter(char)	clIntCharacter((int) (char))
#define clCharCharacter(char)	clIntCharacter((int) (char))
#define clWcharCharacter(char)	clIntCharacter((int) (char))


/* SHORT FLOAT */
/* Not currently implemented. */

/*********************************************************************
 * POINTER-TAGGED objects
 *
 * The clObject is a pointer to an clObjectCell.  The tag appears as
 * the last two or three bits of that pointer (depending on
 * cl_WORD_ALIGNED).
 *
 * Operations:
 *
 * clObjectPointerTag(obj:clObject) => clPointerTagCell
 *   REQUIRES: obj is a pointer tagged object.
 *   EFFECTS: Returns the object tag.
 *   IMPLEMENTATION EFFECTS: Only the low two bits of the pointer are
 *     returned on cl_WORD_ALIGNED systems, because machine pointers
 *     on such systems might have the third lowest bit set.  On
 *     double-word aligned systems, the low three bits of the pointer
 *     are used for the tag.
 *
 * clSetPointerTag(tag:clPointerTagCell, obj:clObject)
 *   MODIFIES: obj.
 *   EFFECTS: Sets the pointer tag of obj to tag.
 *
 * clPointerTagObjectCell(obj:clObject, tag:clPointerTagCell) =>
 *   cell_pointer:(clObjectCell *)
 *   REQUIRES: obj is a pointer tagged object.
 *   EFFECTS: returns a pointer to the clObjectCell represented by
 *     obj.
 *
 * clPointerTagp(obj:clObject, tag:clPointerTagCell) => int
 *   EFFECTS: returns non-zero if obj is a pointer-tagged object using
 *     tag as its pointer tag.  clPointerTagp() will correctly return
 *     zero for immediate or pointer tagged objects.
 *   IMPLEMENTATION: Note that immediate objects may use a different
 *     number of tag bits than pointer tagged objects, so
 *     clPointerTagp() can NOT be used to determined if obj is an
 *     immediate object using tag.  
 *********************************************************************/

#ifdef cl_CTYPECHECKS
#   define clObjectPointerTag(obj) 	_clObjectPointerTag(obj)
#   define clSetPointerTag(tag, obj)	_clSetPointerTag(tag, &obj)
#   define clPointerTagObjectCell(obj, tag) \
                                 _clPointerTagObjectCell(obj, tag)
    clPointerTagCell _clObjectPointerTag __P((clObject));
    void _clSetPointerTag __P((clPointerTagCell, clObject *));
    union clObjectCell *_clPointerTagObjectCell __P((clObject, clPointerTagCell));

#else  /* no CTYPECHECKS */
#   define clObjectPointerTag(obj)		\
 	((clPointerTagCell) (clObjectWord(obj) & cl_TAG_MASK))
#   define clSetPointerTag(tag, obj) 		\
 	(obj = (clObject) (clObjectWord(obj) | (clWord) (tag)))
#   define clPointerTagObjectCell(obj, tag) 	\
 	clObjectAddress((clObject)((char *)(obj)-((int) tag)))
#endif

/* It is not yet clear if we really need to check for NULL!!!
   For example, if we don't check clObjectClass(clEOA) will try to
   illegally access low memory.  If clObjectClass() and friends never
   gets called on clEOA/clUNBOUND, etc., then we should be able to
   remove 'clObjectAddres(obj) ? : ', below. */
#define clPointerTagp(obj, tag) 	\
     (clObjectAddress(obj) ? (clObjectPointerTag(obj)==(tag)) : 0)
#   define clObjectPointerIndex(obj) 	((int) clObjectPointerTag(obj))


/*********************************************************************
 * DATA-TAGGED objects
 *
 * The clObject is a pointer to an clObjectCell.  As far as the
 * pointer-tagged object operations are concerned, all data-tagged
 * objects look like pointer-tagged object with cl_DATA_TAG as the
 * pointer tag.  The "real" tag is kept in the data being pointed to.
 *
 * Implementation:
 *
 * We use cl_DATA_TAG=0, so the pointers can always be dereferenced
 * without supressing pointer tag bits.  
 * N.B. This may change in later versions.
 *
 * The first field of the data cell is a clDataTag that represents an
 * clObject (a class metaobject) and possibly other information.
 *
 * Operations:
 *
 * clDataTagObjectCell(obj:clObject) => (clObjectCell *)
 *   REQUIRES: obj is a data tagged object.
 *   EFFECTS: returns a pointer to the clObjectCell represented by obj. 
 *
 * clTaggedInstanceClass(obj:clObject) => clObject
 *   REQUIRES: obj must be a data tagged object.
 *   EFFECTS: Returns the tag (i.e. class metaobject) of obj.
 *
 * clSetTaggedInstanceClass(obj:clObject, class:clObject) => class
 *   REQUIRES: obj must be a data tagged object, class must be a class.
 *   EFFECTS: Makes class be reported as the class for obj.
 *
 * clObjectClass(obj:clObject) => clObject
 *   REQUIRES: obj expression must not produce side-effects.
 *   EFFECTS: Returns the tag (i.e. class metaobject) of obj,
 *     regardless of whether the object 
 *     is immediate, pointer tagged, or data tagged.
 *   IMPLEMENTATION: clObjectClass() first checks the 2 or 3 low order
 *     bits.  If zero, it gets the clObjectDataTag().  Otherwise, it
 *     uses those bits as an index into an array of class objects.  
 *********************************************************************/
/* The tag field of a data tagged clObjectCell is actually a little
 * more complicated than just an clObject because:
 *  1. We might want the field to have the same alignment as the fields
 *     of a cons so we can play games with treating the first two clWords
 *     of conses and the symbol NIL as identical in format.
 *  2. We may someday want to use unused bits of the clTagCell for
 *     other purposes.
 *  Note that this doesn't cost us anything for most data types as the
 *  field following the tag in an clObjectCell is most likely to
 *  require word alignment anyway.
 *  3. The clTagCell might actually contain both a class (clObject) and
 *     a wrapper (clObject).
 */

typedef struct {
  clIndex 	hash_key;
  clObject 	obsolete_slots;
}			clWrapperCell;

#define clObjectWrapperp(obj)		(clDataTagObjectCell(obj)->wrapper)
#define clWrapperObsoleteSlots(x)	(clObjectWrapperp(x).obsolete_slots)
#define clWrapperHashKey(x)		(clObjectWrapperp(x).hash_key)
#define clMakeWrapperObsolete(x)	clISetq(clWrapperHashKey(x), clIntIndex(0))
#define clWrapperInvalidP(x)		clTest(_clIEq(clWrapperHashKey(x), clIntIndex(0)))

typedef struct {
  union {
    clObject class;
    double dummy;
  } alignment;
  clObject wrapper;
}			clTagCell;

typedef struct {
  clTagCell tag;
}			clDataTaggedCell;

#define clDataTagObjectCell(obj)	clObjectAddress(obj)
#define clObjectTaggedInstance(obj)	(clDataTagObjectCell(obj)->data_tagged)
#define clObjectDataTag(obj)		(clObjectTaggedInstance(obj).tag)

#ifndef cl_WORD_ALIGNED
#  define clSet3BitTag(tag, obj) 	clSetPointerTag(tag, obj)
#endif


#define clTaggedInstanceClass(obj) 	(clObjectDataTag(obj).alignment.class)
#define clTaggedInstanceWrapper(obj)	(clObjectDataTag(obj).wrapper)
#define clSetTaggedInstanceWrapper(obj, new) clSetq(clTaggedInstanceWrapper(obj), new)
#define clSetTaggedInstanceClass(obj, new)   clSetq(clTaggedInstanceClass(obj), new)

#define clObjectClass(obj)			\
  (clPointerTagp(obj, cl_DATA_TAG) ? 		\
   clTaggedInstanceClass(obj) : cl_POINTER_TAGGED_CLASSES[clObjectPointerIndex(obj)])

#define clObjectWrapper(obj)			\
  (clPointerTagp(obj, cl_DATA_TAG) ? 		\
   clTaggedInstanceWrapper(obj) : cl_POINTER_TAGGED_WRAPPERS[clObjectPointerIndex(obj)])

#ifdef cl_ECLIPSE_DEFINITIONS
  clObject cl_POINTER_TAGGED_CLASSES[cl_MAX_TAG];
  clObject cl_POINTER_TAGGED_WRAPPERS[cl_MAX_TAG];
#else
  extern clObject cl_POINTER_TAGGED_CLASSES[];
  extern clObject cl_POINTER_TAGGED_WRAPPERS[];
#endif

/*********************************************************************
 * CONS
 *
 * Conses are the basic element of linked lists.  The car field holds the
 * data, while the cdr holds the next element.  The cdr of the last
 * element holds clNIL.
 *
 * Operations:
 *
 * clCons(car:clObject, cdr:clObject, clEOA) => clObject
 *   EFFECTS: returns a new cons using the specified fields.
 *
 * clConsCar(cons:clObject) => clObject
 *   REQUIRES: cons is a cons.  The
 *     argument expression must not produce side-effects.
 *   EFFECTS: Returns clNIL if the argument is the symbol NIL,
 *     otherwise returns the car of the argument.
 *
 * clConsCdr(cons:clObject) => clObject
 *   REQUIRES: cons is a cons.  The
 *     expression for cons must not produce side-effects.
 *   EFFECTS: Returns clNIL if the argument is the symbol NIL,
 *     otherwise returns the cdr of the argument.
 *********************************************************************/

typedef struct {
#if (defined cl_WORD_ALIGNED)
  clTagCell	tag;
#endif
  clObject car;
  clObject cdr;
}			clConsCell;

#if (defined cl_WORD_ALIGNED)
#  define clObjectCons(obj) 	(clDataTagObjectCell(obj)->cons)
#else
#  define clObjectCons(obj) 	\
		(clPointerTagObjectCell(obj, cl_CONS_TAG)->cons)
#endif

#define clConsCar(x)		(clObjectCons(x).car)
#define clConsCdr(x)		(clObjectCons(x).cdr)
#define clUpdateCar(x, n)	clSetq(clConsCar(x), n)
#define clUpdateCdr(x, n)	clSetq(clConsCdr(x), n)

/*********************************************************************
 * SYMBOL
 *
 * Symbols are the basic identifiers in Lisp programs.  A symbol is
 * specified by a name and a package.  
 *
 * References to a symbol as an identifier for a global variable (or
 * constant) use the value stored in the symbol.
 *
 * References to a symbol as an identifier for a global function
 * definition use the function stored in the symbol.
 *
 * References to a symbol as an identifier for a global set of
 * properties use the plist (property list) stored in the symbol.
 *
 * Operations:
 *
 * _clSymbolName(obj:clObject) => clObject
 *   REQUIRES: obj is a symbol.
 *   EFFECTS: Accesses the symbol name for reading or writing.
 *
 * _clSymbolPackage(obj:clObject) => clObject
 *   REQUIRES: obj is a symbol.  If _clSymbolPackage() is used to assign
 *     a value, (i.e. it is the first argument to clSetq(),) then it
 *     is up to the programmer to make certain that the value being
 *     assigned is consistent with the package system.
 *   EFFECTS: Accesses the symbol package for reading or writing.  A
 *     values of clNIL indicates that the symbol is not interned in
 *     any package.
 *
 * _clSymbolValue(obj:clObject) => clObject
 *   REQUIRES: obj is a symbol.
 *   EFFECTS: Accesses the symbol value for reading or writing.  The
 *     internal object clUNBOUND indicates that there is no current
 *     value.  Note that this is a different empty value result than
 *     _clSymbolFunction(). 
 *
 * _clSymbolFunction(obj:clObject) => clObject
 *   REQUIRES: obj is a symbol.  If _clSymbolFunction() is used to
 *     assign a value, (i.e. it is the first argument to clSetq(),)
 *     then it is up to the programmer to make certain that the value
 *     being assigned has all hooks properly updated.
 *     _clSetSymbolFunction() may be a better choice for making
 *     assignments.
 *   EFFECTS: Accesses the symbol function for reading or writing.
 *     The internal symbol clNIL indicates that there is no
 *     current function definition.
 *
 * _clSymbolPlist(obj:clObject) => clObject
 *   REQUIRES: obj is a symbol.
 *   EFFECTS: Accesses the symbol plist for reading or writing.  The
 *     symbol clNIL indicates that the propery list is empty.
 *********************************************************************/

typedef struct clSymbolCell {
  clTagCell	tag;
  clObject 	value;
  clObject 	function;
  clObject 	name;
  clObject 	package;
  clObject 	setf;
  clObject 	plist;
}			clSymbolCell;

#define clObjectSymbol(obj)	(clDataTagObjectCell(obj)->symbol)
#define _clSymbolName(obj)	clObjectSymbol(obj).name
#define _clSymbolPackage(obj)	clObjectSymbol(obj).package
#define _clSymbolValue(obj)	clObjectSymbol(obj).value
#define _clSymbolFunction(obj)	clObjectSymbol(obj).function
#define _clSymbolPlist(obj)	clObjectSymbol(obj).plist
#define _clSymbolSetfFunction(obj)	clObjectSymbol(obj).setf

#define clSymbolPackageValue(obj)		_clSymbolPackage(obj)
#define clSymbolPackageSetter(pkg, obj) 	clSetq(_clSymbolPackage(obj), pkg)
#define clSymbolValueValue(obj)			_clSymbolValue(obj)
#define clSetSymbolValueValue(obj, val)		clSetq(_clSymbolValue(obj), val)

#define clSymbolFunctionValue(obj)		_clSymbolFunction(obj)
#define clSymbolSetfFunctionValue(obj)		_clSymbolSetfFunction(obj)
#define clSetfExpander(symbol) clSystemProperty(symbol, clSETF_EXPANDER, clNIL)

#define clSetSymbolFunctionValue(obj, val)	clSetq(_clSymbolFunction(obj), val)
#define clSetSymbolSetfFunctionValue(obj, val)	clSetq(_clSymbolSetfFunction(obj), val)
#define clSetSetfExpander(symbol, ff) clSystemPropertySetter(symbol, clSETF_EXPANDER, ff)
clObject clSystemPropertySetter __P((clObject, clObject, clObject)),
  clSystemProperty __P((clObject, clObject, clObject));

/*********************************************************************
 * FLOAT
 *
 * Single-float corresponds to C float, and double-float corresponds
 * to C double.  These use IEEE definitions if the underlying machine
 * uses IEEE floats.  At this time, short-float == single-float and
 * long-float == double-float.  It may be that in the future,
 * short-float will be an immediate data type of less precision than
 * single-float, and long-float will be a C long double on those
 * machines which support it.
 *
 * Operations:
 *
 * clDoubleDoubleFloat(d:double) => clObject
 *   EFFECTS: Returns a double-float representing d.
 *
 * clDoubleSingleFloat(d:double) => clObject
 *   EFFECTS: Returns a single-float representing d.
 *
 * clFloatSingleFloat(f:float) => clObject
 *   REQUIRES: Both the Eclipse library and the source in which the
 *     reference to this funciton appears must both be compiled with
 *     an ANSI compiler.  
 *   EFFECTS: Returns a single-float representing f.
 *   NOTES: This function is quite error prone because of C casting
 *     rules. clDoubleSingleFloat() should almost always be used
 *     instead. 
 *
 * clDoubleFloatDouble(obj:clObject) => double
 *   REQUIRES: obj must be a double float.
 *   EFFECTS: Returns the double representing obj.  
 *
 * clSingleFloatDouble(obj:clObject) => double
 *   REQUIRES: obj must be a single float.
 *   EFFECTS: Returns the double representing obj.  
 *
 * clSingleFloatFloat(obj:clObject) => float
 *   REQUIRES: obj must be a single float.
 *   EFFECTS: Returns the float representing obj.  
 *   NOTES: C casting rules can be tricky.  clSingleFloatDouble might
 *     be safer.
 *
 *********************************************************************/

typedef struct {
  clTagCell	tag;
  float 	value;
}			clSingleFloatCell;

#define clSingleFloatFloat(obj) \
  ((clDataTagObjectCell(obj)->single_float).value)

#define clSingleFloatDouble(x)	((double) clSingleFloatFloat(x))

typedef struct { 
#if (defined cl_WORD_ALIGNED)
  clTagCell	tag;
#endif
  double 	value;
}			clDoubleFloatCell;

#if (defined cl_WORD_ALIGNED)
#  define clDoubleFloatDouble(obj) \
	  ((clDataTagObjectCell(obj)->double_float).value)
#else
#  define clDoubleFloatDouble(obj) \
 ((clPointerTagObjectCell(obj, cl_DOUBLE_FLOAT_TAG)->double_float).value)
#endif



/*********************************************************************
 * OTHER NUMBERS
 *********************************************************************/
typedef struct {
  clTagCell	tag;
  clObject	numerator;
  clObject 	denominator;
}			clRatioCell;

typedef struct {
  clTagCell	tag;
  clObject	realpart;
  clObject 	imagpart;
}			clComplexCell;

#define clRatioNumerator(n) ((clDataTagObjectCell(n)->ratio).numerator)
#define clRatioDenominator(n) ((clDataTagObjectCell(n)->ratio).denominator)

#define clComplexRealpart(n) ((clDataTagObjectCell(n)->complex).realpart)
#define clComplexImagpart(n) ((clDataTagObjectCell(n)->complex).imagpart)


/*********************************************************************
 * MATH
 *********************************************************************/


/*********************************************************************
 * SIMPLE VECTORS
 * 
 * Simple vectors have a length and a homogeneous array of C types.
 * - clSimpleVector is an array of any clObjects
 * - clSimpleBaseString elements are char representing BASE-CHAR
 * - clSimpleString elements are wchar_t representing CHARACTER
 * - clSimpleBitVector is an array of clDigit representing bundles of
 *   bits.   
 * All specialized vectors must create a clObject when elements are
 * accessed in the interpreter, though compiled code may be more
 * efficient. 
 *
 * Other specialized types may be defined in the future.
 *
 * Implementation:
 * All the simple vectors have the same first two structure elements,
 * so that the tag and length can be extracted without regard to type.
 *
 * clSimpleBaseString and clSimpleString are null terminated ('\0'),
 * even though this is not needed within any Eclipse functions.  This
 * allows C programs to easily manipulate Eclipse string data directly
 * without needing to make copies.  Note, though, that like all simple
 * arrays in Common Lisp, clSimpleBaseString and clSimpleString do not
 * have fill pointers.  The fill pointer operations on complex vectors
 * do NOT set the character pointed to by the fill-pointer to be
 * '\0'.  
 *
 * Operations:
 * 
 * clVectorSize(v:clObject) => clIndex
 *   REQUIRES: v is a simple vector (specialized or not).
 *   EFFECTS: Returns the length (i.e. number of elements
 *     stored/storable) in v.  This does not include any null
 *     termination that may be used by the implementation.
 *
 * clSimpleVectorSlots(v:clObject) => clSlots
 *   REQUIRES: v is an unspecialized simple vector.
 *   EFFECTS: Returns a pointer to the first element of the vector.
 * 
 * clGeneralElt(v:clObject, i:clIndex) => clObject
 *   REQUIRES: v is an unspecialized simple-vector.
 *   EFFECTS: Returns the element at index i.
 *
 * clSetGeneralElt(v:clObject, i:clIndex, n:clObject) => n
 *   REQUIRES: v is an unspecialized simple-vector.
 *   EFFECTS: Assigns n to the element at index i.
 *
 * clSimpleBaseStringCharp(v:clObject) => char *
 *   REQUIRES: v is simple base string.
 *   EFFECTS: Returns a pointer to the first element of the vector.
 *
 * clBaseCharElt(v:clObject, i:clIndex) => char
 *   REQUIRES: v is a simple-base-string.
 *   EFFECTS: Returns the element at index i.
 *
 * clSetBaseCharElt(v:clObject, i:clIndex, n:char) => n
 *   REQUIRES: v is a simple-base-string, n is a base-char.
 *   EFFECTS: Assigns n to the element at index i.
 *
 * clSimpleStringWcharp(v:clObject) => wchar_t *
 *   REQUIRES: v is simple-string that is not also a simple-base-string.
 *   EFFECTS: Returns a pointer to the first element of the vector.
 *
 * clExtendedCharElt(v:clObject, i:clIndex) => wchar_t
 *   REQUIRES: v is simple-string that is not also a
 *     simple-base-string.
 *   EFFECTS: Returns the element at index i.
 *
 * clSetExtendedCharElt(v:clObject, i:clIndex, n:wchar_t) => n
 *   REQUIRES: v is an unspecialized simple vector, n is a character.
 *   EFFECTS: Assigns n to the element at index i.
 *
 * clSimpleBitVectorDigitp(v:clObject) => clDigit *
 *   REQUIRES: v is simple bit vector.
 *   EFFECTS: Returns a pointer to the clDigit holding the first set
 *   of bits in the vector.
 *
 * clIndexref(v:clObject, i:clIndex) => clIndex
 *   REQUIRES: v is simple-bit-vector.
 *   EFFECTS: Returns the element at index i, with v treated as an array
 *     of digits.
 *
 * clSetIndexref(v:clObject, i:clIndex, n:clIndex) => n
 *   REQUIRES: v is a simple-bit-vector.
 *   EFFECTS: Assigns n to the element at index i, with v treated as an
 *     array of digits.
 *
 * clBitElt(v:clObject, i:clIndex) => clIndex
 *   REQUIRES: v is simple-bit-vector.
 *   EFFECTS: Returns the element at index i, with v treated as an
 *     array of bits.
 *     i may be evaluated multiple times.
 *
 * clSetBitElt(v:clObject, i:clIndex, n:clIndex) => n
 *   REQUIRES: v is a simple-bit-vector.
 *   EFFECTS: Assigns n to the element at index i, with v treated as an
 *     array of bits.
 *     n and i may be evaluated multiple times.
 *
 * clDigitref(v:clObject, i:clIndex) => clDigit
 *   REQUIRES: v is simple-bit-vector.
 *   EFFECTS: Returns the element at index i, with v treated as an
 *     array of digits.
 *
 * clSetDigitref(v:clObject, i:clIndex, n:clDigit) => n
 *   REQUIRES: v is a simple-bit-vector.
 *   EFFECTS: Assigns n to the element at index i, with v treated as an
 *     array of digits.
 *     n may be evaluated multiple times.
 *********************************************************************/
#define clCommonVectorMembers	\
  clTagCell	tag;		\
  clIndex	length

#define clStaticBaseStringCell(nn) struct { clCommonVectorMembers; char data[nn]; }
typedef clStaticBaseStringCell(1) clSimpleBaseStringCell;

#define clSimpleBaseStringCharp(v)	\
  ((clDataTagObjectCell(v)->simple_base_string).data)
#define clBaseCharElt(v, i)		\
  clSimpleBaseStringCharp(v)[clIndexInt(i)]
#define clSetBaseCharElt(v, i, n)	\
  (clSimpleBaseStringCharp(v)[clIndexInt(i)] = (n))

typedef struct {
  clCommonVectorMembers;
  clObject	data[1];	/* actually [n] */
}			clSimpleVectorCell;
#define clVectorSize(v)		\
  ((clDataTagObjectCell(v)->simple_vector).length)

#define clSimpleVectorSlots(v)		\
  ((clDataTagObjectCell(v)->simple_vector).data)
#define clGeneralElt(v, i)		\
  clSimpleVectorSlots(v)[clIndexInt(i)]
#define clSetGeneralElt(v, i, n)	\
  clSetq(clSimpleVectorSlots(v)[clIndexInt(i)], n)

typedef struct {
  clCommonVectorMembers;
  wchar_t	data[1];	/* actually [n] */
}			clSimpleStringCell;
#define clSimpleStringWcharp(v)		\
  ((clDataTagObjectCell(v)->simple_string).data)
#define clExtendedCharElt(v, i)		\
  clSimpleStringWcharp(v)[clIndexInt(i)]
#define clSetExtendedCharElt(v, i, n)	\
  (clSimpleStringWcharp(v)[clIndexInt(i)] = n)

/* Longest string of bits which can be operated on by the machine.
   Bignums are treated as arrays of one or more of these "digits". */
typedef clIndex clDigit;	/* could be unsigned short */

/* Note that length field is number of bits, not number of digits.
   Furthermore, the number of bits might not be a multiple of
   clDigitBits. */
typedef struct {
  clCommonVectorMembers;
  clDigit	data[1];	/* actually [n] */
}			clSimpleBitVectorCell;
#define clSimpleBitVectorDigitp(v)	\
  ((clDataTagObjectCell(v)->simple_bit_vector).data)

#define clIndexref(v, i)	\
 clSimpleBitVectorDigitp(v)[clIndexInt(i)]
#define clSetIndexref(v, i, n)	\
 clISetq(clSimpleBitVectorDigitp(v)[clIndexInt(i)], n)
#define clDigitref(v, i)	\
 clIndexFixnum(clIndexref(v, i))
#define clSetDigitref(v, i, n)	\
 (clSetIndexref(v, i, clIntIndex(clFixnumInt(n) & clBitMask(clDigitBits))), n)

#define clDigitBits 		(sizeof(unsigned short) * CHAR_BIT)
#define _clBitWord(v, i) 	\
     clSimpleBitVectorDigitp(v)[clIndexInt(i)/clDigitBits]
#define clBitWord(v, i)		clIndexInt(_clBitWord(v, i))

/* Controls how bits are packed within a digit. */
#ifdef DIGIT_BIT_0_IS_MSB
#  define clBitShiftIndex(i) 	\
     (clDigitBits - 1 - (clIndexInt(i) % clDigitBits))
#else
#  define clBitShiftIndex(i) 	(clIndexInt(i) % clDigitBits)
#endif

#define clBitElt(v, i)		\
  clIntIndex(1 & (clBitWord(v, i) >> clBitShiftIndex(i)))
#define clSetBitElt(v, i, n)	\
  clISetq(_clBitWord(v, i), 	\
	  clIntIndex(clIndexInt(n) ?  	\
		     (clBitWord(v, i) | (1 << clBitShiftIndex(i)))  : 	\
		     (clBitWord(v, i) & ~(1 << clBitShiftIndex(i)))))

#define clBignumXint(bignum)	(bignum)
#define clXintBignum(xint)	(xint)
#define clXintInt(xint)	\
     (clIndexInt(clIndexref(xint, clIntIndex(0))) + 	\
      (clIndexInt(clIndexref(xint, clIntIndex(1))) << clDigitBits))
#define clXintFixnum(xint)	clIntFixnum(clXintInt(xint))
#define clNDigitsForBits(nbits)	clCeil(nbits, clDigitBits)
#define clXintSize(xint)	\
     clIntFixnum(clNDigitsForBits(clIndexInt(clVectorSize(xint))))

/*********************************************************************
 * ARRAYS
 *********************************************************************/
#define clCommonArrayMembers	\
  clTagCell	tag;		\
  clObject	contents;	\
  clIndex 	dimensions[7];	\
  clIndex	rank

typedef struct {
  clCommonArrayMembers;
}			clSimpleArrayCell;

#define clSimpleArrayContents(a)	\
  ((clDataTagObjectCell(a)->simple_array).contents)
#define clSimpleArrayRank(a)	\
  ((clDataTagObjectCell(a)->simple_array).rank)
#define clSimpleArrayDimension(a, i)	\
  ((clDataTagObjectCell(a)->simple_array).dimensions[clIndexInt(i)])

typedef struct {
  clCommonArrayMembers;
  clObject	fill_pointer;
  clObject	offset;
}			clComplexArrayCell;

#define clComplexArrayFillPointer(a)	\
  ((clDataTagObjectCell(a)->complex_array).fill_pointer)
#define clSetComplexArrayFillPointer(a, n)	\
  ((clDataTagObjectCell(a)->complex_array).fill_pointer = (n))
#define clComplexArrayOffset(a)	\
  ((clDataTagObjectCell(a)->complex_array).offset)

/*********************************************************************
  OPEN-ADDRESS-HASH-TABLE
 *********************************************************************/
typedef struct {
  clTagCell	tag;
  clSlots	keys;
  clSlots	values;
  clIndex	n_buckets;
  clIndex	size;
  clIndex	count;
  clObject	rehash_size;
}			clOpenAddressHashTableCell;

#define clObjectHashTable(obj)		(clDataTagObjectCell(obj)->open_address_hash_table)
#define clOpenAddressHashTableValues(hh)	clObjectHashTable(hh).values
#define clOpenAddressHashTableKeys(hh)		clObjectHashTable(hh).keys
#define clOpenAddressHashTableNBuckets(hh)	clObjectHashTable(hh).n_buckets
#define clOpenAddressHashTableSize(hh)		clObjectHashTable(hh).size
#define clOpenAddressHashTableCount(hh)		clObjectHashTable(hh).count
#define clOpenAddressHashTableRehashSize(hh)	clObjectHashTable(hh).rehash_size
#define clOpenAddressHashTableValuesSetter(ss, hh)	\
  (clObjectHashTable(hh).values = ss)
#define clOpenAddressHashTableKeysSetter(xx, hh)	\
  (clObjectHashTable(hh).keys = xx)
#define clOpenAddressHashTableNBucketsSetter(ii, hh)	\
  clISetq(clObjectHashTable(hh).n_buckets, ii)
#define clOpenAddressHashTableSizeSetter(ii, hh)	\
  clISetq(clObjectHashTable(hh).size, ii)
#define clOpenAddressHashTableCountSetter(ii, hh)	\
  clISetq(clObjectHashTable(hh).count, ii)
#define clOpenAddressHashTableRehashSizeSetter(xx, hh)	\
  clSetq(clObjectHashTable(hh).rehash_size, xx)

#define clEqHash(x)	clIntFixnum(clObjectWord(x))

/*********************************************************************
 * CAPTURED LEXICAL ENVIRONMENTS AND CLOSURES
 *
 * Each Lisp function is implemented, in part, by an clMFunction -- a
 * machine-compiled C function which returns an clObject.  For each
 * Lisp function which has been machine compiled, there is one
 * clMFunction which "does the work" of that function.
 *
 * A Lisp function object is a tagged object which can be created at
 * runtime, examined, printed, stored in variables, and passed as the
 * first argument to the functions clFuncall and clApply.  We
 * represent a Lisp function object as a clClosureCell which records
 * the type tag and the "implementing" clMFunction.  In part,
 * clFuncall works by calling the clMFunction from the clClosureCell
 * on the arguments that clFuncall is given.
 *
 * Lisp functions can be defined inside other functions, and the inner
 * definition can refer to lexical variables defined in the enclosing
 * function.  C function definitions, though, may not be nested, and
 * the C compiler can only resolve lexical references to the current
 * (inner) function -- i.e. it can only compute access to that part of
 * the stack defined by the function itself.
 *
 * If a C function needs to refer to C variables which correspond to
 * Lisp variables defined by the enclosing Lisp function, it must make
 * use of a captured lexical environment representation.
 *
 * The captured lexical environment represents the bindings in the
 * enclosing functions, not just their values.  For example, if a
 * binding is written to using clSetq() in the inner function, the value
 * of the binding in the outer function changes as well.
 *
 * Both a clMFunction and a (possibly empty) captured lexical
 * environment is necessary to fully represent a Lisp function.  The
 * clMFunction is written so as to make use of the captured lexical 
 * environment, if necessary, as stored in the closure.
 *
 * There are two interesting special cases to point out:
 *
 * 1. Consider a function which contains a loop which iterates 10
 *    times.  Within the loop, several new local variables are stack
 *    allocated.  Each time through the loop, a local function is
 *    defined which captures these new variables.  The loop then
 *    produces 10 closures, each of which captures a different set of
 *    lexical variables.  The code for each local function is the
 *    same, only the captured bindings are different.  Accordingly,
 *    only one clMFunction is defined, which is refered to by each of
 *    the 10 different clClosureCells.
 *
 * 2. The interpreter is an clMFunction.  It is designed to execute
 *    Lisp code, stored in the captured lexical environment.  Thus all
 *    Lisp interpreted function objects are clClosureCells which have
 *    the same clMFunction (the interpreter) as the stored function,
 *    and a list representation of the particular Lisp function code
 *    stored in some element of the captured lexical environment.
 *    Byte-compiled code is similar.
 *
 * Implementation:
 *
 * The captured lexical environment is an array of clBindings --
 * addresses of locations which hold clObjects.  A clClosureCell
 * includes a (possibly 0 length) array of clBindings to represent the
 * captured lexical environment.
 *
 * We could require that the environment array be passed as an
 * additional argument to the function.  However, anyone writting C
 * code which called the function directly would then have to know
 * whether or not to pass in this extra argument, and it would be poor
 * modularity for callers to have to know how a function was
 * implemented.  Instead we place a pointer to the first clBinding of
 * the environment array in a known place, unique for each clMFunction,
 * which the clMFunction knows how to gain access to.  In most cases,
 * this evironment hook never changes.  However, in the special cases
 * enumerated above, the environment does change for different
 * executions of the clMFunction.  We don't require the user to go
 * around setting this hook, but instead, we have the system functions
 * which access closures do the setting.
 *
 * Each clClosureCell stores the address of the environment hook for
 * the clMFunction.  (ex. Different closures which use the same
 * clMFunction will use the same stored environment hook address.)
 * The functions clApply(), clFuncall() and _clSetSymbolFunction()
 * each receive a closure object as an argument.  Each updates the
 * hook address specified by the closure with the address of the first
 * clBinding of the closure.  (clApply() and clFuncall() effectively
 * BIND the hook.  i.e. they set it before calling the clMFunction,
 * and reset it to the old value after the function completes.  This
 * ensures that distinct closures which use the same implementing C
 * function can call each other.)
 *
 * Operations:
 *
 * clClosureFunction(obj:clObject) => clMFunction
 *   REQUIRES: obj is a closure.
 *   EFFECTS: Returns the clMFunction which implements the closure.
 *
 * clClosureEnvironment(obj:clObject) => clBinding[]
 *   REQUIRES: obj is a closure with a non-empty captured lexical
 *     environment. 
 *   EFFECTS: Returns the address of the first clBinding in the
 *     captured lexical environment. 
 *
 * clClosureHook(obj:clObject) => clBinding **
 *   REQUIRES: obj is a closure.
 *   EFFECTS: Returns the address of the environment hook, i.e. the
 *     place where the clMFunction will expect to find the address of
 *     the first clBinding in the captured lexical environment.  This
 *     will be a clNULL_HOOK if the closure does not use a captured
 *     lexical environment. 
 *
 * clMakeClosure(n:unsigned, f:clMFunction, h:clBinding **,
 *               b0:clBinding, b1:clBinding, ...) => clObject
 *   REQUIRES: The number of bn arguments is n.  h is the address of
 *     the environment hook used by f.  f uses exactly n captured
 *     variables. 
 *   EFFECTS: Returns an clObject representing the specified closure.
 *     The location pointed to by h is initialized to point to the
 *     start of the environment within the closure.   
 *   NOTES:
 *     If f does not make use of a captured lexical environment
 *       (i.e. n=0), h may be clNULL_HOOK.  
 *   EXAMPLE: 
 *   { clObject f, x, y, *z=clMakeBinding(); int i = 4;
 *     clSetq(x, clIntFixnum(1));
 *     clSetq(y, clIntFixnum(2));
 *     clSetq(*z, clIntFixnum(3));
 *     clSetq(f, clMakeClosure(4, usrFoo, &clEnvHook(usrFoo),
 *                             &x, &y, z, (clBinding) &i)); 
 *     ... }
 *     Note that the other pointers may be cast to clBindings.
 *
 * N.B. Closures are immutable objects, and the implementation is free
 * to make copies of closures at any time. Specifically, if some C
 * program manipulates the closure fields within some closure, these
 * changes will not necessarilly propogate to all existing uses of
 * that closure.
 *********************************************************************/

typedef clObject (*clMFunction)(clProto);
#define clCommonInstanceMembers clTagCell tag; clObject slots 
typedef clObject *clBinding;
typedef struct {
  clCommonInstanceMembers;	/* slots are used for source, name, etc. */
  clMFunction	function;
  clBinding **	hook;
  clBinding *	environment;
} 			clClosureCell;

#define clObjectClosure(v)	(clDataTagObjectCell(v)->closure)
#define clClosureFunction(v)	clObjectClosure(v).function
#define clClosureHook(v)	clObjectClosure(v).hook
#define clClosureEnvironment(v)	clObjectClosure(v).environment


#  ifndef cl_ECLIPSE_DEFINITIONS
     extern
#  endif
   clBinding *_clNULL_HOOK;
#define clNULL_HOOK  &_clNULL_HOOK

clBinding	clMakeBinding __P((void));


/*********************************************************************
  INSTANCES
 *********************************************************************/
typedef struct clStandardInstanceCell {
  clCommonInstanceMembers;
}			clStandardInstanceCell; 

typedef clClosureCell clFuncallableStandardInstanceCell; 

#define clObjectStandardInstance(obj)	(clDataTagObjectCell(obj)->standard_instance)
#define clStandardInstanceSlots(obj) 	(clObjectStandardInstance(obj).slots)
#define clSetStandardInstanceSlots(object, new) \
  clSetq(clStandardInstanceSlots(object), new)

#define clFuncallableStandardInstanceFunction(object)	object

#define clFastSlotRef(xx, nn) 	clSlotRef(clStandardInstanceSlots(xx), nn)
#define clEmfTable(fin)		clFastSlotRef(fin, 9)
#define clInstanceTag(x) 	clFastSlotRef(clObjectClass(x), 2)
#define clInstanceCpl(x) 	clFastSlotRef(clObjectClass(x), 6)

/*********************************************************************
 * EXITS
 *
 * Exits are UNTAGGED object cells which maintain information about
 * the state of computation.  Constructs such as clReturnFrom(),
 * clGo(), or clThrow() can be used to jump back to an earlier point
 * in the computation, as recorded by the clExitCell.  Since they are
 * not tagged, exits are not full Lisp clObjects and cannot generally
 * be passed around from one function to another.  The constructs
 * which generate and use exits are described in PART III, below.
 *********************************************************************/

typedef clObject	clMultipleValuesElement;

#ifdef EXPANDABLE_CONTROL_STACK
   typedef unsigned short		clDynamicEnv;
#else
   typedef union clControlOpCell *	clDynamicEnv;
#endif

typedef struct {
  jmp_buf 			machine_state;
  clMultipleValuesElement *	values;
  clDynamicEnv 			dynamic_environment;
  unsigned short 		exit_flag;
} 		clExitCell;

/*********************************************************************
 * OBJECT CELLS 
 *********************************************************************/
typedef union clObjectCell {
  /* May be pointer or data tagged, depending on cl_WORD_ALIGNED. */
  clConsCell 			cons;
  clDoubleFloatCell 		double_float; 

  /* Data tagged. */
  clDataTaggedCell 		data_tagged;
  clSymbolCell 			symbol;
  clSingleFloatCell 		single_float;
  clRatioCell			ratio;
  clComplexCell			complex;
  clClosureCell 		closure;
  clSimpleVectorCell		simple_vector;
  clSimpleBaseStringCell	simple_base_string;
  clSimpleStringCell		simple_string;
  clSimpleBitVectorCell		simple_bit_vector;
  clSimpleArrayCell		simple_array;
  clComplexArrayCell		complex_array;
  clOpenAddressHashTableCell	open_address_hash_table;
  clStandardInstanceCell	standard_instance;

  /* Not tagged. Only the compiler really knows...*/
  clExitCell 		exit;
  clWrapperCell		wrapper;
  clObject		slots;
}			clObjectCell;

/*********************************************************************
 * OTHER PREDICATES
 *
 * clTest(x:int) => clObject
 *   EFFECTS: Returns clT if x is non-zero, otherwise clNIL.
 *
 * clPresentp(x:clObject) => clObject
 *   REQUIRES: x is an function argument.
 *   EFFECTS: Returns clT if x was present in the argument list,
 *     (i.e. an actual Lisp value was supplied, not clEOA), otherwise
 *     clNIL. 
 *
 * clTrue(x:clObject) => int
 *   EFFECTS: Returns non-zero if x is not nil, otherwise zero.  
 *   USAGE: The Lisp idiom (if x y z) translates to C as:
 *     if (clTrue(x)) y; else z;
 *
 *********************************************************************/
/* N.B. It is best to use values that have the last three bits ==
   cl_RESERVED_TAG so that your flag won't be confused with any
   Lisp value. */
#ifdef cl_CTYPECHECKS
#  ifndef cl_ECLIPSE_DEFINITIONS 
     extern
#  endif
       clObject clFLAG0, clFLAG1;
   int clTrue __P((clObject));
#else  
#  define clTrue(obj)		_clTrue(obj)
#  define clFLAG0		((clObject) 0)
#  define clFLAG1		((clObject) -1)
#endif

#define clEOA		clFLAG0
#define clUNBOUND 	clFLAG0

#define clTest(int)	((int) ? clT : clNIL)
#define _clTrue(obj) 	(clObjectAddress(obj)!=clObjectAddress(clNIL))
#define _clPresentp(x)	(!_clEq((x), clEOA))
#define _clUnboundp(x)	(_clEq((x), clUNBOUND))
/* Internal version of Lisp predicates.  Note:
 *   - No trailing clEOA in call.
 *   - No multiple values produced.
 *   - Value is int, not clObject. 
 */
#define _clEq(x, y) 	(clObjectAddress(x)==clObjectAddress(y))
#define _clNull(x)	_clEq((x), clNIL)
#define _clNot(x)	_clEq((x), clNIL)

#define clPresentp(x)	clTest(_clPresentp(x))
#define clUnboundp(x)	clTest(_clUnboundp(x))


/*********************************************************************
 *********************************************************************
 *		       PART II: FUNCTION CALLS
 *		       -----------------------
 *********************************************************************/
/*********************************************************************
 * FUNCTION CALLS
 *
 * A typical C function definition for the Lisp function USER:FOO is:
 *
 *   clObject usrFoo(...) ... { ... }
 *
 * One can call usrFoo() directly, as in usrFoo(a, b, clEOA).
 *
 * It is also possible to completely machine compile an entire
 * application which includes calls to functions which are not (and
 * perhaps never will be) machine compiled.  In this case, it is
 * necessary to call the function in C as: clCall(usrFOO)(a, b,
 * clEOA).  This indirects through the closure stored in the
 * symbol-function slot of the symbol usrFOO.
 *
 * Implementation:
 *
 * clCall() is used when the highest safety level is specied by the
 * user, when the function is declared NOTINLINE, or when the function
 * being called is neither defined earlier in the file being compiled
 * nor as a machine function in the Lisp which is doing the compilation.
 * Otherwise, the call is made directly to the C function:
 * ex. usrFoo(..., clEOA).  However, if the function FOO is redefined,
 * existing machine compiled calls will not see the new definition.  
 *
 * Internal Calls:
 *
 * When internal calls are permitted and the compiler judges a
 * function definition to be internalizable, it may generate two
 * machine functions:
 *
 *   some_type _usrFoo(...) ... { ... }
 *   clObject usrFoo(...) ... { ... _usrFoo(...) ... }
 *   clDeclareHook(usrFoo);
 *
 * The internal function, _usrFoo() takes a fixed number of arguments
 * and returns a single value through the normal C function call
 * mechanism.  In addition, arguments and the returned value may be C
 * primitive types instead of Lisp Objects.  The normal function,
 * usrFoo(), calls the internal one, _usrFoo().
 *
 * When direct calls AND internal calls are permitted AND the compiler
 * agrees, a call to FOO will be compiled as _usrFoo(...).  This saves
 * argument parsing and may save "boxing" (heap allocation) of values.
 * No runtime checking of the type or number of arguments is
 * performed.  As with normal direct calls, machine compiled calls
 * will not see new definitions for FOO.
 *
 * Operations:
 *
 * clCall(symbol:clObject)(arg1:clObject, arg2:clObject, ..., clEOA) 
 *     => clObject 
 *   REQUIRES: symbol is a symbol. Evaluating symbol has no side-effects.
 *     The expressions that make op arg1, arg2, ..., argn must not
 *     involve any other uses of clCall, either directly or
 *     indirectly.  In practice, this means that each argument must be
 *     a reference to a temporary variable.  
 *   SIGNALS: Control-error if symbol is not fbound.
 *   EFFECTS: Calls the specified function on the given arguments,
 *     returning the result.  This will work even if symbol is not
 *     given a function until runtime.
 *********************************************************************/

#define clBaitHook(v) 	(*clClosureHook(v) = clClosureEnvironment(v))
#define clCallClosure(v) (*(clBaitHook(v), clClosureFunction(v)))
#define clCall(symbol)	clCallClosure(clSymbolFunction(symbol, clEOA))

/* Shared by APPLY and MULTIPLE-VALUES machinery. */
clObject _clControlError __P((const char *));

#define clCheckCount(ii, limit, message) \
  if (ii++ == limit) (void) _clControlError(message)

#define clArgArray(id) 				\
    id[0], id[1], id[2], id[3], id[4],	 	\
    id[5], id[6], id[7], id[8], id[9],		\
    id[10], id[11], id[12], id[13], id[14], 	\
    id[15], id[16], id[17], id[18], id[19], 	\
    id[20], id[21], id[22], id[23], id[24], 	\
    id[25], id[26], id[27], id[28], id[29], 	\
    id[30], id[31], id[32], id[33], id[34], 	\
    id[35], id[36], id[37], id[38], id[39], 	\
    id[40], id[41], id[42], id[43], id[44], 	\
    id[45], id[46], id[47], id[48], id[49], id[50]


/*********************************************************************
 * ARGUMENT PARSING 
 * 
 * To support optional, named and collected arguments (&optional, &key
 * and &rest in Lisp), all C functions corresponding to Lisp functions
 * are called with arguments terminating in clEOA and are defined
 * using variable argument syntax.
 * 
 * Example: The following function takes one required argument named
 * arg, an optional argument opt, with a default value of 1, and a
 * named argument x, with a default value of 2.  If arg is not
 * supplied, clMissingArgs() is called.  If, after the required and
 * optional arguments are specified, a named argument other than X is
 * supplied, clCheckKeys() triggers an error.
 *
 * clObject clFoo clVdecl(_ap)
 * { clObject arg, opt, opt_p, x_p, x, keys, tmp;
 *   { clBeginParse(_ap);
 *     clSetq(arg, clVp(_ap) ? clVpop(_ap) : clMissingArgs());
 *     clSetq(opt,  ? clVpop(_ap) : I_1);
 *     clSetq(keys, clVp(_ap) : clVargs(_ap));
 *     clSetq(tmp, clKeyArg(keyX, keys, clEOA));
 *     clSetq(x, clTrue(tmp ? clCar(tmp, clEOA) : I_2));
 *     clCheckKeys(keys, clList(keyX, clEOA));  
 *     clEndParse(_ap); }
 *    ... }
 *
 * IMPLEMENTATION NOTES: Curent Eclipse code generation uses
 * "(clVaAlist) clVaDcl" rather than clVdecl(id).  clVaAlist
 * "hardwires" '_ap' as the id.  When clVaAlist is used, id must be
 * _ap when using other parsing macros in the same function.
 * 
 * Operations:
 *
 * clVdecl(id:variable_name)
 *   EFFECTS: Declares a C parameter list in a Lisp function
 *     definition.
 *   IMPLEMENTATION EFFECTS: See clBeginParse().
 *
 * clBeginParse(id:variable_name)
 *   REQUIRES: Only declaration statements can precede clBeginParse()
 *     in the enclosing code block.  Id must be unique with respect to
 *     variables of overlapping scope, and must match id in a
 *     clVdecl() for the current function.
 *   EFFECTS: Sets up argument parsing.
 *   IMPLEMENTATION EFFECTS: In conjuction with clVdecl(), defines an
 *     clObject variable named clCurrentArg(id) and initializes it to
 *     the first argument. May also define an clObject variable named
 *     clArgTmp(id). 
 *
 * clEndParse(id:variable_name)
 *   REQUIRES: id matches that used in clBeginParse() in the same code
 *     block. 
 *   EFFECTS: Ends argument parsing.
 * 
 * clVp(id:variable_name) => clObject
 *   REQUIRES: id matches that used in clBeginParse() in the same code
 *     block. 
 *   EFFECTS: Returns true if an unpopped variable argument is
 *     present.
 *
 * clVpop(id:variable_name) => clObject
 *   REQUIRES: An unpopped variable argument must be available, as
 *     tested by clVp(id).
 *   EFFECTS: Returns the next variable argument.
 *   MODIFIES: The variable argument machinery is effected such that
 *     the next call to clVp(id) will test whether an argument
 *     immediately after the one being popped is present.
 *
 * clVargs(id:variable_name) => clObject
 *   REQUIRES:  An unpopped variable argument must be available, as
 *     tested by clVpop(id).
 *   EFFECTS:  Returns a list of all the unparsed arguments.
 *
 *********************************************************************/
#define cl_CALL_ARGUMENTS_LIMIT	50

#ifdef __STDC__
#  include <stdarg.h>
#  define clVdecl(ap)		(clObject clCurrentArg(ap), ...)
#  define clVaAlist		clObject clCurrentArg(_ap), ...
#  define clVaDcl		
#  define clVa_Start(args)	va_start(args, clCurrentArg(args))
#  define clDeclareVaCur(args)	
#  define clInitVaCur(args)	
#  define clArgNext(args)	clArgLast(args)
   clObject _clVlist(clObject, va_list *);
#else
#  include <varargs.h>
#  define clVdecl(ap)		(clVaAlist) clVaDcl
#  define clVaAlist		va_alist
#  define clVaDcl		va_dcl
#  define clVa_Start(args)	va_start(args)
#  define clDeclareVaCur(args)  clObject clCurrentArg(args); 
#  define clInitVaCur(args)	clSetq(clCurrentArg(args), _clGetObjectArg(args))
   clObject _clVlist __P((clObject, va_list *));
#endif

#define clCurrentArg(args)      clPaste2(CLcurrent,args)
#define clArgTmp(args)      	clPaste2(CLparse_temp,args)
#define _clVp(args)		_clPresentp(clCurrentArg(args))
#define clVp(args)		clTest(_clVp(args))
#define clVpop(args)		\
  (clSetq(clArgTmp(args), clCurrentArg(args)),	\
   clSetq(clCurrentArg(args), _clGetObjectArg(args)),	\
   clArgTmp(args))
#define clVargs(args)		_clVlist(clCurrentArg(args), &args)

#define clBeginParse(args)			\
  va_list args; clObject clArgTmp(args); clDeclareVaCur(args); \
  (void) clArgTmp(args); clVaStart(args); clInitVaCur(args)
#define clEndParse(args)	clVaEnd(args)

#define clGetObjectArg(ap)	(_clVp(ap) ? clVpop(ap) : clEOA)
#define clVlist(ap) 		(_clVp(ap) ? clVargs(ap) : clNIL)

/* Lint on some platforms (SunOS) can't even deal with the native
 * va_start() macro, so when LINTing, we use something which doesn't
 * do anything (but doesn't complain either).  Some systems (HP) also
 * have a  poorly defined definition of va_arg(), which can give rise
 * to complaints of casts introducing widening alignment, etc. */
#ifdef LINT
   extern va_list 		clIgnoreArg _P((clObject));
#  define clVaStart(args) 	args = clIgnoreArg(va_alist)
   clObject _clGetObjectArg __P((va_list));
#else
#  define clVaStart(args)	clVa_Start(args)
#  define _clGetObjectArg(ap) 	va_arg(ap, clObject)
#endif 

#define clVaEnd(args)		va_end(args)

/*********************************************************************
 * ENCLOSED ENVIRONMENTS
 *
 * All calling mechanisms (even direct and internal calls) are
 * independent of whether the called function happens to make use of
 * variables from the defining environment.
 *
 * Here is a typical C function definition for the Lisp function
 * USER:FOO which uses variables from its defining environment:
 *
 *   clDeclareEnv(Foo);
 *   clObject usrFoo(...) ...
 *   { clUseEnvironment(Foo);
 *     ... }
 *
 * Within the body of usrFoo(), clObjects in the captured lexical
 * environment may be referenced as clEnv(0, x), clEnv(1, y), etc.,
 * for the first clObject in the enclosed environment (which happens to
 * be named x), the second clObject (named y), etc.
 *
 * Data of types other than clObject can be captured in the
 * environment.  To reference them, one uses clCEnv(type, n), where
 * type is the type of the data captured.
 *
 * Dynamic-extent vs Indefinite-extent Variables:
 * 
 * The clBindings might just be the addresses of the local variables
 * in the outer function.  These are called dynamic extent variables
 * -- their extent (validity) ends when the defining code block ends
 * and the variables are "popped" from the C stack.
 *
 * However, a clClosureCell may be created and RETURNED from a
 * function.  It may then be applied or funcalled later.  In this case
 * any closed over variables must have indefinite extent -- they must
 * still be valid even after the function which first defined the
 * variables has ended.  In this case, the local variables in the
 * outer function must not simply be of type clObject, but (clObject
 * *).  We must heap allocate a clBinding to hold the pointer to the
 * clObject data.
 *
 * Operations:
 *
 * clEnv(n:unsigned, id) => clObject
 *   REQUIRES: The current function calls clUseEnvironment(), the
 *     environment defined for the current function has at least n
 *     captured bindings, and the nth binding (0 based) holds an
 *     clObject.
 *   EFFECTS: Returns the value of the nth binding from the
 *     environment for reading or writing.  (i.e. clSetq() may be used
 *     with clEnv() as the first argument.)  The ordering referred to
 *     by n is the same as the order presented to clMakeClosure(). id is
 *     ignored and serves as documentation. 
 *
 * clCEnv(val_type:type_name, n:unsigned) => value:val_type
 *   REQUIRES: The current function calls clUseEnvironment(), the
 *     environment defined for the current function has at least n
 *     captured bindings, and the nth binding (0 based) hold data of
 *     type val_type.
 *   EFFECTS: Returns the value of the nth binding from the environment
 *     for reading or writing.  (i.e. If the = operator is acceptable
 *     for data of type val_type, then = may be used with clCEnv() as
 *     the left-hand-side.)  The ordering referred to by n is the same
 *     as the order presented to for clMakeClosure().
 *   
 * clUseEnv(id:function_name)
 *   REQUIRES: clDeclareEnv(id) is used for a matching id.  Only
 *     declaration statements can precede clUseEnv() within the function
 *     in which it appears.
 *   EFFECTS: Sets up the environment within the function id() so that
 *     clEnv() and clCEnv() will work.  
 *   IMPLEMENTATION EFFECTS: clUseEnv() is defined so as to cache the
 *     address of the first clBinding when it is called, for later use
 *     by clEnv()/clCEnv().  This ensures the function will work
 *     properly even if the current clMFunction (or some functin that it
 *     calls) creates a new and different closure which happens to also
 *     use the same clMFunction.  In fact, since the environment address
 *     is cached as a lexical C varible in the funtion, it will be
 *     "reset" properly if a transfer to an exit sends us back to an
 *     earlier invokation of a function which has had its environment
 *     hook set somewheres along the way.
 *
 * clDeclareEnv(id:function_name) 
 *   REQUIRES: id is an identifier denoting a C function.
 *     clDeclareEnv() must appear at top level in the C file (i.e. like
 *     a global variable) before the definition of the function itself.
 *   EFFECTS: Establishes a static hook pointing to the environment for
 *     the specified function. clMakeClosure() initialize this variable
 *     to point to the captured lexical environment in which the C
 *     function is to be run.
 *
 * clMakeBinding() => clBinding
 *   EFFECTS: Returns an indefinite extent binding which holds an
 *     clObject.
 *   USAGE: { clObject *x = clMakeBinding(); ...} Now *x can be used
 *     exactly like any lexical clObject variable, with the additional
 *     benefit that a closure over *x (i.e. clMakeClosure(n, f, h,
 *     &*x, ...) or simply clMakeClosure(n, f, h, x, ...)) will
 *     continue to work even after the extent of *x has ended.
 *********************************************************************/

#define clEnvHook(fname)	clPaste2(fname,_env)

#define clDeclareEnv(fname) 	/* sets up the hook */\
  static clBinding *clEnvHook(fname)

#define clUseEnv(fname)    	/* defines CL_environment as local var */\
  clBinding *CL_environment = clEnvHook(fname)

#define clEnv(n, id)		(*CL_environment[n])
#define clCEnv(type, n)		(*((type *) CL_environment[n]))


#ifdef __STDC__
#  define clMakeClosureProto() \
   clMakeClosure(int n, clMFunction mfunc, clBinding **hook, ...) 
   clObject clMakeClosureProto();
#else
   clObject	clMakeClosure();
#endif

/*********************************************************************
 * MULTIPLE VALUES
 *
 * Instead of always returning one value, Lisp defines certain
 * functions and constructs for producing zero or more values, and
 * certain constructs for receiving these values.
 *
 * In some sense, all Lisp functions and constructs always produce
 * multiple values, though in fact, the NUMBER of "multiple" values is
 * usually one. When these values are to be assigned to a variable or
 * passed as an argument to a function, only the first value -- the
 * primary value -- is used.  In most other cases, a construct passes
 * on all the values produced by some subexpression.
 *
 * Eclipse uses the normal C evaluation and return mechanism to pass
 * the primary value.  For example, C functions implementing Lisp
 * functions return one clObject through the normal C return
 * mechanism.  This is the primary value.
 *
 * The other values (if any) are maintained by the internal multiple
 * values machinery, and are visible only through the constructs
 * defined for receiving multiple values.  Eclipse simulates the
 * passing of multiple values dynamically -- the most recent multiple
 * values produced are the ones seen by any receiving construct.
 *
 * The function clValues() produces multiple values, one for each
 * argument.  It returns the primary value through the normal C return
 * mechanism.  clValues1() is a more efficient version for the single
 * argument case.  All true Lisp functions, even those written in C,
 * should use one of these to produce multiple values.  Note, though,
 * that such functions can rely on all other Lisp functions to produce
 * multiple values.  For example, the following definition is fine:
 *
 * clObject MyFunction(x) clObject x;
 * { if (_clNull(x)) return(clValues1(clNIL));
 *   else if (_clSymbolp(x)) return(clValues(x, CLT, clEOA));
 *   else return(SomeOtherFunction(x)); }
 *
 * If x is NIL, clValues1() is used to produce one "multiple" value.
 * If x is a different symbol, clValues() is used to produce two
 * multiple values.  Otherwise, SomeOtherFunction() is called.  We
 * rely on SomeOtherFunction to obey the rules and produce multiple
 * values itself.  In all cases, a single primary clObject value is
 * returned through the normal C return mechanism.  Also, in each
 * case, the last values produced (by clValues1(), clValues(), or
 * SomeOtherFunction()) before returning to MyFunction's caller are
 * the ones which will be seen by any receiving constructs in the
 * caller.
 *
 * One way to make use of multiple values is to collect them up as
 * arguments to a function.  (Remember that usually a function only
 * sees the primary value of each argument expression.)
 * clMultipleValueCall() arranges this.
 *
 * Implementation:
 *
 * Multiple values are always written to an array of clObjects
 * pointed to by the global variable CL_current_values.  We could have
 * a null CL_current_values indicate that multiple values are not to
 * be produced, but since clValues1() is used so frequently, we want
 * it to be as small and quick as possible, so it doesn't do any
 * branching.
 *
 * clMultipleValueProg1() and clMultipleValueCall() both establish new
 * stack-allocated arrays for accumulating values.
 *
 * MULTIPLE-VALUE-PROG1 begins using the new (junk) buffer after the
 * first form has been evaluated, and restores the CL_current_values
 * variable to point to its original buffer after the last form has
 * been evaluated.  The contents of the junk buffer are never read by
 * anyone.
 *
 * MULTIPLE-VALUE-CALL begins using the new buffer immediately.  After
 * each argument form, it advances the CL_current_values pointer to
 * the next available element in the buffer, checking that
 * CALL-ARGUMENTS-LIMIT has not been exceeded.  Finally, we call the
 * function with the accumulated data.
 *
 * Examples:
 *
 * The following code saves the value produced by usrFoo() and ignores
 * the values produced by usrBar().  The net effect on the multiple
 * values machinery is that only the values produced by usrFoo() are
 * retained as "the most recent values produced"
 *
 * { clMultipleValueProg1(t_MVP);
 *   usrFoo(clEOA);		// <- values produced here are saved
 *   clSaveValues(t_MVP);
 *   usrBar(clEOA);		// <- values produced here are discarded 
 *   clRestoreValues(t_MVP); }
 * 
 * The following code adds up all the values returned by usrBar(clEOA)
 * and clValues1(I_3). 
 *
 * { clMultipleValueCall(t_MVC);
 *   usrBar(clEOA);
 *   clAccumulateValues(t_MVC);
 *   clValues1(I_3);
 *   clAccumulateValues(t_MVC);
 *   return(clMultipleValueMcall(t_MVC, clPLUSFUNC)); } 
 *
 * Operations:
 *
 * clValues(v0:clObject, v1:clObject, ..., clEOA) => clObject
 *   EFFECTS: Returns the first argument, if any.  If there are no
 *     arguments (i.e. only clEOA is supplied), then clNIL is
 *     returned.  In addition, all of the arguments (including the
 *     first) are retained by the multiple values machinery as "the
 *     most recently produced" multiple values.
 *
 * clValues1(v:clObject) => v
 *   EFFECTS: Equivalent to clValues(v, clEOA), but may be more
 *     efficient. 
 *
 * clMultipleValueProg1(id:label)
 *   REQUIRES: Only declaration statements can precede
 *     clMultipleValueProg1() in the enclosing code block.  Id must be
 *     unique with respect to clMultipleValueProg1 id's of overlapping
 *     scope.
 *   EFFECTS: Marks the start of a region of code which will produce
 *     multiple values.  
 *
 * clSaveValues(id:label)
 *   REQUIRES: id must match that used in a clMultipleValueProg1() in
 *     the same code block.
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Marks the end of a region of code which produces
 *     multiple values.  The values previously produced are saved for
 *     use with clRestoreValues(). 
 *   IMPLEMENTATION EFFECTS: Sets CL_current_values pointer to point
 *     to the junk values buffer established by
 *     clMultipleValueProg1(), so that any new multiple values
 *     produced are written to the junk buffer.
 *
 * clRestoreValues(id:label)
 *   REQUIRES: id must match that used in a clSaveValues() in the same
 *     code block.
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Restores the values saved by the matching
 *     clSaveValues(). 
 *   IMPLEMENTATION EFFECTS: Sets CL_current_values pointer to point
 *     back to its original value before the current
 *     clMultipleValueProg1(). 
 *
 * clMultipleValueCall(id:label)
 *   REQUIRES: Only declaration statements can precede
 *     clMultipleValueCall() in the enclosing code block.  Id must be
 *     unique with respect to clMultipleValueCall id's of overlapping
 *     scope.
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Marks the start of a multiple value call.
 *   IMPLEMENTATION EFFECTS: Same as clSaveValues().
 *
 * clAccumulateValues(id:label)
 *   REQUIRES: id matches the id in a clMultipleValueCall(id) in the
 *     same code block.  The code preceding the clAccumulateValues() must
 *     produce multiple values.
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Accumulates the previously produced multiple values for
 *     use in the current multiple value call.
 *   SIGNALS: Control-error if the total number of values collected
 *     for this multiple-value-call exceeds CALL-ARGUMENTS-LIMIT.
 *   IMPLEMENTATION EFFECTS: Advances CL_current_values to point to
 *     the next unused location in the multiple values buffer
 *     established by clMultipleValueCall().
 *
 * clMultipleValueMcall(id:label, f:clMFunction) => clObject
 *   REQUIRES: id matches the id in a clMultipleValueCall(id) in the
 *     same code block.  Multiple values must have been produced since
 *     this clMultipleValueCall(id), and accumulated with
 *     clAccumulateValues(id).
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Calls the specified C function on the accumulated values,
 *     returning the result.  Multiple values produced by the called
 *     function are considered to be the values produced by the entire
 *     clMultipleValueCall() code block.
 *   IMPLEMENTATION EFFECTS: Similar to clRestoreValues(), but any
 *     values produced prior to the clMultipleValueCall() are replaced
 *     with the ones generated by the function call.
 *
 * clMultipleValueFuncall(id:label) => clObject
 *   REQUIRES: id matches the id in a clMultipleValueCall(id) in the
 *     same code block.  Multiple values must have been produced since
 *     this clMultipleValueCall(id), and accumulated with
 *     clAccumulateValues(id).
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Calls the Lisp function object stored in id on the
 *     accumulated values, returning the result.  Multiple values
 *     produced by the called function are considered to be the values
 *     produced by the entire cl clMultipleValueCall() code block.
 *   IMPLEMENTATION EFFECTS: Similar to clRestoreValues(), but any
 *     values produced prior to the clMultipleValueCall() are replaced
 *     with the ones generated by the function call.
 *
 * Internal Operations:
 *
 * clPrimaryValue(id) => clObject
 *   REQUIRES: The desired values were the most recent ones produced.
 *     id is the identifier of the construct which produced the
 *     values. 
 *   EFFECTS: Retrieves the first multiple value, or clNIL if there
 *     were none. 
 *   IMPLEMENTATION EFFECTS: id is not actually used, but serves as
 *     documentation. 
 *********************************************************************/

#define cl_MULTIPLE_VALUES_LIMIT	20 

#ifndef cl_ECLIPSE_DEFINITIONS 
   extern clMultipleValuesElement *CL_current_values;
#endif

/* Note that in most cases, the C compiler can optimize this into:
   clSetq(CL_current_values[1], clEOA), clSetq(CL_current_values[0], x)
   It is precisely those case in which the C compiler cannot make this
   optimization that we must be sure to define clValues1 as follows: */
#define clValues1(x)			\
  (clSetq(CL_current_values[0], (x)), 	\
   clSetq(CL_current_values[1], clEOA), \
   CL_current_values[0])
   

#ifdef cl_CTYPECHECKS
#  define clPrimaryValue(id)	_clPrimaryValue()
   clObject _clPrimaryValue  __P((void));
#else
#  define clPrimaryValue(id) 	\
 	(_clPresentp(*CL_current_values) ? *CL_current_values : clNIL)
#endif
#define clSavedValue(id)	clPrimaryValue(id)
#define clValuesBuf(id)		clPaste2(id,_buf)
#define clOldValues(id)		clPaste2(id,_old)

#define clValuesHeader(id, n)			\
  clMultipleValuesElement clValuesBuf(id)[n], 	\
    *clOldValues(id) = CL_current_values

#define clMultipleValueProg1(id) \
  clValuesHeader(id, cl_MULTIPLE_VALUES_LIMIT)
#define clSaveValues(id)	(CL_current_values = clValuesBuf(id))
#define clRestoreValues(id)	(CL_current_values = clOldValues(id))

#define clCountVar(id)		clPaste2(id,_count)
#define clMultipleValueCall(id)					\
  volatile unsigned short clCountVar(id) = 0;			\
  clObject id;							\
  clValuesHeader(id, cl_CALL_ARGUMENTS_LIMIT + 			\
		 cl_MULTIPLE_VALUES_LIMIT);			\
  clSaveValues(id)

#define clAccumulateValues(id)					\
  while (_clPresentp(*CL_current_values)) {			\
    CL_current_values++;					\
    clCheckCount(clCountVar(id), cl_CALL_ARGUMENTS_LIMIT,	\
		 "Call-arguments-limit exceeded."); }


#define clMultipleValueFuncall(id)	\
  (clRestoreValues(id), clCallClosure(id)(clArgArray(clValuesBuf(id))))
#define clMultipleValueMcall(id, f)	\
  (clRestoreValues(id), clCallMFunction(f)(clArgArray(clValuesBuf(id))))

/********************************************************************* 
 *********************************************************************
 *	     PART III: EXITS AND THE DYNAMIC ENVIRONMENT
 *	     -------------------------------------------
 *
 * Eclipse code uses the lexical C environment (i.e. the C stack) for
 * representing lexical information (i.e. local variables).  The use
 * of C functions to represent Lisp functions and return Lisp data has
 * been described in PART II, above.
 *
 * There are two other aspects of Lisp programming that require
 * additional support than what is defined by the C language:
 *
 *   - EXITS: Lisp defines constructs which specify that a body of
 *     code should be exited before reaching the end and that multiple
 *     values should be returned to some earlier point in the
 *     execution.  Unlike simply placing a C return statement in the
 *     middle of a function, these constructs allow returning to an
 *     abritrary point within any function which has not yet
 *     terminated -- including the one which contains the transfer.
 *     This is somewhat similar to the behavior of setjmp()/longjmp()
 *     in the C library.
 *
 *   - DYNAMIC ENVIRONMENT: There is some information which obeys a
 *     similar stack discipline during execution to a lexical
 *     environment, but which is not lexically apparent to the
 *     compiler.  One example in C would be the temporary setting of
 *     global variables followed by resetting them back to their
 *     original value when the construct completes.  (The stack-like
 *     behavior becomes apparent with nested temporary settings.)
 *     Lisp provides a number of constructs for doing things to the
 *     dynamic environment and later undoing them.
 *
 * The C setjmp()/longjmp() facility restores the lexical environment,
 * but knows nothing about the dynamic environment.  Our mechanisms
 * for exits must not only restore the lexical environment, but the
 * dynamic environment as well.  For example, an exit out of a
 * construct which temporarilly bound dynamic variables must cause the
 * variables to be reset to values they had at the point we exit back
 * to.
 *
 * Thus the exit and dynamic environment machinery must work together. 
 *********************************************************************/
/********************************************************************* 
 * EXITS
 *
 * An clExitCell represents the state of execution of the machine at a
 * particular point, including:
 *   - The contents of the machine registers and execution stack
 *     (lexical environment).
 *   - The dynamic environment.
 *   - Information about where multiple values, if any, are to be
 *     placed. 
 * A transfer of control or "jump" can be performed based on the
 * information in an clExitCell.  Execution will continue at the point
 * the exit was established, with the original dynamic environment.
 *
 * The construct which establishes an exit must not have terminated
 * when control is transferred back to the exit.
 *
 * Implementation:
 *
 * Since the establishing construct must not have returned, the
 * clExitCell can be stored on the C stack.  In addition, it means
 * that the C library setjmp()/longjmp() pairs can be used for the
 * actual transfer of control.
 *
 * The following utilities are used by other constructs which
 * establish exits or transfer control to them.  The process of
 * actually transferring control is discussed later.
 *
 * We actually use _setjmp()/_longjmp() where available, rather than
 * setjmp()/longjmp().  Even so, the performance varies considerable
 * by system.  _setjmp()/_longjmp() appears to be particularly slow on
 * some SPARCs.
 *
 * Internal operations:
 *
 * clEstablishExit(id:identifier)
 *   REQUIRES: Only declaration statements can precede
 *     clEstablishExit() in the enclosing code block.  Id must be
 *     unique with respect to variables of overlapping scope.
 *   EFFECTS: Sets up id as an clExitCell variable which stores
 *     information about the current machine state.  An
 *     clUnwindExit(&id) will restore this state.  
 *   IMPLEMENTATION-EFFECTS: Caches the CL_current_values pointer in
 *     the exit for later use by clReceivesValues()
 *
 * clFinalizeExit(exit:clExitCell) => int
 *   REQUIRES: exit must be an clExitCell established with
 *     clEstablishExit.  No other clFinalizeExit() may be made to the
 *     same exit.  Any instructions between the clEstablishExit() and
 *     the clFinalizeExit() must have no net effect on the dynamic
 *     environment.
 *   EFFECTS: Ordinarilly returns 0.  However, if a transfer of control
 *     to exit is later made, the effect of that transfer is that the
 *     call to clFinalizeExit() returns a SECOND time, with a non-zero
 *     value. The particular non-zero value is defined by
 *     clDefineExitFlag().  See 'man setjmp'.
 *
 * clDefineExitFlag(exit:clExitCell, val:unsigned short)
 *   REQUIRES: Exit must be an active clExitCell established by
 *     clEstablishExit().  val must nonzero. 
 *   MODIFIES: Exit.
 *   EFFECTS: Defines the value clFinalizeExit(exit) will return if a
 *     transfer of control is made to exit.  This is normally 1.
 *     Currently, only clGo() uses this to return a different value,
 *     in which case the different val's are used to indicate
 *     different labels to go to.
 *
 * clReceivesValues(exit:clExitCell)
 *   REQUIRES: Exit must be an active clExitCell.  Multiple values
 *     must be produced after the clReceivesValues(exit) and before
 *     the transfer of control to the exit.
 *   IMPLEMENTATION MODIFIES: CL_current_values.
 *   EFFECTS: Prepares multiple values machinery so that after the
 *     transfer of control and the establishing clFinalizeExit()
 *     returns its non-zero value, any multiple values produced
 *     between clReceivesValues() and the transfer will be seen by
 *     receiving forms as though they were produced by
 *     clFinalizeExit().  See usage in clReturnFrom().
 *   IMPLEMENTATION EFFECTS: Sets CL_current_values to the value
 *     stored in the exit.
 *********************************************************************/

#define clEstablishSimpleExit(id)		\
  clExitCell id; 				\
  (id).dynamic_environment = clCurrentDEnv();	\
  (id).values = CL_current_values;		\

#define clEstablishExit(id)			\
  clEstablishSimpleExit(id);			\
  clDefineExitFlag((id), 1)

#define clDefineExitFlag(exit, val)	((exit).exit_flag = (val))
#define clFinalizeExit(exit)		_setjmp((exit).machine_state)
#define clReceivesValues(exit)	(CL_current_values = (exit).values)

/*********************************************************************
 * EXITS AS OBJECTS
 *
 * As we have described them so far, clExitCells are neither part of
 * the Lisp lexical environment (clObjects on the C stack) nor part of
 * the dynamic environment (described below).  Some Lisp constructs
 * such as block and tagbody require that we store clExitCells as part
 * of the lexical Lisp environment.  To do this, we need to be able to
 * treat them as clObjects.
 *
 * clExitCells are not clObjects.  Strictly speaking, neither are
 * pointers to clExitCells.  Here we define some internal utilities
 * for manipulating clExitCells as objects, while still obeying strict
 * typing.  These look hairy, but a good C compiler should be able to
 * simplify them almost out of existence.
 *
 * Intern operations:
 *
 * clDeclareExitObject(id:variable_name)
 *   REQUIRES: Only declaration statements can precede
 *     clDeclareExitObject() in the enclosing code block.  Id must be
 *     unique with respect to variables of overlapping scope.
 *   EFFECTS: Causes id to defined as an clObject.  Works with
 *     clInitializeExitObject(). 
 *
 * clInitializeExitObject(id:variable_name, exit_cell:clExitCell)
 *   REQUIRES: id is the same as in a matching clDeclareExitObject(id).
 *     exit_cell is an clExitCell established with
 *     clEstablishExit(exit_cell). 
 *   EFFECTS: Causes the clObject named id to retain represent
 *     information about exit_cell.  Works with clObjectExit().
 * 
 * clObjectExit(obj:clObject) => clExitCell
 *   REQUIRES: obj is an clObject initialized with
 *     clInitializeExitObject().  
 *   EFFECTS: Returns the clExitCell stored in obj. 
 *********************************************************************/

#define clCvtVar(id)		clPaste2(converter_,id)

#define clDeclareExitObject(id)			\
  clObject id; 					\
  union { clObject obj; clExitCell *p; } clCvtVar(id)

#define clInitializeExitObject(id, exit_cell)	\
  clCvtVar(id).p = &(exit_cell); 		\
  clSetq((id), clCvtVar(id).obj);

#define clObjectExit(obj) (clObjectAddress(obj)->exit)

/*******************************************************************
 * BLOCK
 *
 * A set of operations which establish an clExitCell and an clObject
 * variable with a given name that stores information about that exit.
 * clReturnFrom() can be used to transfer to this exit.
 *
 * Example. The following code executes the code starting with the
 * call to usrFoo() and ends with the call to usrBar().  If we make it
 * that far, the variable x is set to the primary value returned by
 * usrBar().  In addition, FOO can be closed over like any clObject
 * variable, as shown in call to clMakeClosure.  If any
 * clReturnFrom(FOO, usrBaz(clEOA)) is executed between usrFoo() and
 * usrBar(), then x is set to the primary value returned by usrBaz(),
 * and we procede with the statement following clBlockEnd().
 *
 * { clBlock(FOO);
 *   if (clTrue((clReturned(FOO))) clSetq(x, (clReturnedValue(FOO)));
 *   else
 *     { usrFoo(clEOA);
 *       ... clMakeClosure(..., ..., ..., &FOO) ...;
 *       clSetq(x, usrBar(clEOA));
 *       clBlockEnd(FOO); } }
 *
 * Operations:
 *
 * clBlock(id:label)       
 *   REQUIRES: Only declaration statements can precede clBlock() in
 *     the enclosing code block.  id must be unique with respect to
 *     clBlocks and clTagbodies of overlapping scope.
 *   EFFECTS: Defines id to be an clObject variable representing
 *     information about the current state of execution.  This
 *     information is used by clReturnFrom(). Label may be closed over
 *     like any clObject variable.  However, id is not tagged, so it
 *     may not be examined by arbitrary Lisp operations.
 *   IMPLEMENTATION EFFECTS: Establishes an clExitCell. See
 *     clEstablishExit(). 
 *
 * clReturned(id:label) => clObject
 *   REQUIRES: id must first be established by a corresponding
 *     clBlock(id).  Any statements appearing between the
 *     clBlock() and the clReturned() must have no net effect on the
 *     dynamic environment.
 *   EFFECTS: Returns false.  However, if a clReturnFrom() is later
 *     executed to this id, the effect is that clReturned() returns a
 *     second time with a value of true.  
 *   IMPLEMENTATION EFFECTS: See clFinalizeExit()
 *
 * clReturnedValue(id:label) => clObject
 *   REQUIRES: clReturned(id) must have returned true.
 *   EFFECTS: Returns the primary value produced by the clReturnFrom()
 *     which caused clReturned() to become true. Note that
 *     clReturnFrom() produces multiple values. 
 *
 * clBlockEnd(id:label)
 *   OVERVIEW: Marks the end of a block for use with clLocalReturn().
 *     It can coexist with clBlock(), but clBlockEnd() does not
 *     require a corresponding clBlock(), and vice versa.
 *   REQUIRES: id must be unique with respect to different clBlocks in
 *     the same function.  However, id should be the same as the id in
 *     a clBlock(id) which corresponds to the same exit.  Note that
 *     the uniqueness requirement is slightly different than that for
 *     clBlock().
 *   EFFECTS: If a clLocalReturn(id) is executed within the same
 *     function, execution continues at the instruction following the
 *     clBlockEnd().  See clLocalReturn().
 *********************************************************************/

#define clBlockLabel(id)	clPaste2(label_,id)
#define clBVar(id)		clPaste2(block_,id)

#define clBlock(id)			\
  clDeclareExitObject(id);		\
  clEstablishExit(clBVar(id)); 		\
  clInitializeExitObject(id, clBVar(id))

#define clReturned(id)		clTest(clFinalizeExit(clBVar(id)))
#define clReturnedValue(id)	clPrimaryValue(id)
#define clBlockEnd(id) 		clBlockLabel(id):

/*********************************************************************
 * TAGBODY
 *
 * A set of operations which establish an clExitCell and a clObject
 * variable with a given name that stores information about that exit. 
 *
 * Example.  The following code executes the code between
 * clTagbodyStart() and clTagbodyEnd().  t_TAGBODY can be closed over
 * like any clObject variable, as shown in call to clMakeClosure.  If
 * clGo(t_TAGBODY, 1) is called between clTagbodyStart() and
 * clTagbodyEnd(), execution proceedes at the statement following the
 * label FOO, using the lexical and dynamic environment at the time
 * the code block was entered.  Similarly, clGo(t_TAGBODY, 2)
 * transfers control to the statement following the label BAR.
 *
 * { clTagbody(t_TAGBODY);
 *   clDefineLabel(FOO, 1);
 *   clDefineLabel(BAR, 2);
 *   clTagbodyStart(t_TAGBODY);
 *   ...;
 *  FOO: ;
 *   ...; ... clMakeClosure(..., ..., ..., &t_TAGBODY) ...;
 *  BAR: ;
 *   ...;
 *   clTagbodyEnd(t_TAGBODY); } 
 *
 * clTagbody(id:label)
 *   REQUIRES: Only declaration statements can precede clTagbody() in
 *     the enclosing code block.  Corresponding clTagbodyStart(),
 *     clTagbodyEnd() and at least one clDefineLabel() statements must
 *     be included in the the same code block.  Label must be unique
 *     with respect to clBlocks and clTagbodies of overlapping scope.
 *   EFFECTS: Defines id to be an clObject variable representing
 *     information about the current state of execution.  This
 *     information is used by clGo().  Label may be closed over like
 *     any clObject variable.  However, id is not tagged, so it may
 *     not be examined by arbitrary Lisp operations.
 *   IMPLEMENTATION EFFECTS: Establishes an clExitCell. See clBlock().
 *
 * clDefineLabel(label, n:unsigned short)
 *   REQUIRES: Preceding statement must be either clTagbody() or
 *     clDefineLabel().  A goto label matching the argument label must
 *     appear between clTagbodyStart() and clTagbodyEnd().  n must be
 *     a compile time constant greater than 0.
 *   EFFECTS: Causes transfer of control to the statement following
 *     the corresponding label when a clGo(id, n) is used, where id is
 *     the clObject established by clTagbody and n is the second
 *     argument to clDefineLabel().
 *
 * clTagbodyStart(id:label)
 *   REQUIRES: Preceding statement must be clDefineLabel().  id must
 *     match that used in clTagbody(). 
 *   EFFECTS: Part of clTagbody() syntax.  Establishes beginning of
 *     tagbody code. 
 *
 * clTagbodyEnd(id:label)
 *   REQUIRES: id just match that used in clTagbody().
 *   EFFECTS: Part of enclose_labels syntax.  Establishes end of
 *     tagbody code. 
 *
 * Implementation:
 *
 * The underlying clExitCell mechanism is identical to that used by
 * clBlock().  The two difference is that instead of a simple two-way
 * if, based on the value returned by clFinalizeExit(), we have a
 * different branch for each label in the tagbody.  clFinalizeExit()
 * cannot return a label name, but it can return a small integer.
 * Therefore, we switch on the integer value returned by
 * clFinalizeExit, with the case 0 being the "normal" tagbody code
 * (including labels), and the other cases being hardcoded branches to
 * the various labels.  The correspondence between labels and integers
 * is done with clDefineLabel().
 *********************************************************************/

#define clTagbody(id)				\
  clDeclareExitObject(id);			\
  clEstablishSimpleExit(clBVar(id)); 		\
  clInitializeExitObject(id, clBVar(id));	\
  switch (clFinalizeExit(clBVar(id))) { 

#define clDefineLabel(label, n)	case n: goto label
#define clTagbodyStart(id)	default:
#define clTagbodyEnd(id)	}
/* clTagbodyStart() is realy 'case 0:' because clFinalizeExit()
   returns 0 the first time (setenv initialization).  However, by
   specifying this as 'default:', we make C compilers and
   linters understand that all cases have been covered. */


#define clLabel(id)	id:


/*********************************************************************
 * THE DYNAMIC ENVIRONMENT
 *
 * Various constructs alter the dynamic environment when they are
 * entered, and restore the environment when they are exited.
 *
 * The act of leaving a dynamic-environment-effecting construct and
 * undoing its effects on the environment is called unwinding.  This
 * happens when we fall out the bottom of a construct which effects
 * the dynamic environment and we wish to restore the environment
 * before continuing with the next instruction.  Unwinding also
 * happens when we transfer control to an clExitCell and we wish to
 * restore the environment to that which was in effect when the exit
 * was established.
 *
 * Information is stored in the dynamic environment in such a way that
 * it can be unwound by a different construct than that which stored
 * the information.
 *
 * For example, Eclipse uses shallow binding for special variables --
 * each symbol has a value field which always hold the current value
 * of the symbol.  A construct which dynamically binds a special
 * variable must store the symbol and its original value in the
 * dynamic environment before assigning new data to the value field.
 *
 * If the only way to leave the binding construct was to fall out the
 * bottom, then the code generated for that construct could simply
 * reset the value at the end.  It would not need to represent the
 * information in a dynamic environment, but could have instead kept
 * track of information in its own lexical environment.
 *
 * Now suppose that during the execution of the binding construct, we
 * call some other function which determines that it needs to unwind
 * the dynamic environment to an earlier state -- one which is before
 * the special variables were bound.  When the called function was
 * defined, the compiler did not know that the function might be
 * called within a construct which bound special variables, so it
 * could not have compiled-in lexical references to information about
 * which symbols need to have which values reset.  The information in
 * the dynamic environment is represented in such a way that the
 * unwinding code can always find the information, and, morever, that
 * it can determine at run-time what information needs to be looked
 * for.
 *
 * The types of information which can be represented in the dynamic
 * environment (and unwound) are:
 *  
 *   - Symbols which have been dynamically bound, and their original
 *     values.
 *
 *   - The fact that an operation (progv) bound a particular number of
 *     symbols in one operation.  The number of symbols is determined
 *     at run-time and thus must be stored dynamically.
 *
 *   - A named clExitCell, called a catch.  One way way of
 *     transferring control to an clExitCell involves locating a catch
 *     with a particular name in the dynamic environment.
 *
 *   - A set of instructions, called a cleanup, which are to be
 *     executed when unwinding to an earlier state.  The cleanup
 *     instructions are executed in the lexical and dynamic
 *     environments that were in effect when the cleanup was
 *     established -- i.e. not the environment of the construct which
 *     initiated the unwinding, and not the environment being unwound
 *     to.  After the instructions have executed, unwinding continues,
 *     possibly executing additional cleanups before reaching the
 *     ultimate unwound dynamic state.
 *********************************************************************/
/*********************************************************************
 * DYNAMIC ENVIRONMENT IMPLEMENTATION
 * 
 * A dynamic environment is only valid during the dynamic execution of
 * the construct which creates it.  This means, for example, that no
 * function can establish a new dynamic environment and return a
 * pointer to that environment.  Once the establishing function
 * terminates, the environment cannot be used.
 *
 * A consequence of this is that the normal C stack can be used, when
 * necessary, to hold new parts of the environment.  The memory is
 * then reclaimed when the code block terminates.
 *
 * Each construct which executes an unwindable operation on the
 * dynamic environment requires that at least one control object be
 * represented in the dynamic environment.  The type of the control
 * object indicates the operation, and the data needed to unwind the
 * operation are stored within the control object.  In some cases that
 * control object (or objects) will already exist.  In other cases,
 * the construct will allocate the object on the C stack as a lexical
 * variable.
 *
 * In any case, the dynamic environment represents a stack of control
 * operations as an array of clControlOpCells, where a clControlOpCell
 * is a pointer to a control object.  CL_dynamic_environment always
 * points to the current dynamic environment, i.e. the highest active
 * clControlOpCell.
 *
 * A clDynamicEnv is an index into the stack of clControlOpCells.  In
 * principle, the stack can grow beyond its initial size, which means
 * it can move.  Thus an index must be used to reference a particular
 * dynamic environment, rather than a machine memory pointer.  (The
 * #define parameter EXPANDABLE_CONTROL_STACK controls whether this is
 * really true.)
 *
 * Operations:
 *
 * clCurrentDEnv() => clDynamicEnv
 *   EFFECTS: Returns a clDynamicEnv representing the current dynamic
 *     environment.  
 *
 * clExtendControls(n:unsigned)
 *   MODIFIES: A new dynamic environment is made current. The stack of
 *     clControlOpCells is reallocated if necesary (depending on
 *     EXPANDABLE_CONTROL_STACK). 
 *   EFFECTS: The new dynamic environment has space for n control
 *     operations. 
 *
 * clUnwind(n:unsigned)
 *   REQUIRES: The current dynamic environment must have at least n
 *     control operations.  None of the elements can be a
 *     clCleanupCell. 
 *   IMPLEMENTATION REQUIRES: clUnwind does NOT restore multiple
 *     values to those defined at the point of the exit.  It may be
 *     necessary to use clRestoreValues() after clUnwind().
 *   MODIFIES: The dynamic environment.
 *   EFFECTS: Unwinds the n most recently added control operations
 *     from the dynamic environment.  As each operation is removed, the
 *     corresponding control object is examined and the appropriate
 *     unwinding side-effect is performed.
 *   IMPLEMENTATION: clUnwind() returns to the place where it was
 *     called.  The implementation of cleanups does not allow
 *     unwinding a cleanup to return. clUnwindExit() must be used
 *     instead.
 *
 * clUnwindExit(clExitCell *) => does not return
 *   REQUIRES: The construct which established the clExitCell must not
 *     have returned. 
 *   IMPLEMENTATION REQUIRES: clUnwindExit does NOT restore multiple
 *     values to those defined at the point of the exit (i.e. it does
 *     not reset the CL_current_values pointer).  An exit out of some
 *     multiple-values effecting construct will leave the
 *     multiple values machinery in an unkown state.  The Eclipse
 *     constructs that use clUnwindExit() all use clReceivesValues().
 *   MODIFIES: The dynamic environment.
 *   EFFECTS:  Unwinds sucessively older conrol operations from the
 *     dynamic environment until the environment matches that which
 *     was in force when the clExitCell was established.  As each
 *     control operation is removed, the control object is examined
 *     and the appropriate unwinding side-effect is performed.  This
 *     includes the execution of cleanups.  Finally, a transfer of
 *     control is made to the argument clExitCell.
 *   IMPLEMENTATION: See clUnwindProtect.
 *
 * clRemoveControlOp()
 *   REQUIRES: The current dynamic environment contains a single
 *     control operation which does not require any action to be
 *     unwound.  The only such examples are a single cleanup which has
 *     already finished executing or a single catch.
 *   MODIFIES: The previous dynamic environment is made current.
 *   EFFECTS: Removes the single most recent control operation from
 *     the dynamic environment without performing any unwinding
 *     action.  This may be more efficient than clUnwind(1).
 *   IMPLEMENTATION: Post-decrements dynamic environment pointer, so
 *     the previous control object is actually returned.  
 ******************************************************************/

typedef enum {
  cl_DBIND_TAG = cl_DATA_TAG,	/* i.e. same first word of a SYMBOL */
  cl_PROGV_TAG,			
  cl_CLEANUP_TAG,		
  cl_CATCH_TAG,
    /* We have to reserve 100 as well as 000 because the low three
       bits of the first word of a Symbol could be 100 on
       cl_WORD_ALIGNED systems. */ 
  cl_DBIND_TAG_WORD_ALGINED = cl_DBIND_TAG + 4,
  cl_ENVIRONMENT_CLEANUP_TAG
} clControlTagCell;

typedef struct {
  clControlTagCell 	tag;
  unsigned short 	count;
} 			clProgvCell;

typedef struct {
  clControlTagCell 	tag;
  jmp_buf 		machine_state;
} 			clCleanupCell;

typedef struct {
  clControlTagCell 	tag;
  clExitCell *		buffer; 
  clObject 		catch_tag;
}  			clCatchCell;

typedef struct {
  clControlTagCell 	tag;
  clBinding **		hook;
  clBinding *		old_env;
}  			clEnvironmentCleanupCell;


typedef union clControlOpCell {
  clObject			symbol_data;
  clObject 			value_data;
  clProgvCell 		*	progv_data;
  clCleanupCell 	*	cleanup_data;
  clCatchCell 		*	catch_data;
  clEnvironmentCleanupCell *	environment_cleanup_data;
}  			clControlOpCell;

#ifndef cl_ECLIPSE_DEFINITIONS
   extern
#endif
clControlOpCell
  *CL_dynamic_environment_base,
  *CL_dynamic_environment_limit, /* one past last available binding */
  *CL_dynamic_environment;	 /* current top of control stack */

/* All the control objects which are ever examined are expected to be
 * data tagged.  In addition to the clProgvCell, clCleanupCell, and
 * clCatchCell, a clSymbolCell might be examined, so it had BETTER be
 * data tagged. 
 */
#define clControlObjectType(control_stack) 	\
  (clBitMask(3) & (unsigned) ((control_stack)->cleanup_data->tag))

/* It might make sense to clear the object pointer ABOVE the the
   previous value of CL_dynamic_environment, in order to release old
   bindings for garbage collection! */
#define clRemoveControlOp() 	CL_dynamic_environment--

#ifdef EXPANDABLE_CONTROL_STACK
   void 	clExtendControls __P((unsigned));
#  define clCurrentDEnv() 			\
 	(CL_dynamic_environment - CL_dynamic_environment_base)
#  define clDEnvPointer(index) (index + CL_dynamic_environment_base)
#else
#  define clExtendControls(n) 	(CL_dynamic_environment += (n))
#  define clCurrentDEnv() 	CL_dynamic_environment
#  define clDEnvPointer(index) 	index
#endif

void clUnwind __P((unsigned)), clUnwindExit __P((clExitCell *));

/*********************************************************************
 * DBIND
 *
 * Enters a single control operation in the dynamic environment which
 * records that a symbol is being bound.
 *
 * Example.  The following code registers the symbol A as a bound
 * variable, and sets its value to the integer 3.  It then calls the
 * function DO-SOMETHING-WITH-A, which presumably makes some use of
 * the symbol-value of the symbol A.  It then calls clUnwind(1) to
 * reset A back to its original value.  Note that if, during the
 * execution of DO-SOMETHING-WITH-A, some form such as clThrow(),
 * clReturnFrom(), or clGo() causes the system to unwind to an earlier
 * point, the value of A will still get rebound.
 *
 * { clDbind(usrA);
 *   clSetq(_clSymbolValue(usrA), I_3);
 *   usrDoSomethingWithA(clEOA);
 *   clUnwind(1); }
 *
 * clDbind(symbol:clObject)
 *   REQUIRES: The dynamic environment must eventually be restored by
 *     some unwinding operation.
 *   EFFECTS: Makes note of the symbol A and its current value in the
 *     dynamic environment so that when unwound, the old value is
 *     restored.  
 *   IMPLEMENTATION: Two clControlOpCells are used.  The earlier one
 *     holds the symbol-value and the later one points to the
 *     clSymbolCell.  Therefore, when an clUnwind() or clUnwindExit()
 *     encounter a clSymbolCell, they set its symbol-value to the
 *     value held in then next clControlOpCell.  Both are popped off
 *     as a single control operation.
 *********************************************************************/
void clDbind __P((clObject));

/*********************************************************************
 * PROGV
 *
 * Dbinds each of the symbols in a list, and gives them new values.  
 *
 * Example.  The following dynamically binds all the symbols in
 * T_symbols to the values specified by T_values.  After clProgvEnd(),
 * the symbols will have original values again.  This is also true, if
 * usrSomeFunction() causes transfer of control to some earlier exit
 * point.
 *
 * { clProgvStart(t_PROGV, T_symbols, T_values);
 *   ...; usrSomeFunction(clEOA); ...;
 *   clProgvEnd(t_PROGV); }
 *
 * clProgvStart(id:label, symbols:clObject, values:clObject)
 *   REQUIRES: id must be unique with respect to any clProgvStart()
 *     with overlapping scope.  symbols must be a list of symbols,
 *     values must be a list.  Only declaration statements can precede
 *     clProgvStart() in the enclosing code block.
 *   EFFECTS: Binds all the symbols in symbols to the corresponding
 *     values specified in values.  If there are too few values, the
 *     corresponding symbols are bound and then made to be unbound.
 *     If there are too many values, the extras are ignored.  The
 *     total number of symbols bound is recorded so that the entire
 *     construct can be considered to have added one control operation
 *     to the dynamic environment.
 *   IMPLEMENTATION: Executes Dbind() for each symbol, and then sets
 *     (or clears) their values.  A clProgvCell is stack allocated to
 *     record the total number of symbols bound.  This is entered into
 *     the newest clControlOpCell.  Threfore, when an clUnwind() or
 *     clUnwindExit() encounters a clProgvCell, they clUnwind() the
 *     number of symbols indicated in the cell before preceding. The
 *     clProgvCell and the clSymbolCells and values are all popped off
 *     as a single control operation.
 *
 * clProgvEnd(id:label)
 *    REQUIRES: id must match that in a clProgvStart(id) in the same
 *     code block.  No net unwinding operations may intervene.
 *    EFFECTS: Unwinds the control operation initiated by the
 *     clProgvStart(). 
 *********************************************************************/

#define clPvar(label)		clPaste2(progv_,label) 
#define clProgvStart(label, symbols, values)	\
 clProgvCell clPvar(label);			\
 clProgvDbind(&clPvar(label), symbols, values)
#define clProgvEnd(label) 	clUnwind(1)
void clProgvDbind __P((clProgvCell *, clObject, clObject));

/*********************************************************************
 * CATCH
 *
 * A set of operations which establish a tagged clExitCell in the
 * dynamic environment. clThrow() can be used to transfer to this
 * exit.
 *
 * Example.  The following code establishes a dynamic exit with the
 * tag clFOO (the symbol FOO).  With the exit established, some code
 * is executed which sets the variable x to some value.  However, if
 * during the evaluation of that code, a clThrow() is executed to
 * clFOO, then the primary value returned by the clThrow() is set to x
 * instead.
 *
 * { clCatch(t_CATCH);
 *   clSetq(t_CATCH, clFOO);
 *   if (clTrue((clThrown(t_CATCH)))) clSetq(x, clThrownValue(t_CATCH));
 *   else
 *     { ...some_code...;
 *       clSetq(x, some_value);
 *       clEndCatch(t_CATCH); } }
 *
 * Note that this looks very much like clBlock().  The differences
 * are: 
 *  1. id must be used to set the tag for the catch.
 *  2. The catch is known in the dynamic environment, not the lexical
 *     environment.  In particular, no clObject variable is created to
 *     represent the exit.
 *  3. clEndCatch() is necessary, not optional, to disestablish the
 *     exit. 
 *
 * Operations:
 *
 * clCatch(id:label)       
 *   REQUIRES: Only declaration statements can precede clCatch() in
 *     the enclosing code block.  id must be unique with respect to
 *     any clCatch with overlapping scope.  A matching clEndCatch() is
 *     required.
 *   EFFECTS:  Prepares to  represent the current state of execution
 *     in the dynamic environment.
 *   IMPLEMENTATION EFFECTS: Establishes an clExitCell. See
 *     clEstablishExit(). 
 *
 * clThrown(id:label) => clObject
 *   REQUIRES: id must first be established by a corresponding
 *     clCatch(id).  Any statements appearing between the clCatch()
 *     and the clThrown() must have no net effect on the dynamic
 *     environment.
 *   EFFECTS: Returns false.  However, if a clThrow() is later
 *     executed to this id before the corresponding clEndCatch() is
 *     reached, the effect is that clThrown() returns a second time
 *     with a value of true. 
 *   IMPLEMENTATION EFFECTS: See clFinalizeExit()
 *   IMPLEMENTATION: A clCatchCell is stack allocated by clCatch().
 *     clThrown() adds the clCatchCell to the dynamic environment.  It
 *     is important that this not be done earlier, because any clThrow
 *     executed during the evaluation of the catch tag should not see
 *     this new catch.  The effect of encountering a clCatchCell
 *     during unwinding is simply to pop it off as a single control
 *     operation.
 *
 * clThrownValue(id:label) => clObject
 *   REQUIRES: clThrown(id) must have returned true.
 *   EFFECTS: Returns the primary value produced by the clThrow()
 *     which caused clThrown() to become true.  Note that clThrown()
 *     produces multiple values.
 *
 * clEndCatch(id:label)
 *   REQUIRES: Must be the last statement in a body of code which only
 *     gets executed when the corresponding clThrown(id) returns
 *     false. 
 *   EFFECTS: Disestablishes the catcher from the dynamic environment.
 *********************************************************************/
#define clCvar(label)		clPaste2(catcher_,label)

#define clCatch(label)				\
  clObject label;				\
  clCatchCell clCvar(label);			\
  clEstablishExit(clBVar(label)); 		\
  clCvar(label).tag = cl_CATCH_TAG;		\
  clCvar(label).buffer = &clBVar(label);

#define clThrown(label)	\
  (clExtendControls(1), clSetq(clCvar(label).catch_tag, label),	\
   CL_dynamic_environment->catch_data = &clCvar(label),		\
   clTest(clFinalizeExit(clBVar(label))))

#define clThrownValue(label)	clPrimaryValue(label)

#define clEndCatch(label)	clRemoveControlOp()

/*********************************************************************
 * CLEANUPS
 *
 * clUnwindProtect() is essentially the same behavior as
 * clMultipleValueProg1(), except the second group of statements is
 * guaranteed to be executed -- even if the first group of statments
 * initiates a transfer of control to an earlier exit point.
 *
 * This is very general in that the the transfer may be initiated from
 * any function executing within the protected code -- i.e. the
 * cleanups are dynamic.  clUnwindProtect constructs may also be
 * nested.  A transfer of control out of all of the nested constructs
 * executes all the cleanup code, in the reverse order they were
 * established.
 *
 * Example.  The following executes usrProtectedFunction() and then
 * usrCleanupCode().  The multiple values produced by
 * usrProtectedFunction() are the ones seen by any surrounding
 * multiple value receiving code.  If usrProtectedFunction() doesn't
 * finish executing because of a clThrow, clReturnFrom, or clGo, the
 * clCleanupCode will still be executed.
 *
 * { clUnwindProtect(t_UNWIND);
 *   usrProtectedFunction(clEOA); 
 *   clCleanupStart(t_UNWIND);
 *   usrCleanupCode(clEOA); 
 *   clCleanupEnd(t_UNWIND); }
 *
 * Operations:
 * 
 * clUnwindProtect(id:label)
 *   REQUIRES: Only declaration statements can precede
 *     clUnwindProtect() in the enclosing code block.  id must be
 *     unique with respect to any clUnwindProtect() with overlapping
 *     scope.  A matching clCleanupEnd() is required.  The code
 *     between the clUnwindProtect() and the corresponding
 *     clCleanupStart() must produce multiple values.
 *   EFFECTS: Marks the started of protected code. Establishes that
 *     the code between the matching clCleanupStart() and
 *     clCleanupEnd() will always be executed after the protected code
 *     -- even if the preceding code initiates a transfer of control
 *     out to an earlier exit point.
 * 
 * clCleanupStart(id:label)
 *   REQUIRES: id must match that in an clUnwindProtect() in the same
 *     code block.
 *   EFFECTS: Marks the end of the protected code and the start of the
 *     cleanup code.
 *
 * clCleanupEnd(id:label)
 *   REQUIRES: id must match that in a a clCleanupStart() in the same
 *     code block.
 *   EFFECTS: Marks the end of the cleanup code.  If the cleanup code
 *     was entered normally, (that is, not from a transfer of control
 *     out of the protected code,) then the multiple values produced
 *     by the protected code will still be valid after the
 *     clCleanupEnd().
 *
 * Implementation:
 *
 * A clCleanupCell is stack allocated by clUnwindProtect() and entered
 * into the dynamic environment.  The clCleanupCell holds a jmp_buf
 * which is initialized with setjmp() before starting the protected
 * code.  The value returned by setjmp() is saved for later.  If 0,
 * (the normal case,) we begin executing the protected code.
 * (Otherwise we jump past it, as described below.)
 *
 * If the protected code terminates normally, we pop the clCleanupCell
 * from the dynamic environment, save any multiple values, and proceed
 * with the cleanup code.  If exection was normal, the setjmp return
 * value we saved earlier will be 0, so we just restore the multiple
 * values and we're done.
 *
 * Suppose clUnwindExit() is executed during the protected code.
 * clUnwindExit() is passed a pointer to an clExitCell, which tells it
 * when to stop unwinding and where to jump to when finished.  If
 * clUnwindExit() should encounter our clCleanupCell, it pops it off
 * and does a longjmp() to the jmp_buf we stored there.  It passes the
 * clExitCell pointer as the second argument to longjmp().  Thus this
 * call to clUnwindExit() never completes.
 *
 * This gets us back the lexical and dynamic environment of the
 * clUnwindProtect().  (The dynamic environment is correct because
 * clUnwindExit() already unwound any other intervening operations.)
 * Now the result of the the clUnwindProtect()'s setjmp() is non-zero
 * (the clExitCell pointer), so we jump past the protected form and go
 * to the cleanup code.
 *
 * At this point, the cleanup forms execute normally, including the
 * saving and restoring of multiple values.  When we get to the end,
 * we recognize that the saved setjmp() value is non-zero and we
 * execute a NEW clUnwindExit() with this pointer as argument.
 ******************************************************************/

#define clCleanupValues(label)	clPaste2(cleanup_values,label)
#define clCleanupLabel(label)	clPaste2(cleanup_,label)
#define clUvar(label)		clPaste2(unwind_,label)
#define clUnwindingp(label) 	clPaste2(exit_,label)

#define clUnwindProtect(label)				\
  clMultipleValueProg1(clCleanupValues(label));		\
  clCleanupCell clUvar(label);				\
  int clUnwindingp(label);				\
  clUvar(label).tag = cl_CLEANUP_TAG;			\
  clExtendControls(1); 					\
  CL_dynamic_environment->cleanup_data = &clUvar(label);\
  if ((clUnwindingp(label) = _setjmp(clUvar(label).machine_state)) != 0) \
    goto clCleanupLabel(label)

#define clCleanupStart(label) 			\
  clRemoveControlOp(); clCleanupLabel(label): 	\
  clSaveValues(clCleanupValues(label))

#define clCleanupEnd(label)			\
  clRestoreValues(clCleanupValues(label));	\
  if (clUnwindingp(label)) 			\
    clUnwindExit_int(clUnwindingp(label))

#ifdef LINT
   void clUnwindExit_int __P((int));
#else
#  define clUnwindExit_int(i) clUnwindExit((clExitCell *) i)
#endif

/*********************************************************************
 * TRANSFER OF CONTROL TO EXITS
 *********************************************************************/

/*********************************************************************
 * GO
 *
 * Performs a transfer of control to a label within a tagbody.  The
 * transfer uses an exit established by the tagbody.  This is more
 * than just a "computed goto", because the dynamic environment is
 * unwound back to its state at the establishment of the exit.
 *
 * Examples: clGo(t_TAGBODY, 2); 
 *           clGo(clEnv(3, x), 2);
 *
 * clGo(id:clObject, n:unsigned short) => does not return
 *   REQUIRES:  id must refer to the same clObject defined by id in
 *     clTagbody(id).  n must evaluate to one of the integers used in
 *     a clDefineLabel() in that clTagbody().  The expression for id
 *     must not produce side-effects.
 *   EFFECTS: Transfers control to the label defined with n in the
 *     corresponding clTagbody().  After the transfer, the lexical and
 *     dynamic environments will be the ones in force at the
 *     establishment of the clTagbody().
 *   IMPLEMENTATION EFFECTS: Calls clReceivesValues() before
 *     unwinding.  Even though clGo() does not actually produce
 *     values, we have no way of knowing if we are exiting out of an
 *     execution path which modified the multiple values machinery
 *     (ex. a multiple-value-call or the value expression of a throw).
 *     If the tagbody was executing as part of the calculations of an
 *     argument for a multiple-value-call, we would loose the argument
 *     values accumulate thus far.  clReceivesValues() makes sure that
 *     doesn't occur by resetting the multiple values machinery back
 *     to the state it had at the entrance to the tagbody.
 *********************************************************************/

#define clGo(obj, key)	\
  (clDefineExitFlag(clObjectExit(obj), key),	\
   clReceivesValues(clObjectExit(obj)),		\
   clUnwindExit(&(clObjectExit(obj))))


/*********************************************************************
 * RETURN-FROM
 *
 * Performs a transfer of control to an exit defined in the lexical
 * environment.  The exit is established by clBlock().  Multiple
 * values are returned to the block.  As with all transfers to exits,
 * the dynamic environment is unwound back to its state at the
 * establishment of the exit.
 *
 * Examples.  This code returns the single value I_9 (the fixnum 9) from
 * the end of the clBlock established with the name FOO.
 *
 *   clReturnFrom(FOO, clValues1(I_9));
 *
 * This does the same, where FOO was captured as the third binding in
 * the captured lexical environment:
 *
 *   clReturnFrom(clEnv(2, x), clValues1(I_9));
 *
 * When it is desired to simply jump to the end of a block, it may be
 * more efficient to use clLocalReturn().  Strictly speaking, this is
 * not a transfer to an exit because no unwinding of the dynamic
 * environment is performed.
 *
 * Example: clLocalReturn(FOO);
 *
 * Operations:
 *
 * clReturnFrom(id:clObject, val) => does not return
 *   REQUIRES: id must refer to the same clObject defined by id in
 *     clBlock(id).  val must be an expression which produces multiple
 *     values.  The expression for id must not produce side-effects.
 *   EFFECTS: Evaluates val and transfers control to the matching
 *     clBlock.   After the transfer, any multiple values produced by
 *     val will be visible to receiving forms as though they had been
 *     produced by clReturnedValue().
 *
 * clLocalReturn(id:label) => does not return
 *   REQUIRES: id must match the id in an clBlockEnd in the same code
 *     block.
 *   IMPLEMENTATION REQUIRES: If block being returned to is producing
 *     multiple values, then clLocalReturn() must not be executed from
 *     within a multiple values effecting construct, such as a
 *     multiple-value-call or a throw.
 *   EFFECTS: Transfers control to the statement following the
 *     matching clBlockEnd(). Note that no values are returned and no
 *     unwinding of the dynamic environment is performed. 
 *   USAGE: To return-from a block within the current function, and
 *     between which their are no intervening unwind-protects, it may
 *     be more efficient to use clLocalReturn() then clReturnFrom().
 *     However, it us up to the user to assign result values and
 *     clUnwind() as necessary.  Note that a clLocalReturn() requires
 *     a matching clBlockEnd(), but it does not require a matching
 *     clBlock().
 *   RATIONALE: Certainly, one could just define an end-block label
 *     and use a goto.  However:
 *      1. It is possible for a block to have both clReturnFrom() and
 *         clLocalReturn().  If there is a clReturnFrom(), there must
 *         be a clBlock(), and clBlock() uses the block name for an
 *         clObject variable.  Thus we can't use the block name
 *         directly for the label.  clBlockEnd(name) provides this
 *         abstraction.
 *      2. The current implementation of clBlock() does not require an
 *         clBlockEnd(), but clCatch() does require an clEndCatch().
 *         If the implementation were to change, it would be nice to
 *         have clBlockEnd() already defined.
 *      3. The usual C operation of goto label is a little general.
 *         It doesn't imply anything about why the transfer is made.
 *         The names of the pair clBlockEnd(FOO)/clLocalReturn(FOO)
 *         make the meaning clearer.
 *********************************************************************/

#define clReturnFrom(obj, val)			\
  (clReceivesValues(clObjectExit(obj)), val, 	\
   clUnwindExit(&(clObjectExit(obj))))

#define clLocalReturn(label) 	goto clBlockLabel(label)

/*********************************************************************
 * THROW
 *
 * Performs a transfer of control to a named exit found in the dynamic
 * environment.  The exit is established by clCatch().  Multiple
 * values produced by the throw are seen by the catch.  As with all
 * transfers to exits, the dynamic environment is unwound back to its
 * state at the establishment of the exit.
 *
 * Example.  This code throws the single value I_9 (the fixnum 9) to
 * the clCatch established with the tag clFOO (the symbol FOO).
 *
 * { clThrow(t_THROW);
 *   clSetq(t_THROW, clFOO);      
 *   clFindCatcher(t_THROW);
 *   clExitThrow(t_THROW, clValues1(I_9)); }
 *
 * Operations:
 *
 * clThrow(id:label)
 *   REQUIRES: Only declaration statements can precede clThrow() in 
 *     the enclosing code block.  id must be unique with respect to
 *     all clThrows of overlapping scope.
 *   EFFECTS: Part of the syntax of a throw.
 * 
 * clFindCatcher(id:label)
 *   REQUIRES: The id of the same id must have already
 *     been set with the desired catch tag.  The id must
 *     be in the same code block.
 *   SIGNALS: control-error if no matching catch is found.
 *   EFFECTS: Locates the most recent catch with the specified tag in
 *     the dynamic environment.  
 *   IMPLEMENTATION EFFECTS: Calls clReceiveValues() just before
 *     returning. 
 *
 * clExitThrow(id:label, val) => does not return
 *   REQUIRES: id must match the id of a previous clFindCatcher(id) in
 *     the same code block.  val must be an expression which produces
 *     multiple values.
 *   EFFECTS: Evaluates val and transfers control to the matching
 *     clCatch.   After the transfer, any multiple values produced by
 *     val will be visible to receiving forms as though they had been
 *     produced by clThrownValue().
 *
 * Implementation note:
 *
 * One might expect a simpler process.  Something expressible by:
 *
 * #define Throw(tag, val) 			\
 * { clThrow(x); clSetq(x, tag); 	\
 *   clFindCatcher(x); clExitThrow(x, val); }
 *
 * Instead, we break the parts up so that the assignment of the
 * id and the actual clExitThrow() may be involved in
 * complex statements.  These complex statements might involve nested
 * code blocks, if statements, other clCatch's, clBlock's, etc.
 *
 * Note that our implementation of multiple values would not allow
 * something like this:
 *
 * { clObject throw_tag, throw_val;
 *   clSetq(throw_tag, clFOO);
 *   clSetq(throw_val, clValues(I_9));
 *   Throw(throw_tag, throw_val); }
 *
 * The problem is that the multiple values would have to be copied by
 * Throw() to the place expected by the clCatch().  We avoid this
 * inefficient copying by making clFindCatcher() come before the
 * generation of the multiple values and having clFindCatcher() work
 * with the multiple values machinery so that the values are written
 * to the correct place to begin with.
 *
 * Note that clReturnFrom() and clGo() expand into code like:
 *   (clReceivesValues(...), val, clUnwindExit(...))
 * while clExitThrow() skips the clReceivesValues().  The reason is
 * that for throw,, we don't know at compile time where the pointer to
 * the multiple values buffer is.  clFindCatch() locates this and sets
 * us up to receive multiple values.
 *********************************************************************/

#define clJVar(label)		clPaste2(jump_,label)

#define clThrow(label)	\
  clObject label; clExitCell *clJVar(label)

clExitCell *_clFindCatcher __P((clObject));
#define clFindCatcher(label)				\
  (clJVar(label) = _clFindCatcher(label))

#define clExitThrow(label, val)	(val, clUnwindExit(clJVar(label)))


/*********************************************************************
 * UNWIND-RETURN
 *
 * Returns multiple values from the current function, unwinding a
 * fixed number of control operations from the dynamic environment.
 *
 * clUnwindReturn(val, n:unsigned) => does not return
 *   REQUIRES: val is an expression which produces multiple values.
 *     The current function must have added exactly n control
 *     operations to the dynamic environment which have not yet been
 *     unwound.  None of the control operations may be cleanups.
 *   EFFECTS: val is evaluated in the current lexical and dynamic
 *     environment (i.e. before unwinding).  Then n entries are
 *     unwound from the dynamic environment.  Finally, the current
 *     function returns the primary value produced by val.
 *   NOTE: clUnwindProtect is designed in such a way that cleanups are
 *     unwound in the normal course of execution.
 *********************************************************************/

#define clUnwindReturn(exp, n) \
  return(exp, clUnwind(n), clPrimaryValue(last))

/*********************************************************************
 * UNREACHED RETURN
 *
 * Sometimes a clReturnFrom(), clGo(), or clExitThrow() occurs as the
 * last statement in a Lisp function (or as the last statement in some
 * branch of the function).  Some linters/compilers will complain
 * about the function not having a valid return statement.  In such
 * cases, we use a dummy return.
 *
 * clUnreachedReturn => does not return
 *   REQUIRES: preceding statement is a clReturnFrom(), clGo, or
 *     clExitThrow(). 
 *   EFFECTS: Makes the linter/compiler think that we are returning a 
 *     clObject.  In fact, the statement is never executed because the
 *     previous statement does not return. 
 *********************************************************************/

#define clUnreachedReturn	return(clNIL)


/*********************************************************************
 *********************************************************************
 * OTHER EXTERNAL DEFINITIONS
 *********************************************************************/

#ifndef cl_ECLIPSE_STATIC_DEFINITIONS
   extern clObject clNIL, clT, clstarPACKAGEstar, clstarREADTABLEstar,
    clstarLOAD_TRUENAMEstar, clstarLOAD_PATHNAMEstar, clSETF_EXPANDER;
#endif
clObject clSymbolValue(clProto), clFdefinition(clProto),
    clFuncallFunction(clProto);

#ifdef clDEBUG_INIT		/* See bottom of interface.c */
#  if __STDC__
#    define Describe(thing) describe(#thing, thing, 0)
#  else
#    define Describe(thing) describe("thing", thing, 0)
#  endif
   int describe __P((const char *, clObject, int));
#endif

#endif				/* cl_ECLIPSE */
