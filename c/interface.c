/*********************************************************************
  This data and information is proprietary to, and a valuable trade
  secret of, Elwood Corporation.  It is given in confidence by Elwood,
  and may only be used as permitted under the license agreement under
  which it has been distributed and in no other way.  No disclosure of
  this information shall be made to any person or organization without
  the prior consent of Elwood Corporation. 

  (c) Copyright 1995, 1997, 1998 by Elwood Corp.  All rights reserved.

  Written by HRS.
 *********************************************************************/

/* This file defines the interace to the operating system.  
   It is subject to change. */

#include <sys/types.h>	/* (open stat lseek mkdir) */
#include <sys/stat.h>		/* stat mkdir open */
#ifdef unix
#  include <sys/time.h>		/* select gettimeofday timezone */
#  include <sys/utsname.h>	/* uname */
#  include <sys/resource.h>	/* getrlimit getrusage */
#  include <sys/syscall.h>	/* getdents  */
#endif

#include <stdio.h>		/* rename remove cuserid */
#include <stdlib.h>		/* exit rand getenv mbtowc wctomb */
#include <ctype.h>		/* isprint isalpha isalnum isupper islower toupper tolower */ 
#include <math.h>		/* fabs ldexp cel floor (trig functions) */
#include <string.h>		/* memcpy strlen memmove */
#include <time.h>		/* time localtime */ 
#include <signal.h>		/* signal */ 
#include <errno.h>		/* errno strerrno */
#include <locale.h>		/* setlocale */
#include <fcntl.h>		/* open */ 

#ifdef unix
#  include <termios.h>		/* tcdrain tcflush */
#  include <unistd.h>		/* getcwd lseek tcdrain tcflush sleep rmdir */
#  include <dirent.h>		/* opendir closedir readdir (or MAXNAMLEN) */
#  include <pwd.h>			/* getpwuid */ 
#elif (defined _WIN32)
#  include <io.h>				/* read write etc. */
#  include <direct.h>           /* getcwd mkdir rmdir */
#  include <winsock2.h>			/* select */
#  include <windows.h>
#  define lstat(x, y)		stat(x, y)
#endif

#if (defined unix) && !(defined linux)
#  include <poll.h>		/* poll */
#endif

#if (defined hpux) || (defined linux)
#  define USE_POSIX_TIMES 1
#  include <sys/times.h>		/* times */
#elif (defined _WIN32)
#  include <sys/timeb.h>		/* ftime */
#endif

/* No systems currently define USE_POSIX_OPENDIR */
#if !(defined USE_POSIX_OPENDIR)
#  if (defined linux)
/* #    include <linux/dirent.h>	/* getdents */
#define dirent linux_dirent
          struct linux_dirent {
               unsigned long  d_ino;     /* Inode number */
               unsigned long  d_off;     /* Offset to next linux_dirent */
               unsigned short d_reclen;  /* Length of this linux_dirent */
               char           d_name[];  /* Filename (null-terminated) */
                                 /* length is actually (d_reclen - 2 -
                                    offsetof(struct linux_dirent, d_name)) */
               /*
               char           pad;       // Zero padding byte
               char           d_type;    // File type (only since Linux
                                         // 2.6.4); offset is (d_reclen - 1)
               */

	  };
#    include <linux/unistd.h>	/* getdents */
     extern int getdents __P((unsigned int, struct dirent *, unsigned int));
#  endif
#endif

#if (defined Solaris)
#  include <siginfo.h>			/* FPE codes (other systems from signal.h)*/		
#elif (defined _WIN32)
#  include <float.h>			/* FPE codes, copysign */
#  define copysign(x, y)	_copysign(x, y)
#endif


#ifdef clNOGC
#  include <malloc.h>
#else
#  include "gc.h"
#  if (defined sun) || (defined linux) || (defined MSWIN32) || \
      (defined IRIX5) || (defined __alpha) || (defined PCR)
#    define INCREMENTAL_GC
#  endif  /* incremental gc machines */
#endif

#define cl_ECLIPSE_DEFINITIONS	1
#include "eclipse.h"
#include "statics.h"

/* Missing from header files */
#if !(defined __STDC__) && !(defined linux)
  extern time_t time();
  extern long gethostid();
  extern caddr_t sbrk();
#endif

/*******************************************************************
 * Lisp functions used here and defined in Lisp source. 
 *******************************************************************/

extern clObject clAddPkg(clProto), clAddXint(clProto),
  clCopyReadtable(clProto), clDoubleFloatXint(clProto),
  clGet(clProto), clGetf(clProto), clIntern(clProto),
  clClassWrappers(clProto), clCmpXint(clProto), clEndp(clProto),
  clEql(clProto), clEqlHash(clProto), clEqualpHash(clProto),
  clError(clProto), clExtraArgs(clProto), clFindType(clProto),
  clFuncall(clProto), clFunctionp(clProto), clIdString(clProto),
  clLength(clProto), clLengthList(clProto), clList(clProto),
  clListSTAR(clProto), clMakeCondition(clProto),
  clMakePackage(clProto), clMissingArgs(clProto), clMultXint(clProto),
  clOsNamestring(clProto), clPackageName(clProto),
  clPackageNicknames(clProto), clParseInteger(clProto),
  clPutf(clProto), clResizeHashTable(clProto),
  clSetInterrupt(clProto), clSimpleStringP(clProto),
  clSubtXint(clProto), clUpdateInstancesIfNeeded(clProto),
  clXintDoubleFloat(clProto), clXintInteger(clProto);

/*******************************************************************
 * Functions used before definition in this file.
 *******************************************************************/
char *clBaseStringCharp1 __P((clObject, int));
clObject clCharpSimpleBaseString __P((const char *)),
  clCons(clProto), 
  clDoubleDoubleFloat __P((double)),
  clIntegerXint __P((clObject)),
  clEmfTableSethash __P((clObject, clObject, clObject)),
  clIntInteger __P((int)),
  clMakeKeyword(clProto),
  _clStorageCondition __P((void)); 


/*******************************************************************
 * Additional prototypes.  These prototypes are not strictly needed
 * for K&R or ANSI compilers, but are required for C++.  In addition,
 * they allow this file to lint better.  
 * 
 * There are additional not-strictly needed prototypes scattered
 * through the file: in function-defining macros and before static
 * functions that are temporary or conditionally compiled.
 *******************************************************************/
static void UnrecognizedControlTag __P((void));
static int StringData __P((clObject, char **, wchar_t **, int));
static long sys_time __P((long));
static clObject
 clMakeCommonArray __P((m_size, clObject, clObject, clIndex, clObject, clObject)),
 clUpdateCommonArray __P((clObject, clIndex, clObject, clObject));

void clInit __P((unsigned)), clClearEmfTable __P((clObject)), 
  clEmfTableRehash __P((clObject)), clMillisleep __P((int));

unsigned StringLength __P((clObject));

int tcpoll __P((int)),
  _clSymbolp __P((clObject)),
  _clListp __P((clObject)),
  _clStringp __P((clObject)),
  clFileDescriptorLength __P((clObject)),
  clFileDescriptorPosition __P((clObject)),
  clFileDescriptorSeek __P((clObject, int)),
  clEmfGetHash __P((clObject, unsigned)),
  clEmfGetHasher __P((clObject, unsigned)),
  _clStringEq __P((clObject, clObject)),
  _clEmfSetTest __P((clObject, clObject, int)),
  clEmfGetTest __P((clObject, clObject, unsigned));

char *clFileOwner __P((clObject)), *clFollowLink __P((clObject)),
  *clReadDir __P((clObject, clObject));
  
clIndex clIntegerIndex __P((clObject)); 

clObject _clTypeError __P((clObject, const char *)),
  _clSystemError __P((const char *));

clObject clGcData __P((void)), clUnixTime __P((void)),
  clMakeEmfTable __P((void)), 
  clFpeClass __P((int)),
  clUname __P((int)),
  clFloatSingleFloat __P((float)), clDoubleSingleFloat __P((double)),
  clMakeBaseCharVector  __P((clIndex)),
  clMakeExtendedCharVector  __P((clIndex)),
  clMakeGeneralVector  __P((clIndex)),
  clMakeBitVector  __P((clIndex)),
  clReadOpen __P((clObject)), clWriteOpen __P((clObject)),
  clReadWriteOpen __P((clObject)), clFileModified __P((clObject)),
  clMakeDir __P((clObject)),
  clFileDelete __P((clObject)), clDirDelete __P((clObject)),
  clOpenDir __P((clObject)), clFileType __P((clObject)),
  clGetTimezone __P((clObject)),
  clFileDescriptorClose __P((clObject)), clInteractivep __P((clObject)),
  clDrain __P((clObject)), clPoll __P((clObject)),
  clFlushInput __P((clObject)), clFlushOutput __P((clObject)),
  clCloseDir __P((clObject)), clStringHash __P((clObject)),
  clOpenAddressClrhash __P((clObject)), 
  clLogcountFixnum __P((clObject)), clLengthFixnum __P((clObject)),
  clDigitHigh __P((clObject)), clDigitLow __P((clObject)),
  clTruncateSingleFloat  __P((clObject)),
  clTruncateDoubleFloat  __P((clObject)),
  clMakeXint __P((clObject)), 
  clMakeSlots __P((clObject)), clMakeWrapper __P((clObject)),
  _clEmfSetHasher __P((clObject, int)), 
  clNloadXint __P((clObject, int)), 
  clFileRename __P((clObject, clObject)),
  clOpenAddressMaphash __P((clObject, clObject)),
  clFirstNonEmptyIndex __P((clObject, clObject)), 
  clHashTableKey __P((clObject, clObject)),
  clHashTableValue __P((clObject, clObject)), 
  clClassWrapper __P((clObject, clObject)),
  clAshFixnum  __P((clObject, clObject)),
  clTrimXint  __P((clObject, clObject)),
  clAddIntegerInteger  __P((clObject, clObject)),
  clSubtIntegerInteger  __P((clObject, clObject)),
  clMultIntegerInteger  __P((clObject, clObject)),
  clFloatSign2SingleFloatSingleFloat __P((clObject, clObject)),
  clFloatSign2DoubleFloatSingleFloat __P((clObject, clObject)),
  clFloatSign2SingleFloatDoubleFloat __P((clObject, clObject)),
  clFloatSign2DoubleFloatDoubleFloat __P((clObject, clObject)),
  clScaleFloatSingleFloatInteger __P((clObject, clObject)),
  clScaleFloatDoubleFloatInteger __P((clObject, clObject)),
  clSetSymbolPlist  __P((clObject, clObject)),
  clFastFind  __P((clObject, clObject)),
  clFastFindSlot  __P((clObject, clObject)),
  clMakeRatio  __P((clObject, clObject)),
  clMakeComplex  __P((clObject, clObject)),
  clSetFuncallableStandardInstanceFunction __P((clObject, clObject)),
  clStringEq __P((clObject, clObject, clObject)),
  clEmfTableGet __P((clObject, clObject, clObject)),
  clMakeStandardInstanceFromSlots __P((clObject, clObject, clObject)),
  clMakeFuncallableStandardInstanceFromSlots __P((clObject, clObject, clObject)),
  clMakeStandardInstance __P((clObject, clObject, clObject)),
  clMakeFuncallableStandardInstance __P((clObject, clObject, clObject)),
  _clMakeStandardInstanceFromSlots __P((clObject, clObject, clObject, m_size)),
  clEmfTableSethash1 __P((clObject, clObject, clObject, int)), 
  clMakeOpenAddressHashTable __P((clObject, clIndex, clObject, clIndex)), 
  clEmfTableSet __P((clObject, clObject, clObject, clObject)), 
  clReadAscii __P((clObject, clObject, clObject, clObject)),
  clWriteAscii __P((clObject, clObject, clObject, clObject)),
  clReadUcs2 __P((clObject, clObject, clObject, clObject)),
  clWriteUcs2 __P((clObject, clObject, clObject, clObject)),
  clReadUcs4 __P((clObject, clObject, clObject, clObject)),
  clWriteUcs4 __P((clObject, clObject, clObject, clObject)),
  clReadMb __P((clObject, clObject, clObject, clObject, clObject)),
  clWriteMb __P((clObject, clObject, clObject, clObject)),
  clMakeSimpleArray __P((clIndex, clObject, clObject)),
  clMakeComplexArray __P((clObject, clIndex, clObject, clObject, clObject, clObject)),
  clUpdateArray __P((clObject, clObject, clIndex, clObject, clObject, clObject)),
  clDecodeDoubleFloat(clProto), clDecodeSingleFloat(clProto);


/*******************************************************************
 * Lisp objects defined elsewhere 
 *******************************************************************/

extern clObject clstarINTERRUPTSstar, clstarKEYWORD_PACKAGEstar,
  clstarSTANDARD_READTABLEstar, clADD, clAPPLY, clAPPLY_FUNCTION,
  clARITHMETIC_ERROR, clBUS_ERROR_HANDLER, clCAR, clCDR, clCONS,
  clCONTROL_ERROR, clDIV, clDIVISION_BY_ZERO, clDOUBLE_FLOAT, clEQ,
  clEQL, clEQL_HASH, clEQUAL, clEQUAL_HASH, clEQUALP, clEQUALP_HASH,
  clFILE_ERROR, clFIXNUMP, clFLOATING_POINT_INEXACT,
  clFLOATING_POINT_INVALID_OPERATION, clFLOATING_POINT_OVERFLOW,
  clFLOATING_POINT_UNDERFLOW, clFORMAT, clFPE_HANDLER, clFUNCALL,
  clFUNCALL_FUNCTION, clGETCWD, clGETENV,
  clINTERNAL_TIME_UNITS_PER_SECOND, clILLEGAL_INSTRUCTION_HANDLER,
  clINTERRUPT_HANDLER, clLISTstar, clMACHINE_ID, clMAKE_KEYWORD,
  clMAKE_SYMBOL, clMULT, clNOT_FOUND, clNREVERSE, clPACKAGE,
  clREAL_TIME, clRUN_TIME, clRPLACA, clRPLACD, clSEGV_HANDLER,
  clSET_SYMBOL_PLIST, clSET_SYMBOL_VALUE_VALUE,
  clSET_SYMBOL_FUNCTION_VALUE, clSET_SYMBOL_SETF_FUNCTION_VALUE,
  clSINGLE_FLOAT, clSTORAGE_CONDITION, clSUBT, clSYMBOL_NAME,
  clSYMBOL_PLIST, clSYS_ERROR_HANDLER, clSYSTEM_ERROR,
  clSYSTEM_PROPERTY, clTYPE_ERROR, clTYPEP, clVALUES;


extern clObject clstarENTRIESstar; /* testing only */

clClassDef(clBUILT_IN_FUNCTION); clClassDef(clCHARACTER);
  clClassDef(clCOMPLEX); clClassDef(clCOMPLEX_ARRAY);
  clClassDef(clCOMPLEX_BIT_VECTOR); clClassDef(clCOMPLEX_BASE_STRING);
  clClassDef(clCOMPLEX_EXTENDED_STRING); clClassDef(clCOMPLEX_VECTOR);
  clClassDef(clCONS); clClassDef(clDOUBLE_FLOAT);
  clClassDef(clINTEGER); clClassDef(clNULL);
  clClassDef(clOPEN_ADDRESS_HASH_TABLE);
  clClassDef(clOPEN_ADDRESS_EQUAL_HASH_TABLE); clClassDef(clRATIO);
  clClassDef(clSIMPLE_ARRAY); clClassDef(clSIMPLE_BASE_STRING);
  clClassDef(clSIMPLE_BIT_VECTOR);
  clClassDef(clSIMPLE_EXTENDED_STRING); clClassDef(clSIMPLE_VECTOR);
  clClassDef(clSINGLE_FLOAT); clClassDef(clSYMBOL); clClassDef(clT);
  clClassDef(clUNKNOWN_OBJECT);


/******************************************************************* *
Lisp symbols defined here.
*******************************************************************/
clObject keyDATUM, keyDESCRIPTION, keyDIRECTORY, keyECLIPSE, keyERRNO,
    keyEXPECTED_TYPE, keyFORMAT_ARGUMENTS, keyFORMAT_CONTROL,
    keyFUNCTION, keyJUNK_ALLOWED, keyPATHNAME, keyLINK, keyNICKNAMES,
    keyRADIX;  

static clObject PKG_KEYWORD, KEY_TABLE;


/*********************************************************************
 *********************************************************************
 * KERNEL
 *********************************************************************/

#ifdef LINT
   char *clGetCharpArg();
   int clGetIntArg();
   clMFunction clGetMFunctionArg();
   clBinding **clGetHookAddressArg();
   clBinding clGetBindingArg();
   void clIgnore();
#else
#  define clGetCharpArg(ap)		va_arg(ap, char *)
#  define clGetIntArg(ap)		va_arg(ap, int)
#  define clGetMFunctionArg(ap)		va_arg(ap, clMFunction)
#  define clGetHookAddressArg(ap)	va_arg(ap, clBinding **)
#  define clGetBindingArg(ap)		va_arg(ap, clBinding)
#  define clIgnore(thing) (void) thing
#endif

#if (defined sysV68)		/* byte or word-aligned systems */
/* We need double word (8-byte alignment, but this isn't available 
 * through malloc on some low-performance systems.  This version makes
 * sure that all heap clObjects are double word alligned. 
 */
/* N.B. On sysV68, one must set the environment variable DBLALIGN=YES
 * before compiling. (Motorola thinks a word is 2-bytes, so
 * DBLALIGN=YES really gets us single word alignment.  DBLALIGN=NO
 * gives only short (2 byte!) alignment.)  
 */
#   define clAllign(x)  (8 * clCeil((m_size) x, 8))
#   define clMalloc(size)	clAllign(malloc((size)+sizeof(clWord)))
#elif (defined clNOGC)
#   define clMalloc(size)	malloc(size)
#   define clCMalloc(size)	calloc(size/sizeof(clObject *), sizeof(clObject *))
#   define clRMalloc(obj, size)	realloc(obj, size)
#else
#   define clMalloc(size)	GC_malloc(size)
#   define clCMalloc(size)	GC_malloc(size)
#   define clRMalloc(obj, size)	GC_realloc(obj, size)
#endif

#ifdef cl_CTYPECHECKS
#  ifdef LINT
     clObjectCell *clMallocObjectCell(), *clCMallocObjectCell();
#  else
#    define clMallocObjectCell(size) (clObjectCell *)clMalloc(size)
#    define clCMallocObjectCell(size) (clObjectCell *)clCMalloc(size)
#  endif

int clTrue(x) clObject x; { return(_clTrue(x)); }

clObject clMakeHeapObject(size) m_size size; {
  clObject x;
  if (!(clObjectAddress(x) = clMallocObjectCell(size)))
    _clStorageCondition();
  return(x);
}

clObject clMakeClearHeapObject(size) m_size size; {
  clObject x;
  if (!(clObjectAddress(x) = clCMallocObjectCell(size)))
    _clStorageCondition();
  return(x);
}

clPointerTagCell _clObjectPointerTag(obj) clObject obj; {
  register union { int i; clPointerTagCell t; } tag;
  tag.i = clObjectWord(obj) & cl_TAG_MASK;
  return(tag.t);
}

void _clSetPointerTag(tag, obj) clPointerTagCell tag; clObject *obj; {
  register union { int i; clPointerTagCell t; } cvtr;
  cvtr.t = tag;
  clObjectWord(*obj) |= cvtr.i;
}

clObjectCell *_clPointerTagObjectCell(obj, tag) 
     clObject obj; clPointerTagCell tag;
{
  register union { clPointerTagCell tag; int i; } cvtr;
  cvtr.tag = tag;
  clObjectWord(obj) -= cvtr.i;
  return(clObjectAddress(obj));
}

clObject _clSetq(obj_place, val) clObject *obj_place, val; {
  clObjectAddress(*obj_place) = clObjectAddress(val);
  return(val);
}

clObject clWordObject(word) clWord word; {
  clObject x;
  clObjectWord(x) = word;
  return(x);
}

clObject clAddressObject(addr) clObjectCell *addr; {
  clObject x;
  clObjectAddress(x) = addr;
  return(x);
}

#define DefPointerCvt(name, cell, field)	\
clObject name (addr) cell *addr;		\
{ union { clObject obj; cell *ptr; } x; x.ptr = addr; return(x.obj); }
/*{ clObject x; &(x.address->field) = addr; return(x); }*/

DefPointerCvt(clStandardInstancepObject, clStandardInstanceCell, standard_instance)
DefPointerCvt(clWrapperpObject, clWrapperCell, wrapper)
DefPointerCvt(clSymbolpObject, clSymbolCell, symbol)
DefPointerCvt(clSimpleBaseStringpObject, clSimpleBaseStringCell, simple_base_string)

#else  /* no cl_CTYPECHECKS */
#  define clMakeHeapObject(size)	((clObject) clMalloc(size))
#  define clMakeClearHeapObject(size)	((clObject) clCMalloc(size))
#endif

/******************************************************************
  MULTIPLE VALUES
 ******************************************************************/
static clMultipleValuesElement values_buf[cl_MULTIPLE_VALUES_LIMIT+1];
clMultipleValuesElement *CL_current_values = values_buf;

#ifdef cl_CTYPECHECKS
clObject _clPrimaryValue()
{ return(_clPresentp(*CL_current_values) ? *CL_current_values : clNIL); }
#endif

#ifdef __STDC__
#  define InitFillEm(ap, array, ii)	\
  _clPresentp(clSetq(array[ii++], clCurrentArg(ap)))
#else
#  define InitFillEm(ap, array, ii)	1
#endif

#define FillEm(ap, array, ii)	\
  (clSetq(array[ii], _clGetObjectArg(ap)), _clPresentp(array[ii]))

clObject clValues(clVaAlist) clVaDcl
{ unsigned short i = 0;
  va_list _ap; clVaStart(_ap);

  if (InitFillEm(_ap, CL_current_values, i))
    while (FillEm(_ap, CL_current_values, i))
      clCheckCount(i, cl_MULTIPLE_VALUES_LIMIT-1, "MULTIPLE-VALUE-LIMIT exceeded.");
  
  clVaEnd(_ap);
  return(clPrimaryValue(last)); }

/******************************************************************
  FUNCTION CALLS
 ******************************************************************/
clBinding clMakeBinding() {
  clBinding b = (clBinding) clMalloc(sizeof(clObject));
  if (!b) _clStorageCondition();
  return(b);
}

#define cl_CALL_ARGUMENTS_LIMIT 50
#define CallArray(args)		clCallClosure(args[0])(clArgArray((args+1)))

/* N.B.: We consider call-arguments-limit to be the LOW bound for all functions.
   Since functions can always be invoked with FUNCALL or APPLY, we
   make sure that these can handle the function and
   CALL-ARGUMENTS-LIMIT ADDITIONAL arguments.  We have, accordingly,
   arranged for clCheckCount to signal an error when count is
   exceeded, not when it is about to be exceeded.  This is why clValues
   check for cl_MULTIPLE_VALUES_LIMIT-1. */

#define GetEm(args, ii)					\
  clObject args[cl_CALL_ARGUMENTS_LIMIT+1]; unsigned short ii = 0;	\
  va_list _ap; clVaStart(_ap);				\
  if (InitFillEm(_ap, args, ii))			\
    while (FillEm(_ap, args, ii))			\
      clCheckCount(ii, cl_CALL_ARGUMENTS_LIMIT, "CALL-ARGUMENTS-LIMIT exceeded."); \
  clVaEnd(_ap)

#define ShuffleEm(args, ii, list)	\
 clSetq(list, args[--ii]);		\
 while (!clTrue(clEndp(list, clEOA))) {	\
   clSetq(args[ii], clConsCar(list));	\
   clCheckCount(ii, cl_CALL_ARGUMENTS_LIMIT, "CALL-ARGUMENTS-LIMIT exceeded."); \
   clSetq(list, clConsCdr(list));	\
 } clSetq(args[ii], clEOA);

clObject clFuncallFunction(clVaAlist) clVaDcl { 
  GetEm(args, ii);
  return(CallArray(args));
}

  
clObject clApplyFunction(clVaAlist) clVaDcl {
  clObject list;
  GetEm(args, ii);
  ShuffleEm(args, ii, list);
  return(CallArray(args));
}

/* This first test for BUILT-IN-FUNCTION speeds things up AND it
   allows APPLY to be called before the class system is initialized. */
#define GetFunctionObject(args)	\
{ clObject class; clSetq(class, clObjectClass(args[0]));	\
  if (!_clEq(class, clClassVar(clBUILT_IN_FUNCTION)) &&		\
      !clTrue(clFunctionp(args[0], clEOA)))			\
    clSetq(args[0], clFdefinition(args[0], clEOA)); }
  								  
clObject clApply(clVaAlist) clVaDcl {
  clObject list;
  GetEm(args, ii);
  ShuffleEm(args, ii, list);
  GetFunctionObject(args)
  return(CallArray(args));
}

clObject clFuncall(clVaAlist) clVaDcl {
  GetEm(args, ii);
  GetFunctionObject(args)
  return(CallArray(args));
}

/******************************************************************
  CONTROL STACK
 ******************************************************************/
#ifdef LINT
   clControlOpCell *clMallocControlStack();
   clControlOpCell *clReallocControlStack();
   void clJumpToExit();
#else
#  define clMallocControlStack(size) (clControlOpCell *) clMalloc(size)
#  define clReallocControlStack(p, size) \
 	(clControlOpCell *) clRMalloc((char *)p, size)
#  define clJumpToExit(jbuf, exitp) _longjmp(jbuf, (int) exitp)
#endif

#ifdef EXPANDABLE_CONTROL_STACK
#define clStackGrowth(size) 	\
 (size + (unsigned) (1024 * sqrt(size / 1024)))

static void clGrowControlStack()
{ int n =  CL_dynamic_environment_limit - CL_dynamic_environment_base;
  unsigned old_size = (unsigned) n * sizeof(clControlOpCell);
  unsigned size = clStackGrowth(old_size);
  clWarn(clCharpSimpleBaseString("Growing control stack from ~d to ~d."),
	 clIntFixnum(old_size), clIntFixnum(size), clEOA);
  if (!(CL_dynamic_environment_base =
	clReallocControlStack(CL_dynamic_environment_base, size)))
    _clStorageCondition();
  CL_dynamic_environment_limit = CL_dynamic_environment_base +
    size/sizeof(clControlOpCell); 
  CL_dynamic_environment = CL_dynamic_environment_base + n; }

void clExtendControls(n) unsigned n;
{ if ((CL_dynamic_environment+=n) == CL_dynamic_environment_limit) clGrowControlStack(); }

#endif


void clDbind(symbol) clObject symbol; {
  clExtendControls(2);
  clSetq((CL_dynamic_environment-1)->value_data, _clSymbolValue(symbol));
  clSetq(CL_dynamic_environment->symbol_data, symbol);
}

void clProgvDbind(control, syms, vals)
     clProgvCell *control; clObject syms, vals;
{
  unsigned short count = 0;
  while (!clTrue(clEndp(syms, clEOA))) {
    clObject sym;
    clSetq(sym, clConsCar(syms));
    clDbind(sym);
    if (!clTrue(clEndp(vals, clEOA))) {
      clSetq(_clSymbolValue(sym), clConsCar(vals));
      clSetq(vals, clConsCdr(vals));
    } else
      clSetq(_clSymbolValue(sym), clUNBOUND);
    clSetq(syms, clConsCdr(syms));
    count++;
  }
  control->tag = cl_PROGV_TAG;
  control->count = count;
  clExtendControls(1);
  CL_dynamic_environment->progv_data = control;
}


clExitCell *_clFindCatcher(tag) clObject tag; {
  clControlOpCell *stack = CL_dynamic_environment;
  while (stack > CL_dynamic_environment_base) {
    switch (clControlObjectType(stack)) {
    case cl_DBIND_TAG:
    case cl_DBIND_TAG_WORD_ALGINED:
      stack--;
      break;
    case cl_CATCH_TAG:
      if (_clEq(stack->catch_data->catch_tag, tag)) {
	clReceivesValues(*stack->catch_data->buffer);
	return(stack->catch_data->buffer);
      }
      break;
    case cl_PROGV_TAG:
      stack -= 2 * stack->progv_data->count;
      break;
    }
    stack--;
  }
  clError(clCONTROL_ERROR, keyFORMAT_CONTROL,
	  clCharpSimpleBaseString("Unknown catch tag ~s."),
	  keyFORMAT_ARGUMENTS, clCons(tag, clNIL, clEOA),
	  clEOA);
  return(NULL);
}

static void UnrecognizedControlTag() {
  unsigned x = clControlObjectType(CL_dynamic_environment);
  clError(clCONTROL_ERROR, keyFORMAT_CONTROL,
	  clCharpSimpleBaseString("Unrecognized control tag #x~x at #x~x."),
	  keyFORMAT_ARGUMENTS,
	  clList(clIntFixnum((int) x),
		 clIntFixnum((int) clRemoveControlOp()),
		 clEOA),
	  clEOA);
}
  

void clUnwind(n) unsigned n; {
  while (n--) {
    /* This SHOULD only happen as a result of someone writing bad C code. */
    if (CL_dynamic_environment <= CL_dynamic_environment_base)
      _clControlError("Attempt to unwind to non-existent dynamic environment.");
    switch (clControlObjectType(CL_dynamic_environment)) {
    case cl_DBIND_TAG:
    case cl_DBIND_TAG_WORD_ALGINED:
       {
	 clObject sym;
	 clSetq(sym, clRemoveControlOp()->symbol_data);
	 clSetq(_clSymbolValue(sym), clRemoveControlOp()->value_data);
       }
       break; 
    case cl_CATCH_TAG:
    case cl_CLEANUP_TAG:
      clRemoveControlOp();
      break;
    case cl_ENVIRONMENT_CLEANUP_TAG:
      *CL_dynamic_environment->environment_cleanup_data->hook =
	CL_dynamic_environment->environment_cleanup_data->old_env; 
      clRemoveControlOp();
      break;
    case cl_PROGV_TAG:
      clUnwind(clRemoveControlOp()->progv_data->count);
      break;
    default:
      UnrecognizedControlTag();
      break;
    }
  }
}

void clUnwindExit(exitp) clExitCell *exitp; {
	clControlOpCell *target;
//	fprintf(stderr, "\n1 %ld\n", (long) exitp);
  target = clDEnvPointer(exitp->dynamic_environment);
//	fprintf(stderr, "\n2\n");
  while (CL_dynamic_environment > target) {
    /* This SHOULD only happen as a result of someone writing bad C code. */
    if (CL_dynamic_environment <= CL_dynamic_environment_base)
      _clControlError("Attempt to unwind to non-existent dynamic environment.");
    switch (clControlObjectType(CL_dynamic_environment)) {
    case cl_DBIND_TAG:
    case cl_DBIND_TAG_WORD_ALGINED:
       {
	 clObject sym;
	 clSetq(sym, clRemoveControlOp()->symbol_data);
	 clSetq(_clSymbolValue(sym), clRemoveControlOp()->value_data);
       }
      break; 
    case cl_CATCH_TAG:
      clRemoveControlOp();
      break;
    case cl_CLEANUP_TAG:
      clJumpToExit(clRemoveControlOp()->cleanup_data->machine_state,
		   exitp); 
    case cl_ENVIRONMENT_CLEANUP_TAG:
      *CL_dynamic_environment->environment_cleanup_data->hook =
	CL_dynamic_environment->environment_cleanup_data->old_env; 
      clRemoveControlOp();
      break;
    case cl_PROGV_TAG:
      clUnwind(clRemoveControlOp()->progv_data->count);
      break;
    default:
      UnrecognizedControlTag();
      break;
    }
  }
  _longjmp(exitp->machine_state, (int) exitp->exit_flag);
}



/*********************************************************************
 *********************************************************************
 * UTILITIES
 *********************************************************************/
#define clBaseStringCharp(obj)	clBaseStringCharp1(obj, 0)

clObject _clTypeError(datum, name) clObject datum; const char *name; {
  return(clError(clTYPE_ERROR, keyDATUM, datum,
		 keyEXPECTED_TYPE,
		 clIntern(clCharpSimpleBaseString(name), PKG_ECLIPSE, clEOA),
		 clEOA));
}

clObject _clControlError(str) const char *str; {
  return(clError(clCONTROL_ERROR, keyFORMAT_CONTROL,
		 clCharpSimpleBaseString(str), clEOA));
}

clObject _clMakeSystemError __P((const char *));
clObject _clMakeSystemError(str) const char *str; {
  int err = errno; 
  char *desc = strerror(err);
  errno = 0; /* In case someone looks later. */
#ifdef _WIN32
  if (err == 0) err = GetLastError();
#endif
  return(clMakeCondition(clSYSTEM_ERROR,
			 keyFUNCTION, clCharpSimpleBaseString(str),
			 keyERRNO, clIntFixnum(err),
			 keyDESCRIPTION, clCharpSimpleBaseString(desc),
			 clEOA));
}

clObject _clSystemError(str) const char *str; { 
  return(clError(_clMakeSystemError(str), clEOA));
}

clObject _clStorageCondition() {
  return(clError(clSTORAGE_CONDITION, clEOA));
}

/* Some Windows APIs use the errno/strerror protocol.  Others don't. */
/* We don't actually use this now, but we might in the future. */
#ifdef XXX_WIN32
clObject _clWSAError(const char *str) {
  char *desc; int err = WSAGetLastError();
  switch (err) {	/* Windows ought to decode this for us, like strerror()! */
  case WSANOTINITIALISED: desc = "No successfull WSAStartup"; break;
  case WSAEFAULT: desc = "No resources available"; break; 
  case WSAENETDOWN: desc = "Network failed"; break;
  case WSAEINVAL: desc = "Bad parameters"; break;
  case WSAEINTR: desc = "Blocking call cancelled"; break;
  case WSAEINPROGRESS: desc = "Call in progress"; break;
  case WSAENOTSOCK: desc = "Not a socket"; break;
  case WSASYSNOTREADY: desc = "Network not ready"; break;
  case WSAVERNOTSUPPORTED: desc = "Windows sockets version not supported"; break;
  case WSAEPROCLIM: desc = "Too many sockets tasks"; break; 
  default: desc = NULL;
  }
# ifdef clDEBUG_INIT
  puts("\n***Windows Sockets error."); puts(str); if (desc) puts(desc);
# endif
  return(clError(clMakeCondition(clSYSTEM_ERROR,
				 keyFUNCTION, clCharpSimpleBaseString(str),
				 keyERRNO, clIntFixnum(err),
				 keyDESCRIPTION, clCharpSimpleBaseString(desc),
				 clEOA)));
}
#endif


#define clVectorMSize(vector_cell, element_cell, n_elts)	\
  ((m_size) (sizeof(vector_cell) + (n_elts-1)*sizeof(element_cell)))

clObject _clMakeTaggedInstance(size, class, wrapper)
     m_size size; clObject class, wrapper;
{
  clObject obj;
  clSetq(obj, clMakeHeapObject(size));
  clSetTaggedInstanceClass(obj, class);
  clSetTaggedInstanceWrapper(obj, wrapper);
  return(obj);
}

/* Returns true if x is a base-string, and assigns *bp to the address
  of the first char. Returns false if x is an extended-string, and
  assigns *ep to the address of the first wchar_t.  Should be called
  at top-level with offset=0. */
static int StringData(x, bp, ep, offset)
     clObject x; char **bp; wchar_t **ep; int offset;
{
  clObject class;
  clSetq(class, clTaggedInstanceClass(x));
  if (_clEq(class, clClassVar(clSIMPLE_BASE_STRING))) {
    *bp = clSimpleBaseStringCharp(x) + offset;
    return(1);
  }
  if (_clEq(class, clClassVar(clSIMPLE_EXTENDED_STRING))) {
    *ep = clSimpleStringWcharp(x) + offset;
    return(0);
  }
  return(StringData(clSimpleArrayContents(x), bp, ep,
		    offset + clFixnumInt(clComplexArrayOffset(x))));
}

unsigned int StringLength(x) clObject x; {
  clObject class; clSetq(class, clObjectClass(x));
  if (_clEq(class, clClassVar(clSIMPLE_BASE_STRING)) ||
      _clEq(class, clClassVar(clSIMPLE_EXTENDED_STRING)))
    return(clIndexInt(clVectorSize(x)));
  else
    return(clFixnumInt(clComplexArrayFillPointer(x)));
}

#define RequiredArg(name)	\
 clSetq(name, (_clVp(_ap) ? clVpop(_ap) : clMissingArgs(clNIL, clEOA)))
#define OptionalArg(name, val)	\
 clSetq(name, (_clVp(_ap) ? clVpop(_ap) : val))
#define NoMoreArgs()	if (_clVp(_ap)) clExtraArgs(clVpop(_ap), clEOA)

#define NoArgs()	\
  { clBeginParse(_ap); NoMoreArgs(); clEndParse(_ap); }
#define OneArg(name)	\
  { clBeginParse(_ap); \
    RequiredArg(name); NoMoreArgs(); \
    clEndParse(_ap); }
#define TwoArgs(name1, name2)	\
  { clBeginParse(_ap); \
    RequiredArg(name1); RequiredArg(name2); NoMoreArgs(); \
    clEndParse(_ap); }
#define ThreeArgs(name1, name2, name3)	\
  { clBeginParse(_ap); \
    RequiredArg(name1); RequiredArg(name2); RequiredArg(name3); \
    NoMoreArgs(); clEndParse(_ap); }

/* We explicitly do not check for a second argument.  This allows us to
     specify eoa/unbound as a second argument. Note that a missing
     second arg does NOT default to clNIL */
#define _TwoArgs(name1, name2)	\
  { clBeginParse(_ap); \
    RequiredArg(name1); clSetq(name2, clVpop(_ap)); NoMoreArgs(); \
    clEndParse(_ap); }

clObject clEq(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clValues1(clTest(_clEq(x, y))));
}

/* These are defined as generic functions, which, right now, do not
    automatically generate machine functions. */
clObject clEqual(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clEQUAL))(x, y, clEOA));
}
clObject clEqualp(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clEQUALP))(x, y, clEOA));
}
clObject clEqualHash(clVaAlist) clVaDcl {
  clObject x; OneArg(x);
  return(clCallClosure(clSymbolFunctionValue(clEQUAL_HASH))(x, clEOA));
}

clObject clAdd(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clADD))(x, y, clEOA));
}
clObject clSubt(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clSUBT))(x, y, clEOA));
}
clObject clMult(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clMULT))(x, y, clEOA));
}
clObject clDiv(clVaAlist) clVaDcl {
  clObject x, y; TwoArgs(x, y);
  return(clCallClosure(clSymbolFunctionValue(clDIV))(x, y, clEOA));
}

clObject clTypep(clVaAlist) clVaDcl {
  clObject x, type, env;
  { clBeginParse(_ap);
    RequiredArg(x); RequiredArg(type); OptionalArg(env, clNIL);
    NoMoreArgs();
    clEndParse(_ap); }
  return(clFuncallFunction(clSymbolFunctionValue(clTYPEP),
			   x, type, env, clEOA));
}

clObject clNreverse(clVaAlist) clVaDcl {
  clObject x; OneArg(x);
  return(clCallClosure(clSymbolFunctionValue(clNREVERSE))(x, clEOA));
}

clObject clFormat(clVaAlist) clVaDcl {
  clObject args;
  { clBeginParse(_ap); clSetq(args, clVargs(_ap)); clEndParse(_ap); }
  return(clApplyFunction(clSymbolFunctionValue(clFORMAT), args, clEOA));
}


/* The initial check allows this to be called during bootstrapping
   when the class objects have not yet been initialized.
   IWBNI the few places that used this had an underlying function that
   received a class AND a wrapper explicitly, and that this underlying
   function were used during bootstrapping rather than calling
   classwrapper. For example, the complex array issues come up when
   (MAKE-COMPLEX-ARRAY ...) forms are used to initialize the
   CLASS-PROTOTYPES of the corresponding BUILT-IN-CLASSes.  It would
   be better for the static-defclass form to generate a call to
   make-complex-array1, for example. */
clObject clClassWrapper(x, eoa) clObject x, eoa; {
  clIgnore(eoa);
#define check(name) \
  if (_clEq(x, clClassVar(name))) return(clWrapperVar(name)); 
  check(clOPEN_ADDRESS_EQUAL_HASH_TABLE);
  check(clCOMPLEX_ARRAY);
  check(clCOMPLEX_BASE_STRING);
  check(clCOMPLEX_EXTENDED_STRING);
  check(clCOMPLEX_VECTOR);
  check(clCOMPLEX_BIT_VECTOR);
  return(clGetSlot(clStandardInstanceSlots(x), clIntIndex(16)));
}

int _clSymbolp(x) clObject x; {
  clObject class; clSetq(class, clObjectClass(x)); 
  return(_clEq(class, clClassVar(clSYMBOL)) ||
	 _clEq(class, clClassVar(clNULL)));
}
int _clListp(x) clObject x; {
  clObject class; clSetq(class, clObjectClass(x)); 
  return(_clEq(class, clClassVar(clCONS)) ||
	 _clEq(class, clClassVar(clNULL)));
} 
int _clStringp(x) clObject x; {
  clObject class; clSetq(class, clObjectClass(x)); 
  return(_clEq(class, clClassVar(clSIMPLE_BASE_STRING)) ||
	 _clEq(class, clClassVar(clSIMPLE_EXTENDED_STRING)) ||
	 _clEq(class, clClassVar(clCOMPLEX_BASE_STRING)) ||
	 _clEq(class, clClassVar(clCOMPLEX_EXTENDED_STRING)));
}

		
/*********************************************************************
 * MEMORY
 *********************************************************************/
#if (defined hpux)
#  define RLIMIT_DATA 2		/* only defined in _KERNEL */
#endif

clObject clGcData() {
#ifdef unix
  caddr_t data_end;
  long data_segment_size;
  struct rlimit rl;

  data_segment_size = getrlimit(RLIMIT_DATA, &rl);
  if (-1 == data_segment_size) _clSystemError("getrlimit");
  data_segment_size = rl.rlim_cur;
  if (-1 == (clWord) (data_end = sbrk(0))) _clSystemError("sbrk");
#else
  int data_end = 0, data_segment_size = 0;		/* need windows code!!!*/
#endif

  return(clValues(
#ifdef clNOGC
		  clIntInteger(0),
		  clIntInteger(0),
#else
		  clIntInteger((int) GC_get_heap_size()),
		  clIntInteger((int) GC_get_bytes_since_gc()),
#endif
		  clIntInteger((long) data_end),
		  clIntInteger(data_segment_size),
		  clEOA));

}



/*********************************************************************
 * ASNYNCHROUNOUS INTERRUPTS
 *********************************************************************/
/* ISSUE: How does a C program know if the call stack overflowed?  Is
   it always SIGSEGV?  If so, do we have to make special arrangements
   to use another stack when handling the signal?  (See getrlimit(2).)
   We need to detect this and signal STORAGE-CONDITION. */

#ifdef _WIN32 
/* By default Windows masks useful FPE errors.
   It is still not clear how to enable an FPE error integer divide by zero! */
static void AllowExceptions(void) {
  _controlfp(0,
	     _EM_INVALID | _EM_ZERODIVIDE |
	     _EM_OVERFLOW | _EM_UNDERFLOW);
}
#endif
/* Warning: It may not be posix safe to call certain system functions
   within interrupt handers.  The potentially unsafe calls include:
   sbrk, localtime, gettimeofday, getrusage, getcwd, getenv,
   gethostid, getpitimer, fcntl, ioctl, lstat, getpwuid, readlink
   getdents, opendir, closedir, readdir, mbtowc, wctomb, memcpy, and
   all math library functions. */

void clCatchInterrupts __P((int));

static void IHandler
#if (defined hpux) || (defined SunOS4) || (defined _WIN32)
  __P((int, int)); static void 
  IHandler(sig, code) int sig, code; {
#elif (defined Solaris)
  __P((int, siginfo_t *, void *)); static void 
  IHandler(sig, sip, uap)
    int sig; siginfo_t *sip; void *uap; {
  int code = sip ? sip->si_code : 0; clIgnore(uap);
#else  /* ANSI passes only signal number to handler  */
  __P((int)); static void 
  IHandler(sig) int sig; { int code = 0;
#endif

#ifdef _WIN32
  _fpreset(); AllowExceptions();
#endif

  clCatchInterrupts(sig);
  clFuncall(clGeneralElt(_clSymbolValue(clstarINTERRUPTSstar),
			 clIntIndex(sig)),
	    clIntFixnum(sig),
	    clIntFixnum(code),
	    clEOA);
}
  
void clCatchInterrupts(sig) int sig; {  
#if (defined Solaris)
  /* extended POSIX style sigaction */
  struct sigaction act;
  act.sa_flags = SA_RESTART | SA_SIGINFO | SA_NODEFER;
  act.sa_sigaction = IHandler;
  sigemptyset(&act.sa_mask);
  if (-1 == (int) sigaction(sig, &act, NULL))
#else  /* Stick with ANSI signal */
  if (-1 == (clWord) signal(sig, IHandler))
#endif
    _clSystemError("signal");
}

/* The function clFpeClass, below, takes a FPE code (eg. a second 
   argument to a SIGFPE signal handler, and returns the name of the
   appropriate ANSI CL condition.  The cpp constants that clFpeClass
   were arbitrarilly chosen to be those defined by older unix systems.
   Mappings from these names to platform specific names is done here, 
   and any remaining undefined names get some unlikely value.*/
#if (defined linux)
 /* No code information, though it may be possible to hack some by
    pretending we're the kernel and including asm/sigcontext.h.
    See gc/os_dep.c for examples. */
#elif (defined Solaris)
#  define FPE_INTDIV_TRAP FPE_INTDIV
#  define FPE_FLTDIV_TRAP FPE_FLTDIV
#  define FPE_FLTOVF_TRAP FPE_FLTOVF
#  define FPE_FLTUND_TRAP FPE_FLTUND
#  define FPE_FLTINEX_TRAP FPE_FLTRES
#elif (defined _WIN32)
#  define FPE_DIV0_TRAP _FPE_ZERODIVIDE
#  define FPE_FLTOVF_TRAP _FPE_OVERFLOW
#  define FPE_FLTUND_TRAP _FPE_UNDERFLOW
#  define FPE_FLTINEX_TRAP _FPE_INEXACT
#  define FPE_FLTOPERR_TRAP _FPE_INVALID
#  define FPE_UNEMUL_TRAP _FPE_UNEMULATED
#elif (defined hpux)
#  define FPE_DIV0_TRAP 13	/* by inspection */
#endif /* platform-specific FPE code identifiers */

/* Defaults for the above */
#define FPE_UNDEFINED -1
#ifndef FPE_INTDIV_TRAP
#  define FPE_INTDIV_TRAP FPE_UNDEFINED-1
#endif
#ifndef FPE_FLTDIV_TRAP
#  define FPE_FLTDIV_TRAP FPE_UNDEFINED-2
#endif
#ifndef FPE_FLTOVF_TRAP
#  define FPE_FLTOVF_TRAP FPE_UNDEFINED-3
#endif
#ifndef FPE_FLTUND_TRAP
#  define FPE_FLTUND_TRAP FPE_UNDEFINED-4
#endif
#ifndef FPE_FLTINEX_TRAP
#  define FPE_FLTINEX_TRAP FPE_UNDEFINED-5
#endif
#ifndef FPE_FLTOPERR_TRAP
#  define FPE_FLTOPERR_TRAP FPE_UNDEFINED-6
#endif
#ifndef FPE_UNEMUL_TRAP
#  define FPE_UNEMUL_TRAP FPE_UNDEFINED-7
#endif
#ifndef FPE_DIV0_TRAP
#  define FPE_DIV0_TRAP FPE_UNDEFINED-8
#endif

clObject clFpeClass(code) int code; {
  switch (code) {
  case FPE_UNDEFINED:
	return(clARITHMETIC_ERROR); break;
  case FPE_DIV0_TRAP:
  case FPE_INTDIV_TRAP:
  case FPE_FLTDIV_TRAP:
    return(clDIVISION_BY_ZERO); break;
  case FPE_FLTOVF_TRAP:
    return(clFLOATING_POINT_OVERFLOW); break;
  case FPE_FLTUND_TRAP:
    return(clFLOATING_POINT_UNDERFLOW); break;
  case FPE_FLTINEX_TRAP:
    return(clFLOATING_POINT_INEXACT); break;
  case FPE_FLTOPERR_TRAP:
  case FPE_UNEMUL_TRAP:
    return(clFLOATING_POINT_INVALID_OPERATION); break;
  }
  return(clARITHMETIC_ERROR); 
}



/*********************************************************************
 * TIME
 *********************************************************************/	

/* In every system we have encountered, time_t units are integral seconds
   that advance linearly from January 1, 1970.  However, ISO permits time_t
   to be double and/or non-scalar.  We could accomodate most variations by 
   scaling and casting.  However, if time_t were non-linear, we would have
   to use combinations of gmtime(), mktime(), and difftime() to compute
   the conversion. */
#define TimeUnix(x) x  /* Convert time_t to int "unix time" seconds. */
#define UnixTime(x) x  /* Convert int "unix time" seconds to time_t. */
	       
/* clGetTimezone returns two pieces of information about the current 
   location, computed for the date and time given (as unix time seconds):
   - The number of minutes west of GMT.  (Includes any DST adjustment.)
   - Whether or not DST is in effect for the given time and place.

   Note that tm_gmtoff has positive zones to the East of Greenwich,
   while the timezone variable is positive to the West.  Lisp expects
   the latter. */
/* To compute timezone, it might be more portable to do something
   like: struct tm *loc = localtime(&clock_var);
         (long) difftime(clock_var, mktime(loc));
   Posix requires that time(&x) == mktime(gmtime(&x)), but ISO
   doesn't.  On a non-posix systems, we might need to use gmtime(),
   but then again, a non-posix system might not know the local
   timezone and so either localtime() or gmtime() might return NULL,
   in which case we loose anyway.  Note also, if using both
   localtime() and gmtime(), that they might over-write the same
   struct tm, so we would need to convert back the result of one to a
   saved time_t before calling the second. */

clObject clGetTimezone(unix_time) clObject unix_time; {
  time_t clock_var = UnixTime(clIntegerInt(unix_time));
  struct tm *time_struct = localtime(&clock_var);
  /* Some systems use negative values to indicate unknown dst status. */
  int dst = time_struct->tm_isdst > 0;
#ifdef SunOS4
  int zone = (0 - time_struct->tm_gmtoff);
#else
  int zone = timezone;
  if (dst) zone -= 3600;
#endif    
  return(clValues(clIntFixnum(zone/60),
		  clTest(dst),
		  clEOA));
}

clObject clUnixTime() {  
  long clock_var = (long) TimeUnix(time((time_t *) NULL));
  if (-1 == clock_var) _clSystemError("time");
  return(clIntInteger(clock_var));
}

/* The standard UNIX way to get run time and real time
   information is with the times() function, which gives values in
   clock ticks which are typically 1/60 or 1/100 of a second.
   This isn't nearly accurate enough, particularly for run time.
   Therefore, on machines which provide alternatives (sun), we use
   them.  Typically, the alternatives give numbers in too fine a unit
   (such as microseconds).  We cut them back to something more reasonable,
   like milliseconds. */

#if (defined sun)		/* Sun uses gettimeofday with microsecond values. */
#  define clHZ		1000
#  define Millisec(timeval) \
     (((long) timeval.tv_sec*1000) + ((long) timeval.tv_usec/1000))

static long sys_time(start) long start; {
  struct timeval tp;
  if (-1 == (int) gettimeofday(&tp, (struct timezone *) NULL))
    _clSystemError("gettimeofday");
  return(Millisec(tp) - start); 
}
#elif (defined _WIN32)	/* Windows uses ftime with CLOCKS_PER_SEC values. */
#  define clHZ		CLOCKS_PER_SEC

#  define Millisec(timeval) \
     (((long) timeval.time*1000) + ((long) timeval.millitm))
#  if clHZ != 1000
#    error Run time units are different than real time units.
#  endif

static long sys_time(start) long start; {
  struct _timeb tp;
  _ftime(&tp);	/* Windows does not provide for error checking. */
  return(Millisec(tp) - start); 
}
#elif (defined USE_POSIX_TIMES)	/* POSIX uses times with CLK_TCK values. */
/*#  define clHZ		CLK_TCK		/* or HZ */
#define clHZ   sysconf(_SC_CLK_TCK)

static long sys_time(start) long start; {
  struct tms p;
  clock_t clock_var = (long) times(&p);
  if (-1 == clock_var) _clSystemError("times");
  return(clock_var - start);
}
#else
#  error No mechanism defined for obtaining run time.
#endif

/* A any non-zero argument will reset the "arbitrary time base".  This
   gives us a workaround when a program has been running for years 
   and the real time is about to exceed fixnum size. */
clObject clRealTime(clVaAlist) clVaDcl {
  clObject resetp;
  static long start = 0L;
  { clBeginParse(_ap); OptionalArg(resetp, clEOA);
    NoMoreArgs(); clEndParse(_ap); }
  if (!start || (_clPresentp(resetp) && clTrue(resetp)))
    start = sys_time(0L)-1;	/* Make sure we never return 0=clEOA! */
  return(clValues1(clWordObject(sys_time(start))));
} 
   
clObject clRunTime(clVaAlist) clVaDcl {
#if (defined sun)
  struct rusage p; NoArgs();
  if (-1 == (int) getrusage(RUSAGE_SELF, &p)) _clSystemError("getrusage");
  return(clValues1(clWordObject(Millisec(p.ru_utime) + Millisec(p.ru_stime))));
#elif (defined USE_POSIX_TIMES)
  struct tms p; NoArgs();
  if (-1 == (int) times(&p)) _clSystemError("times");
  return(clValues1(clWordObject(p.tms_utime + p.tms_stime)));
#else /* Default to ISO clock().
    ISO clock() can wrap around in as little as 36 minutes (i.e. if 
	clock_t is signed and CLOCKS_PER_SEC is 1e6 (milliseconds)).  On
	Windows, it wraps in just under 25 days. */
  clock_t clock_var = clock(); NoArgs();
  if (-1 == (int) clock_var) _clSystemError("clock");
  return(clValues1(clWordObject(clock_var)));
#endif
}  

void clMillisleep(milliseconds) int milliseconds; {
#if (defined _WIN32)
	Sleep(milliseconds);
#else
	if (milliseconds) sleep(clCeil(milliseconds, 1000));
#endif
}


/*********************************************************************
 * OPERATING SYSTEM, MACHINE, AND ENVIRONMENT
 *********************************************************************/
#ifndef _POSIX_PATH_MAX
#  define _POSIX_PATH_MAX _MAX_PATH
#endif

clObject clGetcwd(clVaAlist) clVaDcl {
  char buf[_POSIX_PATH_MAX+2]; int len; NoArgs();
  if (!getcwd(buf, _POSIX_PATH_MAX)) _clSystemError("getcwd");
  len = strlen(buf);
  /* Arguably, it would be better to leave this as it comes from the
     OS, and fix it in Lisp using TRUENAME.  Note, however, that 
     clTruename() can't be called until after clInitFile(). */
  if (!len ||  (buf[len-1] != '/'))
    { buf[len++] = '/'; buf[len] = '\0'; }
  return(clValues1(clCharpSimpleBaseString(buf)));
}

clObject clGetenv(clVaAlist) clVaDcl {
  clObject key; OneArg(key);
  return(clValues1(clCharpSimpleBaseString(getenv(clBaseStringCharp(key)))));
}

clObject clUname(c) int c; {
	char *p; 
#ifdef unix
  extern char *cuserid();
  struct utsname name;
  switch (c) {
  case 'u':
  case 'h': break;
  default: if (uname(&name) == -1) c = '\0'; break;
  }
  switch (c) {
  case 'u': p = cuserid(NULL); break;
  case 'h': p = getenv("HOME"); break;
  case 's': p = name.sysname; break;
  case 'n': p = name.nodename; break;
  case 'm': p = name.machine; break;
  case 'r': p = name.release; break;
#ifdef hpux 
  case 'i': p = name.__idnumber; break;
#endif
  default: p = NULL;
  }
#elif (defined _WIN32)
  char buf[MAX_COMPUTERNAME_LENGTH + 2];
  char pbuf[MAX_PATH + 2];
  OSVERSIONINFO os; 
  SYSTEM_INFO sys;
  int plevel; char *p2 = NULL;
  switch (c) {
  case 'm':
  case 'z':
    GetSystemInfo(&sys);	/* No error checking. */
    /* Fall through to get GetVersionInfo */
  case 's':
  case 'r':
    os.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    if (!GetVersionEx(&os)) _clSystemError("GetVersionInfoEx");
    break;
  } /* End of preliminary switch to call info function */
  switch (c) {
  case 'u':
    plevel = MAX_COMPUTERNAME_LENGTH;
    p = GetUserName(buf, &plevel) ? buf : NULL;
    break;
  case 'h':
    if (!(p = getenv("HOMEPATH")))
      p = GetWindowsDirectory(pbuf, MAX_PATH) ? pbuf : NULL;
    break;
  case 'n':
    plevel = MAX_COMPUTERNAME_LENGTH+1;
    p = GetComputerName(buf, &plevel) ? buf : NULL;
    break;
  case 's':
    switch (os.dwPlatformId) {
    case VER_PLATFORM_WIN32_WINDOWS: p = "Windows-95"; break;
    case VER_PLATFORM_WIN32_NT: p = "Windows-NT"; break;
    default: p = "Windows"; break;
    } break;
  case 'r':
    return(clIdString(clIntFixnum(os.dwMajorVersion),
		      clIntFixnum(os.dwMinorVersion),
		      ((os.szCSDVersion && os.szCSDVersion[1]) ?
		       clCharpSimpleBaseString(os.szCSDVersion) :
		       clNIL), 
		      clEOA));
    break;
  case 'm':
    if (os.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
      /* Windows 95 only */
      switch (sys.dwProcessorType) {
      case PROCESSOR_INTEL_386: p = "I386"; break;
      case PROCESSOR_INTEL_486: p = "I486"; break;
      case PROCESSOR_INTEL_PENTIUM: p = "I586"; break;
      }
    else { /* The "current" way... */
      plevel = sys.wProcessorLevel;
      switch (sys.wProcessorArchitecture) {
      case PROCESSOR_ARCHITECTURE_INTEL: p = "I"; p2 = "86"; break;
      case PROCESSOR_ARCHITECTURE_ALPHA: p = "Alpha"; break;
      case PROCESSOR_ARCHITECTURE_PPC:
	switch (plevel) {
	case 6: plevel = 3; p2 = "+"; break;
	case 9: plevel = 4; p2 = "+"; break;
	}
	p = (plevel < 10) ? "PPC-60" : "PPC-6";
	break;
      case PROCESSOR_ARCHITECTURE_MIPS:
	p = "MIPS-R"; plevel *= 100; break; 
      }
      return(clIdString(clCharpSimpleBaseString(p),
			clIntFixnum(plevel),
			clCharpSimpleBaseString(p2),
			clNIL, clEOA));
    } break;
  case 'z':
    if (os.dwPlatformId != VER_PLATFORM_WIN32_WINDOWS) 
      return(clIdString(clIntFixnum((sys.wProcessorRevision & 0xFF00) >> 16),
			clIntFixnum(sys.wProcessorRevision & 0xFF),
			clEOA));
    else p = NULL;
    break;
  }
#endif
  return(clCharpSimpleBaseString(p));
}

clObject clMachineId(clVaAlist) clVaDcl {  
  int n; 
#if (defined sun) || (defined linux)
  n = (int) gethostid();
#elif (defined dec3100)
  n = getpitimer(NULL);
#elif (defined xx_WIN32)  /* No WSA interface on '95! */
  char hostnamebuf[MAX_COMPUTERNAME_LENGTH + 1];
  struct hostent *hostinfo;
  WSADATA wsaData; n = -1;
  if (WSAStartup(MAKEWORD(1, 0), &wsaData) == 0) {
	  if ((gethostname(hostnamebuf, MAX_COMPUTERNAME_LENGTH) == 0) &&
		  ((hostinfo = gethostbyname(hostnamebuf)) != NULL))
		  memcpy(&n, *hostinfo->h_addr_list, 4);
	  WSACleanup();
  }	
#elif (defined ibm) || (defined hpux)
#  ifdef hpux
     int key = 'i';
#  else
     int key = 'm';
#  endif
  return(clValues1(clParseInteger(clUname(key),
				  keyRADIX, clIntFixnum(16),
				  keyJUNK_ALLOWED, clNIL, clEOA)));
#else  /* unknown machine-id */
  n = -1;
#endif
  if (-1 == n) return(clValues1(clNIL));
  return(clValues1(clIntInteger(n)));
}


/*********************************************************************
 * STREAMS AND FILE SYSTEM
 *********************************************************************/
/* There are two places where mode 0777 or 0666 is used:
   checked_open() and clMakeDir().  In each case,
   open(), or mkdir() will correctly xor this with the process umask
   to obtain the real mode used in creating the file. */

static clObject FileError __P((clObject, const char *));
static clObject FileError(path, msg)
     clObject path; const char *msg;
{
  char *desc = strerror(errno);
  return(clError(clFILE_ERROR,
		 keyPATHNAME, path,
		 keyFORMAT_CONTROL,
		 clCharpSimpleBaseString("~s cannot be ~a~@[: ~a~]."),
		 keyFORMAT_ARGUMENTS,
		 clList(path,
			clCharpSimpleBaseString(msg),
			clCharpSimpleBaseString(desc),
			clEOA),
		 clEOA));
}

/* We currently use ISO-C, POSIX, and Unix functions which take char *
   arguments to represent file namestrings (as opposed to, say,
   wchar_t *).  If this were to change, this macro and the functions
   that use it would be effected. */

#define OSNString(lisp_string) clSimpleBaseStringCharp(lisp_string)

static char *GetPathNs __P((clObject));
static char *GetPathNs(path) clObject path; {
  return(OSNString(clOsNamestring(path, clEOA)));
}

static clObject CheckedOpen __P((clObject, int));
static clObject CheckedOpen(path, flags) clObject path; int flags; {
  int fd = open(GetPathNs(path), flags, 0666);
  if (fd == -1) return(FileError(path, "opened"));
  else return(clIntFixnum(fd));
}

clObject clReadOpen(path) clObject path; {
  return(CheckedOpen(path, O_RDONLY));
}
clObject clWriteOpen(path) clObject path; {
  return(CheckedOpen(path, O_WRONLY | O_CREAT));
}
clObject clReadWriteOpen(path) clObject path; {
  return(CheckedOpen(path, O_RDWR | O_CREAT));
}

/* The old Unix name for remove() is unlink(). */
clObject clFileDelete(path) clObject path; {
  if (remove(GetPathNs(path)) == -1)
    FileError(path, "deleted");
  return(clT);
}

clObject clDirDelete(path) clObject path; {
  if (rmdir(GetPathNs(path)) == -1)
    FileError(path, "deleted");
  return(clT);
}

clObject clMakeDir(path) clObject path; {
  if (mkdir(GetPathNs(path)
#ifndef _WIN32
	  , (mode_t) 0777
#endif
	  ) == -1)
    FileError(path, "created");
  return(clT);
}

clObject clFileRename(old, new) clObject old, new; {
  if (rename(GetPathNs(old), GetPathNs(new)) == -1)
    FileError(new, "created as new name");
  return(clT);
}

/* Get information for a file into a stat buffer and return true.
   If the file doesn't exist, return false.
   Signal FILE-ERROR if necessary. */
static int FInfo __P((clObject, struct stat *));
static int FInfo(path, buf) clObject path; struct stat *buf; {
  char *ns = GetPathNs(path);
#ifdef _WIN32			/* Can't handle trailing separator. */
  char ns1[_MAX_PATH+1];
  int end = strlen(ns)-1;
  if (ns[end] == '\\') {
    memcpy(ns1, ns, end);
    ns1[end] = '\0';
    ns = ns1;
  }
#endif
  if (lstat(ns, buf) == -1) {
    if (errno == ENOENT) return(0);
    else FileError(path, "examined");
  }
  return(1);
}

clObject clFileModified(path) clObject path; {
  struct stat buf;
  if (!FInfo(path, &buf)) return(clNIL);
  return(clIntInteger((long) buf.st_mtime));
}

char *clFileOwner(path) clObject path; {
  struct stat buf;
#ifdef _WIN32  /* Can't we use file properties??? */
  FInfo(path, &buf); return(NULL);
#else
  struct passwd *pw;
  if (!FInfo(path, &buf)) return(NULL);
  if (!(pw = getpwuid((uid_t) buf.st_uid))) return(NULL);
  return(pw->pw_name);
#endif
}

#ifdef _WIN32
#  define S_ISREG(mode)	mode & _S_IFREG
#  define S_ISDIR(mode) mode & _S_IFDIR
#  define S_ISLNK(mode) 0	/* But aren't "shortcuts" really links? */
#endif

clObject clFileType(path) clObject path; {
  struct stat buf;
  if (!FInfo(path, &buf)) return(clNIL);
  if (S_ISREG(buf.st_mode)) return(clT);
  if (S_ISDIR(buf.st_mode)) return(keyDIRECTORY);
  if (S_ISLNK(buf.st_mode)) return(keyLINK);
  return(clNIL);
  }

static char link_buf[_POSIX_PATH_MAX];
char *clFollowLink(path) clObject path; {
#ifdef _WIN32
  return(NULL);				/* Is there a way to "read" a "shortcut"? */
#else
  int r = readlink(GetPathNs(path), link_buf, _POSIX_PATH_MAX);
  if (r == -1) FileError(path, "followed");    
  link_buf[r] = '\0';
  return(link_buf);
#endif
}

/* Some implementation of opendir() use malloc() and thus
   conflict with sbrk() and the garbage collector. 
   The Solaris 2.6 man page for opendir no longer says anything
   about using malloc, and opendir() does work more reliably, but not
   COMPLETELY reliably.

   USE_POSIX_OPENDIR should be defined ONLY on those systems which are
   known to have a reliable (i.e. non-malloc-using) definition of
   opendir(). */

#ifdef clDEBUG_DIRECTORY
#  define clDEBUGF(args) (void) printf args
#else
#  define clDEBUGF(args)
#endif

#if (defined USE_POSIX_OPENDIR)
typedef struct {
  clObject path;
  DIR dir;
} cl_DIR;
#  define cl_opendir(ns)	opendir(ns)
#  define cl_closedir(dd)	closedir(dd->dir)
#  define cl_readdir(dd)	readdir(dd->dir)

/* Otherwise, we must define our own */
#elif (defined _WIN32)	

#define dirent _finddata_t
#define d_name name
typedef struct {
	clObject path;
	long handle;
	int first_time_p;
	struct _finddata_t fileinfo;
} cl_DIR;

static cl_DIR *cl_opendir __P((char *));
static cl_DIR *cl_opendir(ns) char *ns; {
	char buf[_MAX_PATH+1], *pb = buf;
	cl_DIR *dd = (cl_DIR *) GC_malloc_atomic(sizeof(cl_DIR));
	while (*ns) *pb++ = *ns++;	/* buf holds "'ns'*" */
	*pb++ = '*'; *pb = '\0';
	clDEBUGF(("opening |%s|\n", buf));
	if (-1 == (dd->handle = _findfirst(buf, &dd->fileinfo))) return(NULL);
	dd->first_time_p = 1;
	return(dd);
}
static int cl_closedir __P((cl_DIR *));
static int cl_closedir(dd) cl_DIR *dd; {
  return(_findclose(dd->handle));
}

static struct dirent *cl_readdir __P((cl_DIR *));
static struct dirent *cl_readdir(dd) cl_DIR *dd; {
	if (dd->first_time_p) dd->first_time_p = 0;
	else {
		if (-1 == _findnext(dd->handle, &dd->fileinfo)) {
			if (errno == ENOENT) errno = 0;
			return(NULL);
		}
	}
	return(&dd->fileinfo);
}

#else  /* use Unix open/getdents/close */

/* On some systems, we will lose entires if we use a buffer size
   smaller than the block size used by the file system.  */
#  if (defined hpux)
#    define cl_DIR_BUF_SIZE DIRBLKSIZ
#  elif (defined sun)
#    define cl_DIR_BUF_SIZE DIRBUF
#  else
     /* Some dirent structs define d_name[1], while others use
        d_name[MAXNAMLEN].  This should work with either. */
#    define cl_DIR_BUF_SIZE (sizeof(struct dirent) + MAXNAMLEN + 1)
#  endif

typedef struct {
  clObject path;
  unsigned int fd;		/* Some unix calls expect unsigned,
				   others signed! */
  char buf[cl_DIR_BUF_SIZE];
  struct dirent *next;
  struct dirent *end;
} cl_DIR;

static cl_DIR *cl_opendir __P((char *));
static cl_DIR *cl_opendir(ns) char *ns; {
  cl_DIR *dd = (cl_DIR *) GC_malloc_atomic(sizeof(cl_DIR));
  clDEBUGF(("opening |%s|\n", ns));
  if (-1 == (dd->fd = open(ns, O_RDONLY))) return(NULL);
  dd->next = dd->end = (struct dirent *)&dd->next;
  return(dd);
}
static int cl_closedir __P((cl_DIR *));
static int cl_closedir(dd) cl_DIR *dd; {
  return(close((int) dd->fd));
}


#if (defined hpux)
int getdents __P((int, struct dirent *, size_t));
int getdents(fd, buf, nbytes) int fd; size_t nbytes; struct dirent *buf; {
  char *dp = (char *) buf;
  nbytes = read(fd, dp, nbytes);
  clDEBUGF(("read %d bytes\n", nbytes));
    /* "If the first entry of a directory block is free, then its
       dp->d_ino is set to 0." */
  if ((nbytes > 0) && (buf->d_ino == 0))
    memmove(dp, dp + buf->d_reclen,
	    nbytes -= buf->d_reclen);
  return(nbytes);
}
#endif

/* Absurdly, SunOS4 and Linux getdents() may give EFAULT error if
   buffer argument is within clMalloc()'ed space.  We may work around
   this by passing a static buffer and copying it to the buffer within
   dd. */
#if (defined SunOS4) || (defined linux)
#  define BROKEN_GETDENTS_ADDRESS
#endif

static struct dirent *cl_readdir __P((cl_DIR *));
static struct dirent *cl_readdir(dd) cl_DIR *dd; {
  struct dirent *entry = dd->next;
#ifdef BROKEN_GETDENTS_ADDRESS
  static char hack[cl_DIR_BUF_SIZE];
#  define HACK_BUF hack
#else
#  define HACK_BUF &(dd->buf)
#endif
  if ((char *)entry >= (char *) dd->end) {
/*
    int nb = getdents(dd->fd,
		      (struct dirent *) HACK_BUF,
		      cl_DIR_BUF_SIZE);
*/
    int nb = syscall(SYS_getdents, dd->fd,
		      (struct dirent *) HACK_BUF,
		      cl_DIR_BUF_SIZE);

    /* Should we repeat if nb==-1 and errno=EIO or EINTR ? */
    if (nb <= 0) return(NULL);

#ifdef BROKEN_GETDENTS_ADDRESS
    (void) memcpy(&(dd->buf), HACK_BUF, (size_t) nb);
#endif

    dd->end = (struct dirent *) &(dd->buf[nb]);
    entry = dd->next = (struct dirent *) dd->buf;
    clDEBUGF(("nb = %d, entry = %x, dd->end = %x\n", nb, entry, dd->end));
  }
  /* Are there any systems where we need to check for d_reclen==0? */
  dd->next = (struct dirent *) (((char *)dd->next) + entry->d_reclen);
  clDEBUGF(("  entry = %x + %d bytes => dd->next = %x\n", entry, entry->d_reclen, dd->next));

  return(entry);
}
#endif /* opendir implementations */

clObject clOpenDir(path) clObject path; {
  cl_DIR *dd = cl_opendir(GetPathNs(path));
  if (dd == NULL) return(FileError(path, "opened"));
  clSetq(dd->path, path);
  return(clAddressObject((clObjectCell *) dd));
}

clObject clCloseDir(dir) clObject dir; {
  cl_DIR *dd = (cl_DIR *) clObjectAddress(dir);
  if (cl_closedir(dd))
    return(FileError(dd->path, "closed"));
  return(clNIL);
}

/* Return the next entry from dd that matches lname, otherwise NULL.
   Matching is case insenstive. */
char *clReadDir(dd, lname) clObject dd; clObject lname; {
  struct dirent *entry;
  char *name = OSNString(lname);
  cl_DIR *dirp = (cl_DIR *) clObjectAddress(dd);
  clDEBUGF(("   scanning %x for |%s|\n", dirp, name));
  errno = 0;
  while ((entry = cl_readdir(dirp)) != NULL) {
    char *p1 = entry->d_name, *p2=name;
    clDEBUGF(("   |%s|\n", p1));
    while (*p2) if (toupper(*p2++) != toupper(*p1++)) goto next_read;
    /* Some readdir return "." and "..", others don't.
       The only way we can be consistent is to filter them out unless
       specifically asked. */
    if (*name || (*(p1 = entry->d_name) != '.') ||
	((*++p1 != '\0') && ((*p1 != '.') || (*++p1 != '\0'))))
      return(entry->d_name);
  next_read: ;
  }
  if (errno != 0) FileError(dirp->path, "scanned");
  return(NULL);
}

/* File Descriptor operations */
#if (defined unix)
#  define tcflushInput(fd)		tcflush(fd, TCOFLUSH)
#  define tcflushOutput(fd)		tcflush(fd, TCIFLUSH)
#elif (defined _WIN32)
#  define clOSHandle(fd)		((HANDLE) _get_osfhandle(fd))
/* CancelIO is not available on Windows-95! */
#  define tcflushInput(fd)		-1 /*(CancelIo(clOSHandle(fd)) ? 0 : -1)*/
#  define tcflushOutput(fd)		tcflushInput(fd)
/* Should we also call WaitForSingleObject(clOSHandle(fd), INFINITE)?
   What if fd is open for writing AND reading?  Wouldn't
   WaitForSingleObject hang? */
#  define tcdrain(fd)			_commit(fd)
#endif

/* returns -1 on error, 0 for no data, n>0 if data available for
   immediate reading.  Another way to do this is:
      ioctl(fd, I_NREAD, &arg) ? -1 : arg
   where I_NREAD is for streamio, and FIONREAD would be for FILIO.  
   We don't use those because they are less portable.
   poll() is more reliable and faster than select(), though not as
   widely available. */
int tcpoll(fd) int fd; {
#if (defined linux)
  struct timeval tm; fd_set fds; 
  tm.tv_sec = tm.tv_usec = 0;
  FD_ZERO(&fds);
  FD_SET(fd, &fds);
  return(select(1, &fds, NULL, &fds, &tm));
#elif (defined _WIN32)
  switch (WaitForSingleObject(clOSHandle(fd), 0)) {	  
  case WAIT_TIMEOUT: return(0);
  case WAIT_FAILED: return(-1);
  default: return(1); 
  }
#else
  struct pollfd fds[1]; 
  fds[0].fd = fd;
  fds[0].events = POLLIN | POLLPRI | POLLRDNORM | POLLRDBAND;
  return(poll(fds, 1, 0)); 
#endif
}


/* All the remaining functions in this section take a clObject fd
   argument representing an int file-descriptor.  IWBNI if some
   protocol were arranged whereby they could report operating system
   errors to their Lisp callers.  Maybe they should just take a stream
   argument and signal stream-error directly? */ 

clObject clFileDescriptorClose(fd) clObject fd; {
  return(clTest(!close(clFixnumInt(fd))));
}

clObject clDrain(fd) clObject fd; {
  return(clTest(!tcdrain(clFixnumInt(fd))));
}

clObject clFlushInput(fd) clObject fd; {
  return(clTest(!tcflushInput(clFixnumInt(fd))));
}

clObject clFlushOutput(fd) clObject fd; {
  return(clTest(!tcflushOutput(clFixnumInt(fd))));
}

clObject clPoll(fd) clObject fd; {
  return(clTest(tcpoll(clFixnumInt(fd))));
}

clObject clInteractivep(fd) clObject fd; {
  return(clTest(isatty(clFixnumInt(fd))));
}


/* To block/unblock streams, we could use tcsetattr(), or:

   #include <sys/filio.h>		
     ioctl(ifd, FIONBIO, 0);	
   for blocking, and 1 for unblocking. 

   #include <fcntl.h>
     val = fcntl(ifd, F_GETFL, NULL);
     if (val != -1) val = fcntl(ifd, F_SETFL, val ^ FNDELAY);
   for blocking, and val | FNDELAY for unblocking.
*/

/* Should this use fstat() instead? */
int clFileDescriptorLength(fd_obj) clObject fd_obj; {
  int fd = clFixnumInt(fd_obj);
  off_t cur = lseek(fd, (off_t) 0L, SEEK_CUR), end;
  if (-1 == cur) return(cur);
  end = lseek(fd, (off_t) 0L, SEEK_END);
  (void) lseek(fd, cur, SEEK_SET); return(end);
}

int clFileDescriptorPosition(fd) clObject fd; {
  return(lseek(clFixnumInt(fd), (off_t) 0L, SEEK_CUR));
}

int clFileDescriptorSeek(fd, pos) clObject fd; int pos; {
  return(-1 != lseek(clFixnumInt(fd), (off_t) pos, SEEK_SET));
}


/* Size is maximum characters to retrieve. 
   Will typically read until size or eof for non-interactive
   (blocking) streams. */
clObject clReadAscii(fd, buffer, offset, size)
     clObject fd, buffer, offset, size; 
{ return(clIntFixnum(read(clFixnumInt(fd),
			  clSimpleBaseStringCharp(buffer) +
			  clFixnumInt(offset),
			  (size_t) clFixnumInt(size)))); }

clObject clWriteAscii(fd, buffer, offset, size)
     clObject fd, buffer, offset, size;
{ return(clIntFixnum(write(clFixnumInt(fd),
			   clSimpleBaseStringCharp(buffer) +
			   clFixnumInt(offset),
			   (size_t) clFixnumInt(size)))); }

#define BUFSIZE (12 * 128)		/* must be multiple of (lcm 4 6) */
/* Forces out all the bytes.  We call this rather than write() when a
   number of bytes together represent an atomic datum, and outputing
   only some would cause us grief.  
   Issue: what if there is a good reason for the OS to not write out
   all the bytes? We should really buffer the unrwritten bytes in the
   Lisp stream, the way we do with unread bytes (partial characters)
   in multi-byte input. */ 
static int xwrite __P((int, char *, size_t));
static int xwrite(fd, buf, nbyte) int fd; char *buf; size_t nbyte; {
  int n, total = nbyte;
  while (nbyte)
    { n = write(fd, buf, nbyte);
      nbyte -= n;
      buf += n;
    }
  return(total);
}


clObject clReadUcs2(fd, buffer, offset, size)				
     clObject fd, buffer, offset, size;						
{ static char buf[BUFSIZE]; char *b = buf; wchar_t *p, *e;
  int n = clFixnumInt(size) * 2;
  if (n > BUFSIZE) n = BUFSIZE;
  n = read(clFixnumInt(fd), buf, (size_t) n);
  /* What if read got only a partial character??? */
  if (n)
    { n /= 2;
      p = clSimpleStringWcharp(buffer) + clFixnumInt(offset);
      e = p + n;
      while (p < e)
	{ *p = (b[1] << CHAR_BIT) + b[0]; p++; b+=2; } }
  return(clIntFixnum(n)); }

clObject clWriteUcs2(fd, buffer, offset, size)				
     clObject fd, buffer, offset, size;						
{ static char buf[BUFSIZE]; char *b = buf; wchar_t *p, *e;
  int n = clFixnumInt(size) * 2;
  if (n > BUFSIZE) n = BUFSIZE;
  if (n)
    { p = clSimpleStringWcharp(buffer) + clFixnumInt(offset);
      e = p + n;
      while (p < e)
	{ b[1] = *p >> CHAR_BIT;
	  b[0] = *p & clBitMask(CHAR_BIT);
	  p++; b+=2; } 
      n = xwrite(clFixnumInt(fd), buf, (size_t) n) / 2; }
  return(clIntFixnum(n)); }

clObject clReadUcs4(fd, buffer, offset, size)				
     clObject fd, buffer, offset, size;						
{ static char buf[BUFSIZE]; char *b = buf; wchar_t *p, *e;
  int n = clFixnumInt(size) * 4;
  if (n > BUFSIZE) n = BUFSIZE;
  n = read(clFixnumInt(fd), buf, (size_t) n);
  /* What if read got only a partial character??? */
  if (n)
    { n /= 4;
      p = clSimpleStringWcharp(buffer) + clFixnumInt(offset);
      e = p + n;
      while (p < e)
	{ *p = (b[0] << (CHAR_BIT * 3)) +
	    (b[1] << (CHAR_BIT * 2)) +
	    (b[2] << CHAR_BIT) + b[3];
	  p++; b+=4; } }
  return(clIntFixnum(n)); }

clObject clWriteUcs4(fd, buffer, offset, size)				
     clObject fd, buffer, offset, size;						
{ static char buf[BUFSIZE]; char *b = buf; wchar_t *p, *e;
  int n = clFixnumInt(size) * 4;
  if (n > BUFSIZE) n = BUFSIZE;
  if (n)
    { p = clSimpleStringWcharp(buffer) + clFixnumInt(offset);
      e = p + n;
      while (p < e)
	{ b[3] = *p >> (CHAR_BIT*3);
	  b[2] = *p >> (CHAR_BIT*2);
	  b[1] = *p >> CHAR_BIT;
	  b[0] = *p & clBitMask(CHAR_BIT);
	  p++; b+=4; } 
      n = xwrite(clFixnumInt(fd), buf, (size_t) n) / 4; }
  return(clIntFixnum(n)); }

/* Unconverted characters are stored in aux, so that each stream can
   maintain its own shift state.
   Multi-byte (aka FSS-UTF or UTF-8) uses up to 6 bytes to represent
   each unicode character. */

clObject clReadMb(fd, buffer, offset, size, aux) 
     clObject fd, buffer, offset, size, aux;						
{ static char buf[BUFSIZE];
  char *bp=buf, *auxp = clSimpleBaseStringCharp(clSimpleArrayContents(aux));
  int bytes = clFixnumInt(size),
    old_bytes = clFixnumInt(clComplexArrayFillPointer(aux));
  if (bytes > BUFSIZE) bytes = BUFSIZE;
  bytes -= old_bytes;
  (void) memcpy(bp, auxp, (size_t) old_bytes);
  bytes = read(clFixnumInt(fd), bp+old_bytes, (size_t) bytes);
  if (bytes < 0) return(clIntFixnum(bytes));
  bytes += old_bytes;
  { wchar_t *ws =clSimpleStringWcharp(buffer) + clFixnumInt(offset),
	*wp = ws;
    char *be = bp + bytes;
    (void) mbtowc(wp, (char *)NULL, 0);
    while (bp < be)
      { int n = mbtowc(wp, bp, (size_t) (be - bp));
        if (!n) n = 1;
	if (n < 0)
	  { size_t left = (be - bp);
	    (void) memcpy(auxp, bp, left);
	    clSetq(clComplexArrayFillPointer(aux),
		   clIntFixnum(left));
	    break; }
	wp++; bp += n; }
    return(clIntFixnum(wp - ws)); } }

clObject clWriteMb(fd, buffer, offset, size) 
     clObject fd, buffer, offset, size;						
{ static char buf[BUFSIZE];
  int bi = 0;
  int wchars = clFixnumInt(size);
  wchar_t *ws = clSimpleStringWcharp(buffer) + clFixnumInt(offset),
    *we = ws + wchars, *wp = ws;
  (void) wctomb((char *) NULL, *wp);
  while (wp < we)
    { int next, n = wctomb(&buf[bi], *wp);
      if (!n) n = 1;
      if (n <= 0) return(clNIL); /* error */
      next = bi + n;
      if (next > (BUFSIZE-6)) break;
      bi = next; wp++; }
  (void) xwrite(clFixnumInt(fd), buf, (size_t) bi);
  return(clIntFixnum(wp - ws)); }

#define DummyWriter(name) \
clObject name __P((clObject, clObject, clObject, clObject)); \
clObject name(fd, buffer, offset, size) clObject fd, buffer, offset, size; { \
  return(_clControlError("Binary stream I/O is not defined yet.")); }

DummyWriter(clWriteLeu32)
DummyWriter(clWriteLeu16) 
DummyWriter(clWriteLeu8)
DummyWriter(clWriteLes32) 
DummyWriter(clWriteLes16) 
DummyWriter(clWriteLes8) 
DummyWriter(clWriteBeu32) 
DummyWriter(clWriteBeu16) 
DummyWriter(clWriteBeu8)
DummyWriter(clWriteBes32) 
DummyWriter(clWriteBes16) 
DummyWriter(clWriteBes8) 

DummyWriter(clReadLeu32)  
DummyWriter(clReadLeu16)  
DummyWriter(clReadLeu8)  
DummyWriter(clReadLes32)  
DummyWriter(clReadLes16)  
DummyWriter(clReadLes8) 
DummyWriter(clReadBeu32)  
DummyWriter(clReadBeu16)  
DummyWriter(clReadBeu8)  
DummyWriter(clReadBes32)  
DummyWriter(clReadBes16)  
DummyWriter(clReadBes8)  



/*********************************************************************
 * SLOTS
 *********************************************************************/
/* Note that returned memory is cleared.  As long as clUNBOUND is really
  0, we're ok.  Otherwise, we have to explicitly clear it ourselves. */
clObject clMakeSlots(size) clObject size; {
  int s = clFixnumInt(size);
  return(s ? clMakeClearHeapObject((m_size) s * sizeof(clObject)) : clUNBOUND);
}

/* Used on startup only */
clObject clMakeStaticSlots(clVaAlist) clVaDcl
{ va_list _ap; int count = 0, i; clObject slots; clSlots p;
  clVaStart(_ap);
#ifdef __STDC__
  if (_clVp(_ap)) { count++;
#else
  if (1) {
#endif
    while (_clPresentp(_clGetObjectArg(_ap))) count++;
  }
  clVaEnd(_ap);
  clSetq(slots, clMakeSlots(clIntFixnum(count)));
  p = clObjectSlots(slots);
  clVaStart(_ap);
  if (count) {
    i = 0;
#ifdef __STDC__
    clSetq(*p++, clCurrentArg(_ap)); i++;
#endif
    for (; i < count; i++, p++) clSetq(*p, _clGetObjectArg(_ap));
  }
  clVaEnd(_ap);
  return(slots);
}

/*********************************************************************
 * WRAPPER
 *********************************************************************/
clObject clMakeWrapper(obsolete_slots) clObject obsolete_slots;
{ long key = 0;
  clObject wrapper;
  clSetq(wrapper, clMakeHeapObject(sizeof(clWrapperCell)));
  while (key == 0) key = rand();
  clISetq(clWrapperHashKey(wrapper), clIntIndex(key));
  clSetq(clWrapperObsoleteSlots(wrapper), obsolete_slots);
  return(wrapper); }

/*********************************************************************
 * CLOSURES
 *********************************************************************/
static clBinding clNULL_ENV[1];

clObject 
#ifdef __STDC__
  clMakeClosureProto()
#else
  clMakeClosure(clVaAlist) clVaDcl 
#endif
{
  clObject closure; int i; clBinding *env; va_list ap;

#ifdef __STDC__
  va_start(ap, hook);
#else
  int n; clMFunction mfunc; clBinding **hook;
  clVaStart(ap);
  n = clGetIntArg(ap);
  mfunc = clGetMFunctionArg(ap);
  hook = clGetHookAddressArg(ap);
#endif

  if (n) {
    if (!(env = ((clBinding *) clMalloc((m_size) n *
					sizeof(clBinding)))))
      _clStorageCondition();
  } else 
    env = clNULL_ENV; 
  for (i=0; i<n; i++) env[i] = clGetBindingArg(ap);
  clVaEnd(ap);

  clSetq(closure, clMakeTaggedInstance(sizeof(clClosureCell), clBUILT_IN_FUNCTION));
  clClosureFunction(closure) = mfunc;
  clClosureHook(closure) = hook;
  *clClosureHook(closure) = clClosureEnvironment(closure) = env;
  clSetStandardInstanceSlots(closure, clUNBOUND);
  return(closure); } 

/* We rely here on closures besing considered immutable -- i.e. we
   don't have to worry about someone later changing the fields of function and
   expecting the results to be propogated to object. */
clObject clSetFuncallableStandardInstanceFunction(object, function)
     clObject object, function;
{ clClosureFunction(object) = clClosureFunction(function);
  clClosureHook(object) = clClosureHook(function);
  clClosureEnvironment(object) = clClosureEnvironment(function);
  return(object); }
			      

/*********************************************************************
 * HASH TABLE
 *********************************************************************/
#define empty_key 	0
#define removed_key 	-1
#define clEMPTY 	clFLAG0
#define clREMOVED 	clFLAG1
#define no_op 

clObject clStringHash(x) clObject x; {
  unsigned int key = 0Xaa, length = StringLength(x);
  char *bp; wchar_t *ep;
  if (StringData(x, &bp, &ep, 0))
    while (length--) key += *bp++;
  else
    while (length--) key += *ep++; 
  return(clIntFixnum(key));
}

int _clStringEq(x, y) clObject x, y; {
  unsigned int length = StringLength(x), x_basic_p;
  char *bx, *by; wchar_t *ex, *ey;
  if (length != StringLength(y)) return(0);
  x_basic_p = StringData(x, &bx, &ex, 0);
  if (StringData(y, &by, &ey, 0)) {
    if (x_basic_p) {
      while (length--) if (*bx++ != *by++) return(0);
    } else {
      while (length--) if (*ex++ != *by++) return(0);
    }
  } else {
    if (x_basic_p) {
      while (length--) if (*bx++ != *ey++) return(0);
    } else {
      while (length--) if (*ex++ != *ey++) return(0);
    }
  }
  return(1);
}
clObject clStringEq(x, y, eoa) clObject x, y, eoa; {
  clIgnore(eoa); return(clValues1(clTest(_clStringEq(x, y))));
}

#define clWithOpenAddressKey(hasher,test, table, index, key, keys, empty, removed, found) \
{ clSlots keys = clOpenAddressHashTableKeys(table);				\
  unsigned int n_buckets = clIndexInt(clOpenAddressHashTableNBuckets(table));	\
  unsigned int index = (unsigned) clFixnumInt(hasher(key, clEOA)) % n_buckets;	\
  while (1) {								\
    clObject this_key;							\
    clSetq(this_key, keys[index]);					\
    switch (clObjectWord(this_key)) {					\
    case empty_key: empty; break;					\
    case removed_key: removed; break;					\
    default: if (test(this_key, key)) {  found; }			\
    } index = (1 + index) % n_buckets; } }

#define clOpenAddressGethash(hasher,test, key, table, default)	\
  clWithOpenAddressKey(hasher,test, table, indexr, key, keys,	\
		       return(default),				\
		       no_op,					\
		       return(clOpenAddressHashTableValues(table)[indexr]))

#define clOpCount(table, op)	\
clOpenAddressHashTableCountSetter(clIntIndex(clIndexInt(clOpenAddressHashTableCount(table)) op 1),\
				  table)

#define clOpenAddressRemhash(hasher,test, key, table)		\
  clWithOpenAddressKey(hasher,test, table, indexr, key, keys,	\
		       return(clNIL),				\
		       no_op,					\
		       { clSetq(keys[indexr], clREMOVED);	\
		         clOpCount(table, -); return(clT); })

#define clOpenAddressRehash(table,setter)			\
  { clSlots rkeys = clOpenAddressHashTableKeys(table);		\
    clSlots rvalues = clOpenAddressHashTableValues(table);	\
    unsigned int ri, rn_buckets = clIndexInt(clOpenAddressHashTableNBuckets(table));	\
    clResizeHashTable(table, clEOA);				\
    for (ri = 0; ri < rn_buckets; ri++)				\
      { clObject rthis_key;					\
        clSetq(rthis_key, rkeys[ri]);				\
        switch (clObjectWord(rthis_key)) {			\
	case empty_key:						\
	case removed_key: break;					\
	default: setter(rthis_key, table, rvalues[ri]); } } }	

#define clSetVal(table,index,new_value)  \
  clSetq(clOpenAddressHashTableValues(table)[index], new_value)
#define clSetKey(keys,index,key,table)	\
  { clSetq(keys[index], key); clOpCount(table, +); }

#define clOpenAddressSethash(hasher,test, key, table, new_value,setter,rehash)	\
{ clSlots keys = clOpenAddressHashTableKeys(table);			\
  unsigned int n_buckets = clIndexInt(clOpenAddressHashTableNBuckets(table));	\
  unsigned int indexr = (unsigned) clFixnumInt(hasher(key, clEOA)) % n_buckets;	\
  while (1) {								\
    clObject this_key;							\
    clSetq(this_key, keys[indexr]);					\
    switch (clObjectWord(this_key)) {					\
    case empty_key:							\
      if (clIndexInt(clOpenAddressHashTableCount(table)) >= 		\
	  clIndexInt(clOpenAddressHashTableSize(table))) {		\
	rehash(table);							\
        return(setter(key, table, new_value)); 				\
      } else {								\
        clSetKey(keys,indexr,key,table); 				\
	return(clSetVal(table,indexr,new_value));			\
      }									\
    break;								\
    case removed_key:							\
      clSetKey(keys,indexr,key,table); 					\
      return(clSetVal(table,indexr,new_value));				\
      break;								\
    default:								\
      if (test(this_key, key)) return(clSetVal(table,indexr,new_value));	\
    } indexr = (1 + indexr) % n_buckets; } }

#define _clEql(x, y) clTrue(clEql(x, y, clEOA))
#define _clEqual(x, y) clTrue(clEqual(x, y, clEOA))
#define _clEqualp(x, y) clTrue(clEqualp(x, y, clEOA))
#define XEqHash(x, y) clEqHash(x)
#define XStringHash(x, y) clStringHash(x)

#define DefHash(name,hash,test)	\
clObject clPaste3(clOpenAddress,name,Gethash) __P((clObject, clObject, clObject));	\
clObject clPaste3(clOpenAddress,name,Sethash) __P((clObject, clObject, clObject));	\
clObject clPaste3(clOpenAddress,name,Remhash) __P((clObject, clObject));		\
void clPaste3(clOpenAddress,name,Rehash) __P((clObject));		\
	 								\
clObject clPaste3(clOpenAddress,name,Gethash)(key, table, default_val) 	\
  clObject key, table, default_val;					\
{ clOpenAddressGethash(hash,test, key, table, default_val) }		\
	 								\
clObject clPaste3(clOpenAddress,name,Sethash)(key, table, value) 	\
  clObject key, table, value;						\
{ clOpenAddressSethash(hash,test, key, table, 				\
       value,clPaste3(clOpenAddress,name,Sethash),clPaste3(clOpenAddress,name,Rehash)) } \
	 								\
void clPaste3(clOpenAddress,name,Rehash)(table) clObject table;		\
{ clOpenAddressRehash(table,clPaste3(clOpenAddress,name,Sethash)) }	\
	 								\
clObject clPaste3(clOpenAddress,name,Remhash)(key, table)		\
   clObject key, table; 						\
{ clOpenAddressRemhash(hash,test, key, table) }

DefHash(Eq,XEqHash,_clEq)
DefHash(Eql,clEqlHash,_clEql)
DefHash(Equal,clEqualHash,_clEqual)
DefHash(Equalp,clEqualpHash,_clEqualp)
DefHash(String,XStringHash,_clStringEq)

clObject clOpenAddressClrhash(table) clObject table;
{ clIndex n;
  clISetq(n, clOpenAddressHashTableNBuckets(table));
  clOpenAddressHashTableKeysSetter(clObjectSlots(clMakeSlots(clIndexFixnum(n))),
				   table);
  clOpenAddressHashTableValuesSetter(clObjectSlots(clMakeSlots(clIndexFixnum(n))),
				     table);
  clOpenAddressHashTableCountSetter(clIntIndex(0), table);
  return(table); }

clObject clMakeOpenAddressHashTable(class, size, rehash_size, n_buckets)
     clObject class, rehash_size; clIndex size, n_buckets;
{ clObject table;
  clSetq(table,
	 _clMakeTaggedInstance(sizeof(clOpenAddressHashTableCell),
			       class, clClassWrapper(class, clEOA)));
  clOpenAddressHashTableNBucketsSetter(n_buckets, table);
  clOpenAddressHashTableSizeSetter(size, table);
  clOpenAddressHashTableRehashSizeSetter(rehash_size, table);
  return(clOpenAddressClrhash(table)); }
    

clObject clOpenAddressMaphash(function, table)
     clObject function, table;
{
  unsigned int i = 0, end = clIndexInt(clOpenAddressHashTableNBuckets(table));
  clObject *keyp = clOpenAddressHashTableKeys(table);
  clObject *valuep = clOpenAddressHashTableValues(table);
  while (i < end) {
    clObject key; clSetq(key, keyp[i]);
    switch (clObjectWord(key)) {
    case empty_key: 
    case removed_key:
      break;
    default:
      clFuncall(function, key, valuep[i], clEOA);
    }
    i++;
  }
  return(clNIL);
}


clObject clFirstNonEmptyIndex(table, indexr) clObject table, indexr;
{ clSlots keys = clOpenAddressHashTableKeys(table);			
  unsigned int n_buckets = clIndexInt(clOpenAddressHashTableNBuckets(table));	
  unsigned int nindex;
  for (nindex = clFixnumInt(indexr); nindex < n_buckets; nindex++) {
    clObject this_key;
    clSetq(this_key, keys[nindex]);
    switch (clObjectWord(this_key)) {				
    case empty_key:
    case removed_key: break;						
    default: return(clIntFixnum(nindex));
    }
  }
  return(clIntFixnum(nindex));
}


clObject clHashTableKey(table, indexr) clObject table, indexr; {
  return(clOpenAddressHashTableKeys(table)[clFixnumInt(indexr)]);
}
clObject clHashTableValue(table, indexr) clObject table, indexr; {
  return(clOpenAddressHashTableValues(table)[clFixnumInt(indexr)]);
}

clObject clMakeEmfTable() {
  return(clMakeOpenAddressHashTable(clClassVar(clOPEN_ADDRESS_HASH_TABLE),
				    clIntIndex(40),
				    clIntFixnum(30),
				    clIntIndex(50)));
}

void clClearEmfTable(table) clObject table; { clOpenAddressClrhash(table); }


clObject _clEmfSetHasher(list, length) clObject list; int length; {
  unsigned int hh = 0; 
  while (length-- > 0) {
    hh += (unsigned) clIndexInt(clWrapperHashKey(clConsCar(list)));
    clSetq(list, clConsCdr(list));
  }
  return(clIntFixnum(hh));
}

int clEmfGetHasher(list, length) clObject list; unsigned int length; {
  unsigned int hh = 0; clObject wrapper;
  while (length-- > 0) {
    clSetq(wrapper, clObjectWrapper(clConsCar(list)));
    if (clTrue(clWrapperInvalidP(wrapper))) {
      (void) clUpdateInstancesIfNeeded(list, clEOA);
      return(0);
    }
    hh += (unsigned) clIndexInt(clWrapperHashKey(wrapper));
    clSetq(list, clConsCdr(list));
  }
  return(hh);
}

int _clEmfSetTest(this, keylist, length)
     clObject this, keylist; int length;
{
  while (length-- > 0) {
    clObject this_wrapper, key_wrapper;
    clSetq(this_wrapper, clConsCar(this));
    clSetq(key_wrapper, clConsCar(keylist));
    if (!_clEq(this_wrapper, key_wrapper)) return(0);
    clSetq(this, clConsCdr(this));
    clSetq(keylist, clConsCdr(keylist));
  }
  return(1);
}

int clEmfGetTest(this, keylist, length)
     clObject this, keylist; unsigned int length;
{
  while (length-- > 0) {
    clObject this_wrapper, key_wrapper;
    clSetq(this_wrapper, clConsCar(this));
    clSetq(key_wrapper, clObjectWrapper(clConsCar(keylist)));
    if (!_clEq(this_wrapper, key_wrapper)) return(0);
    clSetq(this, clConsCdr(this));
    clSetq(keylist, clConsCdr(keylist));
  }
  return(1);
}

#define clEmfSetTest(obj1, obj2) _clEmfSetTest(obj1, obj2, length)
#define clEmfSetHasher(obj, ignore) _clEmfSetHasher(obj, length)

void clEmfTableRehash(table) clObject table; {
  clOpenAddressRehash(table,clEmfTableSethash);
}

clObject clEmfTableSethash1(key, table, new_value, length)
     clObject key, table, new_value; int length;
{
  clOpenAddressSethash(clEmfSetHasher,clEmfSetTest, key, table,
		       new_value,clEmfTableSethash,clEmfTableRehash);
}


clObject clEmfTableSethash(key, table, new_value)
     clObject key, table, new_value;
{
  return(clEmfTableSethash1(key, table, new_value,
			     clFixnumInt(clLengthList(key, clEOA))));
}

clObject clEmfTableSet(table, length, classes, value)
     clObject table, length, classes, value;
{
  return(clEmfTableSethash1(clClassWrappers(classes, clEOA),
			    table, value, clFixnumInt(length))); }

clObject clEmfTableGet(table, length, args) clObject table, length, args;
/* IWBNI we used clWithOpenAddressKey1() */
{
  unsigned int ilength = clFixnumInt(length);
  clSlots keys = clOpenAddressHashTableKeys(table);			
  unsigned int n_buckets = clIndexInt(clOpenAddressHashTableNBuckets(table));	
  unsigned int indexr = clEmfGetHasher(args, ilength);
  clObject this_key;							
  if (indexr) indexr %= n_buckets; else return(clNIL);
repeat:
  clSetq(this_key, keys[indexr]);
  switch (clObjectWord(this_key)) {
  case empty_key: return(clNIL); break;
  case removed_key: break;
  default:
    if (clEmfGetTest(this_key, args, ilength))
      return(clOpenAddressHashTableValues(table)[indexr]);
  }
  indexr = (1 + indexr) % n_buckets;
  goto repeat;
}


/*********************************************************************
 * MATH
 *********************************************************************/
/* Much of this goes away when we get the compiler to be smarter about
   the types of arguments to functions. */

#define clIntegerFloat(x)	\
  (_clFixnump(x) ? ((float) clFixnumInt(x)) : \
   ((float) clDoubleFloatDouble(clXintDoubleFloat(clBignumXint(x), clEOA))))
#define clRatioFloat(x)	\
  (clIntegerFloat(clRatioNumerator(x)) / clIntegerFloat(clRatioDenominator(x)))

#define clIntegerDouble(x)	\
  (_clFixnump(x) ? ((double) clFixnumInt(x)) : \
   clDoubleFloatDouble(clXintDoubleFloat(clBignumXint(x), clEOA)))
#define clRatioDouble(x)	\
  (clIntegerDouble(clRatioNumerator(x)) / clIntegerDouble(clRatioDenominator(x)))

/* BOOLEAN */
clObject clFixnump(clVaAlist) clVaDcl {
  clObject obj; OneArg(obj);
  return(clValues1(clTest(_clFixnump(obj))));
}

#define IntegerCmp(name, op)					\
clObject name __P((clObject, clObject)); 			\
clObject name(x, y) clObject x, y;				\
{ return(clTest((_clFixnump(x) && _clFixnump(y)) ?		\
		(clFixnumInt(x) op clFixnumInt(y)) :		\
		(clFixnumInt(clCmpXint(clIntegerXint(x),	\
				       clIntegerXint(y), clEOA)) op 0))); }

IntegerCmp(clEqIntegerInteger, ==)
IntegerCmp(clLtIntegerInteger, <)
IntegerCmp(clGtIntegerInteger, >)
IntegerCmp(clLeIntegerInteger, <=)
IntegerCmp(clGeIntegerInteger, >=)

#define SingleFloatCmp(name,t1,t2,op)	\
clObject clPaste3(name,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(name,t1,t2)(f1, f2) clObject f1, f2;		\
{ return(clTest(clPaste3(cl,t1,Float)(f1) op clPaste3(cl,t2,Float)(f2))); }

SingleFloatCmp(clEq,Integer,SingleFloat,==)
SingleFloatCmp(clEq,SingleFloat,Integer,==)
SingleFloatCmp(clEq,SingleFloat,SingleFloat,==)
SingleFloatCmp(clLt,Integer,SingleFloat,<)
SingleFloatCmp(clLt,SingleFloat,Integer,<)
SingleFloatCmp(clLt,SingleFloat,SingleFloat,<)
SingleFloatCmp(clGt,Integer,SingleFloat,>)
SingleFloatCmp(clGt,SingleFloat,Integer,>)
SingleFloatCmp(clGt,SingleFloat,SingleFloat,>)
SingleFloatCmp(clLe,Integer,SingleFloat,<=)
SingleFloatCmp(clLe,SingleFloat,Integer,<=)
SingleFloatCmp(clLe,SingleFloat,SingleFloat,<=)
SingleFloatCmp(clGe,Integer,SingleFloat,>=)
SingleFloatCmp(clGe,SingleFloat,Integer,>=)
SingleFloatCmp(clGe,SingleFloat,SingleFloat,>=)

#define DoubleFloatCmp(name,t1,t2,op)	\
clObject clPaste3(name,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(name,t1,t2)(f1, f2) clObject f1, f2;		\
{ return(clTest(clPaste3(cl,t1,Double)(f1) op clPaste3(cl,t2,Double)(f2))); }

DoubleFloatCmp(clEq,Integer,DoubleFloat,==)
DoubleFloatCmp(clEq,DoubleFloat,Integer,==)
DoubleFloatCmp(clEq,SingleFloat,DoubleFloat,==)
DoubleFloatCmp(clEq,DoubleFloat,SingleFloat,==)
DoubleFloatCmp(clEq,DoubleFloat,DoubleFloat,==)
DoubleFloatCmp(clLt,Integer,DoubleFloat,<)
DoubleFloatCmp(clLt,DoubleFloat,Integer,<)
DoubleFloatCmp(clLt,SingleFloat,DoubleFloat,<)
DoubleFloatCmp(clLt,DoubleFloat,SingleFloat,<)
DoubleFloatCmp(clLt,DoubleFloat,DoubleFloat,<)
DoubleFloatCmp(clGt,Integer,DoubleFloat,>)
DoubleFloatCmp(clGt,DoubleFloat,Integer,>)
DoubleFloatCmp(clGt,SingleFloat,DoubleFloat,>)
DoubleFloatCmp(clGt,DoubleFloat,SingleFloat,>)
DoubleFloatCmp(clGt,DoubleFloat,DoubleFloat,>)
DoubleFloatCmp(clLe,Integer,DoubleFloat,<=)
DoubleFloatCmp(clLe,DoubleFloat,Integer,<=)
DoubleFloatCmp(clLe,SingleFloat,DoubleFloat,<=)
DoubleFloatCmp(clLe,DoubleFloat,SingleFloat,<=)
DoubleFloatCmp(clLe,DoubleFloat,DoubleFloat,<=)
DoubleFloatCmp(clGe,Integer,DoubleFloat,>=)
DoubleFloatCmp(clGe,DoubleFloat,Integer,>=)
DoubleFloatCmp(clGe,SingleFloat,DoubleFloat,>=)
DoubleFloatCmp(clGe,DoubleFloat,SingleFloat,>=)
DoubleFloatCmp(clGe,DoubleFloat,DoubleFloat,>=)


/* INDEX */
clIndex clIntegerIndex(x) clObject x;
{ int i;
  if (!_clFixnump(x)) (void) _clTypeError(x, "FIXNUM");
  i = clFixnumInt(x);
  if (i < 0) (void) _clTypeError(x, "UNSIGNED");
  return(clIntIndex(i)); }

/* FIXNUM */
clObject clAshFixnum(f1, f2) clObject f1, f2; {
  int j, i = clFixnumInt(f1), n = clFixnumInt(f2);  
  if (0 <= n) j = (i << n);
  else j = (i >> -n);
  return(clIntFixnum(j));
}

clObject clLogcountFixnum(f) clObject f; {
  int count = 0; unsigned int l = (unsigned) clFixnumInt(f);
  while (l)
    {l &= l-1;			/* deletes rightmost 1-bit */
     count++;}
  return(clIntFixnum(count));
}

#define Signp(l) 		( (l) & ~( (~ ((unsigned int) 0) ) >> 1) )

clObject clLengthFixnum(f) clObject f; {
  int l = clFixnumInt(f), count = (sizeof(int) * CHAR_BIT); 
  if (Signp(l)) l = ~l;
  if (!l) return(clIntFixnum(0));
  while (!Signp(l)) {count--; l <<= 1;}
  return(clIntFixnum(count));
}

clObject clDigitHigh(i) clObject i; {
  return(clIntFixnum(((unsigned) clIntegerInt(i)) >> clDigitBits));
}
clObject clDigitLow(i) clObject i; {
  return(clIntFixnum(((unsigned) clIntegerInt(i)) & clBitMask(clDigitBits)));
}



/* XINT */
clObject clTrimXint(xint, size) clObject xint, size; {
  clISetq(clVectorSize(xint),
	 clIntIndex(clFixnumInt(size) * clDigitBits));  
  return(xint);
}

clObject clMakeXint(size) clObject size; {
  return(clTrimXint(clMakeTaggedInstance(clVectorMSize(clSimpleBitVectorCell,
						       clDigit, clFixnumInt(size)),
					 clINTEGER),
		    size));
}
  
clObject clNloadXint(xint, val) clObject xint; int val; {
  clSetIndexref(xint, clIntIndex(0),
		clIntIndex(val & clBitMask(clDigitBits))); 
  clSetIndexref(xint, clIntIndex(1),
		clIntIndex(((unsigned) val) >> clDigitBits));
  return(xint);
}

clObject clIntegerXint(integer) clObject integer; {
  if (_clFixnump(integer))
    return(clNloadXint(clMakeXint(clIntFixnum(2)),
		       clFixnumInt(integer)));
  else
    return(clBignumXint(integer));
}

/* INTEGER */
#define most_positive_int clBitMask((sizeof(int)*CHAR_BIT)-1)
#define most_negative_int 	(~most_positive_int)
#define most_positive_fixnum clBitMask((sizeof(int)*CHAR_BIT)-3)
#define most_negative_fixnum 	(~most_positive_fixnum)
#define most_positive_digit 	clBitMask(clDigitBits)
#define most_negative_digit 	(~most_positive_digit)
#define between(low, x, high)	((low <= x) && (x <= high))

static clObject clBigIntInteger __P((int, int));
static clObject clBigIntInteger(i, minusp) int i, minusp; {
  clObject x;
  clSetq(x, clNloadXint(clMakeXint(clIntFixnum(3)), i));
  clSetDigitref(x, clIntIndex(2), clIntFixnum(minusp ? -1 : 0));
  return(x);
}

clObject clIntInteger(i) int i; {
  if (between(most_negative_fixnum, i, most_positive_fixnum))
    return(clIntFixnum(i));
  {
    int minusp = i < 0, signp = Signp(i);
    if ((minusp && !signp) ||
	(!minusp && signp))
      return(clBigIntInteger(i, minusp));
  }
  return(clNloadXint(clMakeXint(clIntFixnum(2)), i));
}

clObject clAddIntegerInteger(x, y) clObject x, y; {
  return((_clFixnump(x) && _clFixnump(y)) ?
	 clIntInteger(clFixnumInt(x) + clFixnumInt(y)) :
	 clXintInteger(clAddXint(clIntegerXint(x),
				 clIntegerXint(y), clEOA), clEOA));
}

clObject clSubtIntegerInteger(x, y) clObject x, y; {
  return((_clFixnump(x) && _clFixnump(y)) ?
	 clIntInteger(clFixnumInt(x) - clFixnumInt(y)) :
	 clXintInteger(clSubtXint(clIntegerXint(x),
				  clIntegerXint(y), clEOA), clEOA));
}

clObject clMultIntegerInteger(x, y) clObject x, y; {
  if (_clFixnump(x) && _clFixnump(y)) {
    int xx = clFixnumInt(x), yy = clFixnumInt(y);
    if (between(most_negative_digit, xx, most_positive_digit) &&
	between(most_negative_digit, yy, most_positive_digit)) {
      int zz = xx * yy;
      if (zz) {
	int sz = zz < 0, sxy = (xx < 0) ^ (yy < 0);
	if (sxy ^ sz) return(clBigIntInteger(zz, sxy));
      }
      return(clIntInteger(zz));
    }
  }
  return(clXintInteger(clMultXint(clIntegerXint(x),
				  clIntegerXint(y), clEOA), clEOA));
}

clObject clTruncateSingleFloat(x) clObject x; {
  double xx = clSingleFloatDouble(x);
  if (between(most_negative_int, xx, most_positive_int))
    return(clIntInteger((int) xx));
  return(clXintBignum(clDoubleFloatXint(clDoubleDoubleFloat(xx), clEOA)));
}

clObject clTruncateDoubleFloat(x) clObject x; {
  double xx = clDoubleFloatDouble(x);
  if (between(most_negative_int, xx, most_positive_int))
    return(clIntInteger((int) xx));
  return(clXintBignum(clDoubleFloatXint(x, clEOA)));
}

/* RATIO */
clObject clMakeRatio(n, d) clObject n, d; {
  clObject obj;
  clSetq(obj, clMakeTaggedInstance(sizeof(clRatioCell), clRATIO));
  clSetq(clRatioNumerator(obj), n);
  clSetq(clRatioDenominator(obj), d);
  return(obj);
}


/* FLOAT */

/* We want to use scalbn() because it handles non-radix-2 machines,
   but in practice, there aren't any. */
#if !(defined sun)
#  define scalbn(x, n)	ldexp(x, n)
#endif

#if !(defined SunOS4)
#  define signbit(x) ((x) < 0.0 )
   /* modf() could also be used. */
   double aint __P((double));
   double aint (x) double x; { return ( signbit(x) ? ceil(x) : floor(x) ); }
#endif

/* Used for defining round(), below.
   Note that tru might be too big to be an int. */
#if !(defined sun)
double remainder __P((double, double));
double remainder(x, y) double x, y; {
  double thresh = fabs(y)/2.0,
    rem = fmod(x, y),
    tru = ((x-rem)/y),
    tru2 = fmod(tru, 2.0),
    atru2 = fabs(tru2);
  int oddp = between(0.5, atru2, 1.5);
  if ((rem > thresh) ||
      ((rem==thresh) && oddp)) 
    return((y < 0) ? (rem + y) : (rem - y));
  thresh = 0 - thresh;
  if ((rem < thresh) ||
      ((rem==thresh) && oddp))
    return((y < 0) ? (rem - y) : (rem + y));
  return(rem);
}
#endif

/* Systems with weak math libraries (i.e. only posix/iso-c) */
#if (defined hpux)
double copysign(x, y) double x, y; {
  if (signbit(y)) {
    if (signbit(x)) return x;
    else return -x;
  } else {
    if (signbit(x)) return -x;
    else return x;
  }
}
#endif 

#if (defined hpux) || (defined _WIN32)
/* IWBNI these produced more accurate results. */
double asinh(x) double x;
{ return(log(x + sqrt(1+(x*x)))); }
double acosh(x) double x;
{ return(log(x + (x+1)*sqrt((x-1)/(x+1)))); }
double atanh(x) double x;
{ return((log(1+x) - log(1-x)) / 2); }
#endif /* weak math libraries */

/* It would be nice if we could rely on rint(), instead of defining
   our own round(), but other code might muck with the "current IEEE
   rounding direction" in a non-ANSI-CL compatible way.  Also, many
   systems don't even have rint(). */
#define round eround  /* avoid system defined round  */
static double round __P((double));
static double round(x) double x; { return(x - remainder(x, 1.0)); }

/* SINGLE-FLOAT */
/* This and clSingleFloatDouble() seem silly, but support traditional
   C library implementations in which all operations are on
   doubles. */
clObject clDoubleSingleFloat(f) double f;
{ clObject obj;
  clSetq(obj, clMakeTaggedInstance(sizeof(clSingleFloatCell), clSINGLE_FLOAT));
  clSingleFloatFloat(obj) = (float) f;
  return(obj); }

/* This is really what we want, but there is no way to have it work in
   both traditional and ANSI C, unless separate libraries are used. */
clObject clFloatSingleFloat(f) float f;
{ clObject obj;
  clSetq(obj, clMakeTaggedInstance(sizeof(clSingleFloatCell), clSINGLE_FLOAT));
  clSingleFloatFloat(obj) = f;
  return(obj); }

#define SingleFloatOp(name,t1,t2,op)	\
clObject clPaste3(name,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(name,t1,t2)(f1, f2) clObject f1, f2;		\
{ return(clDoubleSingleFloat(clPaste3(cl,t1,Double)(f1) op clPaste3(cl,t2,Double)(f2))); }

SingleFloatOp(clAdd,SingleFloat,SingleFloat,+)
SingleFloatOp(clSubt,SingleFloat,SingleFloat,-)
SingleFloatOp(clMult,SingleFloat,SingleFloat,*)
SingleFloatOp(clDiv,SingleFloat,SingleFloat,/)

SingleFloatOp(clAdd,Integer,SingleFloat,+)
SingleFloatOp(clSubt,Integer,SingleFloat,-)
SingleFloatOp(clMult,Integer,SingleFloat,*)
SingleFloatOp(clDiv,Integer,SingleFloat,/)

SingleFloatOp(clAdd,SingleFloat,Integer,+)
SingleFloatOp(clSubt,SingleFloat,Integer,-)
SingleFloatOp(clMult,SingleFloat,Integer,*)
SingleFloatOp(clDiv,SingleFloat,Integer,/)

SingleFloatOp(clAdd,Ratio,SingleFloat,+)
SingleFloatOp(clSubt,Ratio,SingleFloat,-)
SingleFloatOp(clMult,Ratio,SingleFloat,*)
SingleFloatOp(clDiv,Ratio,SingleFloat,/)

SingleFloatOp(clAdd,SingleFloat,Ratio,+)
SingleFloatOp(clSubt,SingleFloat,Ratio,-)
SingleFloatOp(clMult,SingleFloat,Ratio,*)
SingleFloatOp(clDiv,SingleFloat,Ratio,/)

#define SingleFloatTrig(name,t1,op)	\
clObject clPaste2(name,t1) __P((clObject));	\
clObject clPaste2(name,t1)(f1) clObject f1;	\
{ return(clDoubleSingleFloat(op(clPaste3(cl,t1,Double)(f1)))); }

SingleFloatTrig(clExp,Integer,exp)
SingleFloatTrig(clSqrt1,Integer,sqrt)
SingleFloatTrig(clLog,Integer,log)
SingleFloatTrig(clSin,Integer,sin)
SingleFloatTrig(clAsin1,Integer,asin)
SingleFloatTrig(clSinh,Integer,sinh)
SingleFloatTrig(clAsinh,Integer,asinh)
SingleFloatTrig(clCos,Integer,cos)
SingleFloatTrig(clAcos1,Integer,acos)
SingleFloatTrig(clCosh,Integer,cosh)
SingleFloatTrig(clAcosh1,Integer,acosh)
SingleFloatTrig(clTan,Integer,tan)
SingleFloatTrig(clAtan1,Integer,atan)
SingleFloatTrig(clTanh,Integer,tanh)
SingleFloatTrig(clAtanh1,Integer,atanh)

SingleFloatTrig(clExp,Ratio,exp)
SingleFloatTrig(clSqrt1,Ratio,sqrt)
SingleFloatTrig(clLog,Ratio,log)
SingleFloatTrig(clSin,Ratio,sin)
SingleFloatTrig(clAsin1,Ratio,asin)
SingleFloatTrig(clSinh,Ratio,sinh)
SingleFloatTrig(clAsinh,Ratio,asinh)
SingleFloatTrig(clCos,Ratio,cos)
SingleFloatTrig(clAcos1,Ratio,acos)
SingleFloatTrig(clCosh,Ratio,cosh)
SingleFloatTrig(clAcosh1,Ratio,acosh)
SingleFloatTrig(clTan,Ratio,tan)
SingleFloatTrig(clAtan1,Ratio,atan)
SingleFloatTrig(clTanh,Ratio,tanh)
SingleFloatTrig(clAtanh1,Ratio,atanh)

SingleFloatTrig(clExp,SingleFloat,exp)
SingleFloatTrig(clSqrt1,SingleFloat,sqrt)
SingleFloatTrig(clLog,SingleFloat,log)
SingleFloatTrig(clSin,SingleFloat,sin)
SingleFloatTrig(clAsin1,SingleFloat,asin)
SingleFloatTrig(clSinh,SingleFloat,sinh)
SingleFloatTrig(clAsinh,SingleFloat,asinh)
SingleFloatTrig(clCos,SingleFloat,cos)
SingleFloatTrig(clAcos1,SingleFloat,acos)
SingleFloatTrig(clCosh,SingleFloat,cosh)
SingleFloatTrig(clAcosh1,SingleFloat,acosh)
SingleFloatTrig(clTan,SingleFloat,tan)
SingleFloatTrig(clAtan1,SingleFloat,atan)
SingleFloatTrig(clTanh,SingleFloat,tanh)
SingleFloatTrig(clAtanh1,SingleFloat,atanh)

SingleFloatTrig(clFceiling,SingleFloat,ceil)
SingleFloatTrig(clFfloor,SingleFloat,floor)
SingleFloatTrig(clFtruncate,SingleFloat,aint)
SingleFloatTrig(clFround,SingleFloat,round)

#define SingleFloatAtan2(t1,t2)	\
clObject clPaste3(clAtan2,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(clAtan2,t1,t2)(f1,f2) clObject f1, f2;	\
{ return(clDoubleSingleFloat(atan2(clPaste3(cl,t1,Double)(f1),	\
				   clPaste3(cl,t2,Double)(f2)))); }

SingleFloatAtan2(Integer,Integer)
SingleFloatAtan2(Integer,Ratio)
SingleFloatAtan2(Integer,SingleFloat)
SingleFloatAtan2(Ratio,Integer)
SingleFloatAtan2(Ratio,Ratio)
SingleFloatAtan2(Ratio,SingleFloat)
SingleFloatAtan2(SingleFloat,Integer)
SingleFloatAtan2(SingleFloat,Ratio)
SingleFloatAtan2(SingleFloat,SingleFloat)

#define SingleFloatExpt2(t1,t2)	\
clObject clPaste3(clExpt2,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(clExpt2,t1,t2)(f1,f2) clObject f1, f2;	\
{ return(clDoubleSingleFloat(pow(clPaste3(cl,t1,Double)(f1),	\
				 clPaste3(cl,t2,Double)(f2)))); }

SingleFloatExpt2(Integer,SingleFloat)
SingleFloatExpt2(Ratio,SingleFloat)
SingleFloatExpt2(SingleFloat,SingleFloat)

SingleFloatExpt2(Integer,Ratio)
SingleFloatExpt2(Ratio,Ratio)
SingleFloatExpt2(SingleFloat,Ratio)

clObject clFloatSign2SingleFloatSingleFloat(f1, f2) clObject f1, f2;
{ return(clDoubleSingleFloat(copysign(clSingleFloatDouble(f2),
				      clSingleFloatDouble(f1)))); }

clObject clFloatSign2DoubleFloatSingleFloat(f1, f2) clObject f1, f2;
{ return(clDoubleSingleFloat(copysign(clSingleFloatDouble(f2),
				      clDoubleFloatDouble(f1)))); }

clObject clScaleFloatSingleFloatInteger(ff, ii) clObject ff, ii;
{ return(clDoubleSingleFloat(scalbn(clSingleFloatDouble(ff),
				    (int) clIntegerInt(ii)))); }

/* DOUBLE-FLOAT */
clObject clDoubleDoubleFloat(f) double f;
{ clObject obj;
#if (defined cl_WORD_ALIGNED)
  clSetq(obj, clMakeTaggedInstance(sizeof(clDoubleFloatCell), clDOUBLE_FLOAT));
#else
  clSetq(obj, clMakeHeapObject(sizeof(clDoubleFloatCell)));
  clSet3BitTag(cl_DOUBLE_FLOAT_TAG, obj);
#endif
  clDoubleFloatDouble(obj) = f;
  return(obj); }

#define DoubleFloatOp(name,t1,t2,op)	\
clObject clPaste3(name,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(name,t1,t2)(f1, f2) clObject f1, f2;		\
{ return(clDoubleDoubleFloat(clPaste3(cl,t1,Double)(f1) op clPaste3(cl,t2,Double)(f2))); }

DoubleFloatOp(clAdd,DoubleFloat,DoubleFloat,+)
DoubleFloatOp(clSubt,DoubleFloat,DoubleFloat,-)
DoubleFloatOp(clMult,DoubleFloat,DoubleFloat,*)
DoubleFloatOp(clDiv,DoubleFloat,DoubleFloat,/)

DoubleFloatOp(clAdd,Integer,DoubleFloat,+)
DoubleFloatOp(clSubt,Integer,DoubleFloat,-)
DoubleFloatOp(clMult,Integer,DoubleFloat,*)
DoubleFloatOp(clDiv,Integer,DoubleFloat,/)

DoubleFloatOp(clAdd,DoubleFloat,Integer,+)
DoubleFloatOp(clSubt,DoubleFloat,Integer,-)
DoubleFloatOp(clMult,DoubleFloat,Integer,*)
DoubleFloatOp(clDiv,DoubleFloat,Integer,/)

DoubleFloatOp(clAdd,Ratio,DoubleFloat,+)
DoubleFloatOp(clSubt,Ratio,DoubleFloat,-)
DoubleFloatOp(clMult,Ratio,DoubleFloat,*)
DoubleFloatOp(clDiv,Ratio,DoubleFloat,/)

DoubleFloatOp(clAdd,DoubleFloat,Ratio,+)
DoubleFloatOp(clSubt,DoubleFloat,Ratio,-)
DoubleFloatOp(clMult,DoubleFloat,Ratio,*)
DoubleFloatOp(clDiv,DoubleFloat,Ratio,/)

DoubleFloatOp(clAdd,DoubleFloat,SingleFloat,+)
DoubleFloatOp(clSubt,DoubleFloat,SingleFloat,-)
DoubleFloatOp(clMult,DoubleFloat,SingleFloat,*)
DoubleFloatOp(clDiv,DoubleFloat,SingleFloat,/)

DoubleFloatOp(clAdd,SingleFloat,DoubleFloat,+)
DoubleFloatOp(clSubt,SingleFloat,DoubleFloat,-)
DoubleFloatOp(clMult,SingleFloat,DoubleFloat,*)
DoubleFloatOp(clDiv,SingleFloat,DoubleFloat,/)


#define DoubleFloatTrig(name,t1,op)	\
clObject clPaste2(name,t1) __P((clObject));	\
clObject clPaste2(name,t1)(f1) clObject f1;		\
{ return(clDoubleDoubleFloat(op(clPaste3(cl,t1,Double)(f1)))); }

DoubleFloatTrig(clExp,DoubleFloat,exp)
DoubleFloatTrig(clSqrt1,DoubleFloat,sqrt)
DoubleFloatTrig(clLog,DoubleFloat,log)
DoubleFloatTrig(clSin,DoubleFloat,sin)
DoubleFloatTrig(clAsin1,DoubleFloat,asin)
DoubleFloatTrig(clSinh,DoubleFloat,sinh)
DoubleFloatTrig(clAsinh,DoubleFloat,asinh)
DoubleFloatTrig(clCos,DoubleFloat,cos)
DoubleFloatTrig(clAcos1,DoubleFloat,acos)
DoubleFloatTrig(clCosh,DoubleFloat,cosh)
DoubleFloatTrig(clAcosh1,DoubleFloat,acosh)
DoubleFloatTrig(clTan,DoubleFloat,tan)
DoubleFloatTrig(clAtan1,DoubleFloat,atan)
DoubleFloatTrig(clTanh,DoubleFloat,tanh)
DoubleFloatTrig(clAtanh1,DoubleFloat,atanh)

DoubleFloatTrig(clFceiling,DoubleFloat,ceil)
DoubleFloatTrig(clFfloor,DoubleFloat,floor)
DoubleFloatTrig(clFtruncate,DoubleFloat,aint)
DoubleFloatTrig(clFround,DoubleFloat,round)

#define DoubleFloatAtan2(t1,t2)	\
clObject clPaste3(clAtan2,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(clAtan2,t1,t2)(f1,f2) clObject f1, f2;	\
{ return(clDoubleDoubleFloat(atan2(clPaste3(cl,t1,Double)(f1),	\
				   clPaste3(cl,t2,Double)(f2)))); }

DoubleFloatAtan2(Integer,DoubleFloat)
DoubleFloatAtan2(Ratio,DoubleFloat)
DoubleFloatAtan2(SingleFloat,DoubleFloat)
DoubleFloatAtan2(DoubleFloat,Integer)
DoubleFloatAtan2(DoubleFloat,Ratio)
DoubleFloatAtan2(DoubleFloat,SingleFloat)
DoubleFloatAtan2(DoubleFloat,DoubleFloat)


#define DoubleFloatExpt2(t1,t2)	\
clObject clPaste3(clExpt2,t1,t2) __P((clObject, clObject));	\
clObject clPaste3(clExpt2,t1,t2)(f1,f2) clObject f1, f2;	\
{ return(clDoubleDoubleFloat(pow(clPaste3(cl,t1,Double)(f1),	\
				 clPaste3(cl,t2,Double)(f2)))); }

DoubleFloatExpt2(Integer,DoubleFloat)
DoubleFloatExpt2(Ratio,DoubleFloat)
DoubleFloatExpt2(SingleFloat,DoubleFloat)
DoubleFloatExpt2(DoubleFloat,DoubleFloat)

DoubleFloatExpt2(DoubleFloat,Ratio)
DoubleFloatExpt2(DoubleFloat,SingleFloat)


/* N.B. Order is refersed between float-sign and copysign. */
clObject clFloatSign2SingleFloatDoubleFloat(f1, f2) clObject f1, f2;
{ return(clDoubleDoubleFloat(copysign(clDoubleFloatDouble(f2),
				      clSingleFloatDouble(f1)))); }

clObject clFloatSign2DoubleFloatDoubleFloat(f1, f2) clObject f1, f2;
{ return(clDoubleDoubleFloat(copysign(clDoubleFloatDouble(f2),
				      clDoubleFloatDouble(f1)))); }

clObject clScaleFloatDoubleFloatInteger(ff, ii) clObject ff, ii;
{ return(clDoubleDoubleFloat(scalbn(clDoubleFloatDouble(ff),
				    (int) clIntegerInt(ii)))); }


/* The next two groups (float2 stuff) should not have been defined
   this way:
   - Should be c:function/c:macro, not take eoa, and not use clValues1().
   - Should not take second argument either.
   However, it probably doesn't pay to clean it up now, as this will
   change when the compiler gets smarter anyway.
   The same applies to SingleFloatFloater. */
#define DoubleFloatFloater(name,t1)	\
clObject clPaste3(name,t1,DoubleFloat) __P((clObject, clObject, clObject));	\
clObject clPaste3(name,t1,DoubleFloat)(f1, f2, eoa) clObject f1, f2, eoa;	\
{ clIgnore(f2); clIgnore(eoa);					\
  return(clValues1(clDoubleDoubleFloat(clPaste3(cl,t1,Double)(f1)))); }

DoubleFloatFloater(clFloat2,Integer)
DoubleFloatFloater(clFloat2,Ratio)
DoubleFloatFloater(clFloat2,SingleFloat)

#define SingleFloatFloater(name,t1)	\
clObject clPaste3(name,t1,SingleFloat) __P((clObject, clObject, clObject));	\
clObject clPaste3(name,t1,SingleFloat)(f1, f2, eoa) clObject f1, f2, eoa;	\
{ clIgnore(f2); clIgnore(eoa);					\
  return(clValues1(clDoubleSingleFloat(clPaste3(cl,t1,Double)(f1)))); }
SingleFloatFloater(clFloat2,Integer)
SingleFloatFloater(clFloat2,Ratio)
SingleFloatFloater(clFloat2,DoubleFloat)

/* COMPLEX */
clObject clMakeComplex(r, i) clObject r, i;
{ clObject obj;
  clSetq(obj, clMakeTaggedInstance(sizeof(clComplexCell), clCOMPLEX));
  clSetq(clComplexRealpart(obj), r);
  clSetq(clComplexImagpart(obj), i);
  return(obj); }

/* DECODING */
/* These should use globals for sign instead of consing up new ones.*/
clObject clDecodeDoubleFloat(clVaAlist) clVaDcl {
  int expt; double f, s, sign = 1.0; 
  clObject double_float; OneArg(double_float);
  f = clDoubleFloatDouble(double_float);
  s = frexp(f, &expt);
  if (signbit(s)) sign = -1.0;
  s = fabs(s);
  return(clValues(clDoubleDoubleFloat(s),
		  clIntFixnum(expt),
		  clDoubleDoubleFloat(sign),
		  clEOA));
}
clObject clDecodeSingleFloat(clVaAlist) clVaDcl {
  int expt; double f, s, sign = 1.0;
  clObject single_float; OneArg(single_float);
  f = clSingleFloatDouble(single_float);
  s = frexp(f, &expt); 
  if (signbit(s)) sign = -1.0;
  s = fabs(s);
  return(clValues(clDoubleSingleFloat(s),
		  clIntFixnum(expt),
		  clDoubleSingleFloat(sign),
		  clEOA));
}


/*********************************************************************
 * SYMBOL
 *********************************************************************/
void clClearSymbol(symbol, name) clObject symbol, name;
{ clSetq(_clSymbolName(symbol), name);
  clSetq(_clSymbolPackage(symbol), clNIL);
  clSetq(_clSymbolPlist(symbol), clNIL);
  clSetq(_clSymbolValue(symbol), clUNBOUND);
  clSetq(_clSymbolFunction(symbol), clNIL);
  clSetq(_clSymbolSetfFunction(symbol), clNIL); }

clObject clMakeSymbol(clVaAlist) clVaDcl {
  clObject obj, name; OneArg(name);
  if (!_clStringp(name)) (void) _clTypeError(name, "STRING");
  clSetq(obj, clMakeTaggedInstance(sizeof(clSymbolCell), clSYMBOL));
  clClearSymbol(obj, name);
  return(clValues1(obj));
}

clObject clMakeKeyword(clVaAlist) clVaDcl {
  clObject key, name; OneArg(name);
  if (_clSymbolp(name)) clSetq(name, _clSymbolName(name));
  if (!_clStringp(name)) (void) _clTypeError(name, "STRING");
  clSetq(key, clOpenAddressStringGethash(name, KEY_TABLE, clNIL));
  if (_clNull(key)) {
    clSetq(key, clMakeSymbol(name, clEOA));
    clOpenAddressStringSethash(name, KEY_TABLE, key);
    clSetq(_clSymbolValue(key), key);
    clSetq(_clSymbolPackage(key), PKG_KEYWORD);
  }
  return(clValues1(key));
}

clObject clSymbolName(clVaAlist) clVaDcl {
  clObject symbol; OneArg(symbol);
  if (!_clSymbolp(symbol)) (void) _clTypeError(symbol, "SYMBOL");
  return(clValues1(_clSymbolName(symbol)));
}

clObject clSymbolPlist(clVaAlist) clVaDcl {
  clObject symbol; OneArg(symbol);
  if (!_clSymbolp(symbol)) (void) _clTypeError(symbol, "SYMBOL");
  return(clValues1(_clSymbolPlist(symbol)));
}

clObject clSetSymbolPlist(symbol, list) clObject symbol, list; {
  if (!_clSymbolp(symbol)) (void) _clTypeError(symbol, "SYMBOL");
  if (!_clListp(list)) (void) _clTypeError(list, "LIST");
  return(clSetq(_clSymbolPlist(symbol), list));
}

clObject clSystemProperty(symbol, indicator, default_val)
     clObject symbol, indicator, default_val;
{
  return(clGetf(clGet(symbol, clSYSTEM_PROPERTY, clEOA),
		indicator, default_val, clEOA));
}

clObject clSystemPropertySetter(symbol, indicator, value)
     clObject symbol, indicator, value;
{
  clObject plist, system_properties;
  clObject new_properties, new_plist;
  clSetq(plist, _clSymbolPlist(symbol));
  clSetq(system_properties, clGetf(plist, clSYSTEM_PROPERTY, clEOA));
  clSetq(new_properties, clPutf(system_properties,
		       indicator,
		       value, 
		       clEOA));
  clSetq(new_plist, clPutf(plist,
		clSYSTEM_PROPERTY,
		new_properties, 
		clEOA));
  clSetq(_clSymbolPlist(symbol),
	 new_plist);
  return(clValues1(value)); 
}

/*********************************************************************
 * CONS
 *********************************************************************/
clObject clCons(clVaAlist) clVaDcl {
  clObject obj, car, cdr; _TwoArgs(car, cdr);
#if (defined cl_WORD_ALIGNED)
  clSetq(obj, clMakeTaggedInstance(sizeof(clConsCell), clCONS));
#else
  clSetq(obj, clMakeHeapObject(sizeof(clConsCell)));
  clSet3BitTag(cl_CONS_TAG, obj);
#endif
  clSetq(clConsCar(obj), car);
  clSetq(clConsCdr(obj), cdr);
  return(clValues1(obj));
}

clObject clCar(clVaAlist) clVaDcl {
  clObject class, cons; OneArg(cons);
  clSetq(class, clObjectClass(cons));
  if (_clEq(class, clClassVar(clCONS))) return(clValues1(clConsCar(cons)));
  if (_clEq(class, clClassVar(clNULL))) return(clValues1(cons));
  return(_clTypeError(cons, "LIST"));
}

clObject clCdr(clVaAlist) clVaDcl {
  clObject class, cons; OneArg(cons);
  clSetq(class, clObjectClass(cons));
  if (_clEq(class, clClassVar(clCONS))) return(clValues1(clConsCdr(cons)));
  if (_clEq(class, clClassVar(clNULL))) return(clValues1(cons));
  return(_clTypeError(cons, "LIST"));
}

clObject clRplaca(clVaAlist) clVaDcl {
  clObject class, cons, car; TwoArgs(cons, car);
  clSetq(class, clObjectClass(cons));
  if (_clEq(class, clClassVar(clCONS)))
    clSetq(clConsCar(cons), car);
  else
    (void) _clTypeError(cons, "CONS");
  return(clValues1(cons));
}

clObject clRplacd(clVaAlist) clVaDcl {
  clObject class, cons, cdr; _TwoArgs(cons, cdr);
  clSetq(class, clObjectClass(cons));
  if (_clEq(class, clClassVar(clCONS)))
    clSetq(clConsCdr(cons), cdr);
  else
    (void) _clTypeError(cons, "CONS");
  return(clValues1(cons));
}

/* If we ever change &rest args to not cons up a new list (i.e. to
   share structure with the last argument to APPLY), then we need to
   make the following additional changes:
   1. Modify handle-logical-block in ~@<...~:> code to bypass
   circularity detection at top level.
*/

clObject _clVlist(last, ap) clObject last; va_list *ap; {
  clObject x, new_cons, last_cons, result;
  /* parse-lambda sometimes calls clVargs() unequivocally to get an
     &rest arg.  If we decide to explicitly test (with clVp()) in the
     generated code before calling clVars(), then we won't need this
     next line. */
  if (!_clPresentp(last)) return(clNIL);
  clSetq(last_cons, clCons(last, clNIL, clEOA));
  clSetq(result, last_cons);
  while (clSetq(x, _clGetObjectArg(*ap)), _clPresentp(x)) {
    clSetq(new_cons, clCons(x, clNIL, clEOA));
    clSetq(clConsCdr(last_cons), new_cons); 
    clSetq(last_cons, new_cons);
  }
  return(result);
}

clObject clFastFind(item, list) clObject item, list; {
  while (!_clNull(list)) {    
    if (_clEq(item, clConsCar(list))) return(item);
    clSetq(list, clConsCdr(list));
  }
  return(clNIL);
}

clObject clFastFindSlot(class, slot_name) clObject class, slot_name; {
  clObject list;
  clSetq(list, clFastSlotRef(class, 8));
  while (!_clNull(list)) {    
    if (_clEq(slot_name, clFastSlotRef(clConsCar(list), 2)))
      return(clConsCar(list));
    clSetq(list, clConsCdr(list));
  }
  return(clNIL);
}

/*********************************************************************
 * SIMPLE BASIC VECTORS

 *********************************************************************/
clObject clMakeBaseCharVector(size) clIndex size;
{ clObject obj; int n = clIndexInt(size)+1;
  clSetq(obj, clMakeTaggedInstance(clVectorMSize(clSimpleBaseStringCell,
						 char, n),
				   clSIMPLE_BASE_STRING));
  clISetq(clVectorSize(obj), size);
  clBaseCharElt(obj, size) = '\0'; /* N.B.: terminated! */
  return(obj); }

clObject clMakeExtendedCharVector(size) clIndex size;
{ clObject obj; int n = clIndexInt(size)+1;
  clSetq(obj, clMakeTaggedInstance(clVectorMSize(clSimpleStringCell,
						 wchar_t, n),
				   clSIMPLE_EXTENDED_STRING));
  clISetq(clVectorSize(obj), size);
  clExtendedCharElt(obj, size) = (wchar_t) 0; /* N.B.: terminated! */
  return(obj); }

clObject clMakeGeneralVector(size) clIndex size;
{ clObject obj; int n = clIndexInt(size);
  clSetq(obj, clMakeTaggedInstance(clVectorMSize(clSimpleVectorCell,
						 clObject, n),
				   clSIMPLE_VECTOR));
  clISetq(clVectorSize(obj), size);
  return(obj); }

/* N.B. It is important that unused bits in the last digit are zero so
   that the entire digit can participate in bit-boolean operations.
   clMakeTaggedInstance()->clMakeHeapObject()->clMallocObjectCell()
   currently clears all bits, so we don't need to do it again.
   However, if this should change, we need to add code here. */

clObject clMakeBitVector(size) clIndex size;
{ clObject obj; int n = clIndexInt(size);
  if (n) n = clNDigitsForBits(n);
  clSetq(obj, clMakeTaggedInstance(clVectorMSize(clSimpleBitVectorCell,
						 clDigit, n),
				   clSIMPLE_BIT_VECTOR));
  clISetq(clVectorSize(obj), size);
  return(obj); }

/*********************************************************************
 * STRING
 *********************************************************************/
char *clBaseStringCharp1(obj, offset) clObject obj; int offset;
{ return(clTrue(clSimpleStringP(obj, clEOA)) ?
	 clSimpleBaseStringCharp(obj) + offset :
	 clBaseStringCharp1(clSimpleArrayContents(obj),
			    offset +
			    clFixnumInt(clComplexArrayFillPointer(obj)))); }


clObject clCharpSimpleBaseString(name) const char *name;
{ clObject obj; size_t size;
  if (!name) return(clNIL);
  size = strlen(name);
  clSetq(obj, clMakeBaseCharVector(clIntIndex(size)));
  (void) memcpy(clSimpleBaseStringCharp(obj), name, size);
  return(obj); }

/*********************************************************************
 * ARRAY
 *********************************************************************/
static clObject clUpdateCommonArray(array, rank, contents, dims)
     clIndex rank; clObject array, contents, dims;
{ int i = 0;
  clSetq(clSimpleArrayContents(array), contents);
  clISetq(clSimpleArrayRank(array), rank);
  while (!_clNull(dims))
    { clISetq((clDataTagObjectCell(array)->simple_array).dimensions[i++],
	      clFixnumIndex(clConsCar(dims)));
      clSetq(dims, clConsCdr(dims)); }
  return(array); }

static clObject clMakeCommonArray(cell_size, class, wrapper, rank, contents, dims)
     m_size cell_size; clIndex rank; clObject class, wrapper, contents, dims;
{ return(clUpdateCommonArray(_clMakeTaggedInstance(cell_size, class, wrapper),
			     rank, contents, dims)); }

clObject clMakeSimpleArray(rank, dimensions, contents)
     clIndex rank; clObject contents, dimensions;
{ return(clMakeCommonArray(sizeof(clSimpleArrayCell),
			   clClassVar(clSIMPLE_ARRAY),
			   clWrapperVar(clSIMPLE_ARRAY), 
			   rank, contents, dimensions)); } 

clObject clMakeComplexArray(class, rank, dimensions, contents, offs, fp)
     clIndex rank; clObject class, contents, dimensions, offs, fp;
{ clObject obj;
  clSetq(obj, clMakeCommonArray(sizeof(clComplexArrayCell),
				class, clClassWrapper(class, clEOA),
				rank, contents, dimensions));
  clSetq(clComplexArrayFillPointer(obj), fp);
  clSetq(clComplexArrayOffset(obj), offs);
  return(obj); }

clObject clUpdateArray(array, contents, rank, dimensions, offs, fp)
     clObject array, contents, dimensions, offs, fp; clIndex rank;
{ (void) clUpdateCommonArray(array, rank, contents, dimensions);
  clSetq(clComplexArrayFillPointer(array), fp);
  clSetq(clComplexArrayOffset(array), offs);
  return(array); }
  

/*********************************************************************
 * STANDARD-INSTANCE
 *********************************************************************/
clObject _clMakeStandardInstanceFromSlots(class, wrapper, slots, cell_size)
     clObject class, wrapper; clObject slots; m_size cell_size;
{ clObject obj;
  clSetq(obj, _clMakeTaggedInstance(cell_size, class, wrapper));
  clSetStandardInstanceSlots(obj, slots);
  return(obj); }

clObject clMakeStandardInstanceFromSlots(class, wrapper, slots)
     clObject class, wrapper; clObject slots;
{ return(_clMakeStandardInstanceFromSlots(class, wrapper, slots,
					  sizeof(clStandardInstanceCell))); }

clObject clMakeFuncallableStandardInstanceFromSlots(class, wrapper, slots)
     clObject class, wrapper; clObject slots;
{ return(_clMakeStandardInstanceFromSlots(class, wrapper, slots,
					  sizeof(clFuncallableStandardInstanceCell))); }

clObject clMakeStandardInstance(n_slots, class, wrapper)
     clObject n_slots, class, wrapper;
{ return(clMakeStandardInstanceFromSlots(class, wrapper,
					 clMakeSlots(n_slots))); } 

clObject clMakeFuncallableStandardInstance(n_slots, class, wrapper)
     clObject n_slots, class, wrapper;
{ return(clMakeFuncallableStandardInstanceFromSlots(class, wrapper,
						    clMakeSlots(n_slots))); }


/*********************************************************************
 * INTERPRETER HELPER FUNCTIONS
 *********************************************************************/
/* Right now, the interpreter can't evaluate functions which don't
  have Lisp fdefinitions.  This includes c:functions and c:macros.
  Unfortunately, some DEFxxx macros use some of these.  We deal with
  this (for now), by defining some machine coded helper functions and
  making sure that the symbols are fbound.

  Other solutions might be:
  + Don't have generate any references to c:functions or c:macros in
  expansions of public macros that are intended to be usable at
  top-level.  
  + Allow c:functions, and make the interpreter recognize from the
  declaration that it must locate (at run-time) the c function symbol
  and pass it the correct number of evaluated arguments.  This
  requires some of the machinery needed for dynamic loading of object
  files, which we don't have yet.
  + Define a DEFFOREIGN style inteface that defines the helper
  functions in Lisp, use it in our Lisp source for these functions,
  and then machine compile the result.
  */

clObject clSetSymbolPlist_HELPER(clVaAlist) clVaDcl {
  clObject symbol, list; TwoArgs(symbol, list);
  return(clValues1(clSetSymbolPlist(symbol, list)));
}

clObject clSetSymbolValueValue_HELPER(clVaAlist) clVaDcl {
  clObject symbol, val; TwoArgs(symbol, val);
  return(clValues1(clSetSymbolValueValue(symbol, val)));
}

clObject clSetSymbolFunctionValue_HELPER(clVaAlist) clVaDcl {
  clObject symbol, val; TwoArgs(symbol, val);
  return(clValues1(clSetSymbolFunctionValue(symbol, val)));
}

clObject clSetSymbolSetfFunctionValue_HELPER(clVaAlist) clVaDcl {
  clObject symbol, val; TwoArgs(symbol, val);
  return(clValues1(clSetSymbolSetfFunctionValue(symbol, val)));
}

/*********************************************************************
 * INITIALIZATION
 *********************************************************************/
#define WrapperInit(tag,class_name)	\
 cl_POINTER_TAGGED_WRAPPERS[tag] = clWrapperVar(class_name)
#define ClassInit(tag,class_name)	\
  clSetq(cl_POINTER_TAGGED_CLASSES[tag], clClassVar(class_name))
#define TagInit(tag,class_name)  \
  (WrapperInit((int)tag,class_name), ClassInit((int)tag,class_name))

#define clKeyInit(name, string)	\
  clSetq(name, clMakeKeyword(clCharpSimpleBaseString(string), clEOA))

extern void clInitStaticClass __P((void)),
   clInitSymbolsExt __P((void)), clInitSymbolsInt __P((void)),
   clInitSymbolsAdd __P((void)), clInitCommon __P((void)),
   clInitKernel __P((void)), clInitMopInit __P((void)),
   clInitTypesRun __P((void)), clInitControlRun __P((void)),
   clInitSymbol __P((void)), clInitList __P((void)),
   clInitClasses __P((void)), clInitClosRun __P((void)),
   clInitClosDefine __P((void)), clInitClosSeq __P((void)),
   clInitMop __P((void)), clInitClassMeth __P((void)), 
   clInitMethods __P((void)), clInitGfunc __P((void)), 
   clInitMethodInit __P((void)), clInitPredicates __P((void)),
   clInitArithmetic __P((void)), clInitConv __P((void)),
   clInitHash __P((void)), clInitTypeOps __P((void)),
   clInitTypeSeq __P((void)), clInitMethodComb __P((void)),
   clInitTypeMops __P((void)), clInitSequence __P((void)),
   clInitSeqMod __P((void)), clInitSearch __P((void)),
   clInitSort __P((void)), clInitControl __P((void)), 
   clInitNumbers __P((void)), clInitTrig __P((void)),
   clInitNumConv __P((void)), clInitArray __P((void)), 
   clInitString __P((void)), clInitStructRun __P((void)),
   clInitCharacter __P((void)), clInitResource __P((void)),
   clInitTree __P((void)), clInitPackage __P((void)), 
   clInitCondition __P((void)), clInitSet __P((void)), 
   clInitAlist __P((void)), clInitBitArray __P((void)), 
   clInitBignum __P((void)), clInitBits __P((void)), 
   clInitEqualp __P((void)), clInitStream __P((void)),
   clInitFileStream __P((void)), clInitCompStream __P((void)),
   clInitCircle __P((void)), clInitInit __P((void)), 
   clInitReader __P((void)), clInitPrinter __P((void)), 
   clInitFormat __P((void)), clInitPretty __P((void)),
   clInitPrintObject __P((void)), clInitDoc __P((void)),
   clInitDescriber __P((void)), clInitMiscel __P((void)),
   clInitRandom __P((void)), clInitPathname __P((void)),
   clInitFile __P((void)), clInitEnclose __P((void)), 
   clInitDispatch __P((void)), clInitParameters __P((void)), 
   clInitPkg __P((void)),
   clInitCPkg __P((void)), clInitEnv __P((void));

int clPROGRESS_FLAG = 0;
void clProgress __P((const char *));
void clProgress(msg) const char *msg; {
  if (!clPROGRESS_FLAG) return;
  (void) write(1, " ", 1);
  (void) write(1, msg, strlen(msg));
  /*(void) tcdrain(1);*/
}


void clInit(control_stack_size) unsigned int control_stack_size; {
  clObject PKG_USR;
  clProgress("Initializing:");
#ifndef clNOGC
#  ifdef INCREMENTAL_GC
  GC_enable_incremental();
#  endif
  if (!GC_expand_hp(3000*4096)) {
    (void) puts("Not enough memory.");
    exit(1);
  }
#endif
  if (!control_stack_size) control_stack_size = 1024;
  CL_dynamic_environment = CL_dynamic_environment_base =
    (clControlOpCell *)
    clMallocControlStack(control_stack_size * sizeof(clControlOpCell));
  CL_dynamic_environment_limit = CL_dynamic_environment_base + control_stack_size;

  setlocale(LC_CTYPE, "");
  clInitStaticClass();
  { int i;
    for (i=0; i<(int)cl_MAX_TAG; TagInit(i,clUNKNOWN_OBJECT)) i++; }
  TagInit(cl_SIMPLE_CHARACTER_TAG,clCHARACTER);
  TagInit(cl_RESERVED2_TAG,clCHARACTER);
  TagInit(cl_EVEN_FIXNUM_TAG,clINTEGER);
  TagInit(cl_ODD_FIXNUM_TAG,clINTEGER);
#if !(defined cl_WORD_ALIGNED)
  TagInit(cl_CONS_TAG,clCONS);
  TagInit(cl_DOUBLE_FLOAT_TAG,clDOUBLE_FLOAT);
#endif
  clSetq(KEY_TABLE,
	 clMakeOpenAddressHashTable(clClassVar(clOPEN_ADDRESS_EQUAL_HASH_TABLE),
				    clIntIndex(1200),
				    clIntFixnum(400),
				    clIntIndex(1501)));
  clSetq(clECLIPSE_INTERN,
	 clMakeOpenAddressHashTable(clClassVar(clOPEN_ADDRESS_EQUAL_HASH_TABLE),
				    clIntIndex(6501),
				    clIntFixnum(400),
				    clIntIndex(8127)));
  clSetq(clECLIPSE_EXTERN,
	 clMakeOpenAddressHashTable(clClassVar(clOPEN_ADDRESS_EQUAL_HASH_TABLE),
				    clIntIndex(1502),
				    clIntFixnum(400),
				    clIntIndex(1877)));
  clSetq(PKG_KEYWORD,
	 clMakeStandardInstanceFromSlots(
	 clClassVar(clUNKNOWN_OBJECT),
	 clWrapperVar(clUNKNOWN_OBJECT),
	 clMakeStaticSlots(clCharpSimpleBaseString("KEYWORD"),
			   clNIL, clNIL, clNIL,
			   clMakeOpenAddressHashTable(clClassVar(clOPEN_ADDRESS_EQUAL_HASH_TABLE),
						      clIntIndex(1),
						      clIntFixnum(1),
						      clIntIndex(2)),
			   KEY_TABLE, 
			   clNIL, clNIL, clEOA)));
  clSetq(PKG_ECLIPSE,
	 clMakeStandardInstanceFromSlots(
	 clClassVar(clUNKNOWN_OBJECT),
	 clWrapperVar(clUNKNOWN_OBJECT),
	 clMakeStaticSlots(clCharpSimpleBaseString("ECLIPSE"),
			   clList(clCharpSimpleBaseString("SYS"),
				  clCharpSimpleBaseString("SYSTEM"), clEOA),
			   clNIL, clNIL,
			   clECLIPSE_INTERN, clECLIPSE_EXTERN,
			   clNIL, clNIL, clEOA)));
  clProgress("intern-ext"); clInitSymbolsExt(); 
  clProgress("intern-int"); clInitSymbolsInt(); 
  clProgress("intern-add"); clInitSymbolsAdd(); 
  clTypeInit(clNIL,clNULL);
  clSetSymbolValueValue(clNIL, clNIL);
  clSetSymbolValueValue(clT, clT);

  clKeyInit(keyDATUM, "DATUM");
  clKeyInit(keyDESCRIPTION, "DESCRIPTION");
  clKeyInit(keyDIRECTORY, "DIRECTORY");
  clKeyInit(keyECLIPSE, "ECLIPSE");
  clKeyInit(keyERRNO, "ERRNO");
  clKeyInit(keyEXPECTED_TYPE, "EXPECTED-TYPE");
  clKeyInit(keyFORMAT_ARGUMENTS, "FORMAT-ARGUMENTS");
  clKeyInit(keyFORMAT_CONTROL, "FORMAT-CONTROL");
  clKeyInit(keyFUNCTION, "FUNCTION");
  clKeyInit(keyJUNK_ALLOWED, "JUNK-ALLOWED");
  clKeyInit(keyPATHNAME, "PATHNAME");
  clKeyInit(keyLINK, "LINK");
  clKeyInit(keyNICKNAMES, "NICKNAMES");
  clKeyInit(keyRADIX, "RADIX");

  /* Other initializations...*/
  clSetSymbolValueValue(clNOT_FOUND, clMakeSymbol(_clSymbolName(clNOT_FOUND), clEOA));

  clSetSymbolFunctionValue(clCONS, clMakeClosure(0, clCons, clNULL_HOOK));
  clSetSymbolFunctionValue(clCAR, clMakeClosure(0, clCar, clNULL_HOOK));
  clSetSymbolFunctionValue(clCDR, clMakeClosure(0, clCdr, clNULL_HOOK));
  clSetSymbolFunctionValue(clRPLACA, clMakeClosure(0, clRplaca, clNULL_HOOK));
  clSetSymbolFunctionValue(clRPLACD, clMakeClosure(0, clRplacd, clNULL_HOOK));
  clSetSymbolFunctionValue(clVALUES, clMakeClosure(0, clValues, clNULL_HOOK));
  clSetSymbolFunctionValue(clAPPLY, clMakeClosure(0, clApply, clNULL_HOOK));
  clSetSymbolFunctionValue(clFUNCALL, clMakeClosure(0, clFuncall, clNULL_HOOK));
  clSetSymbolFunctionValue(clEQ, clMakeClosure(0, clEq, clNULL_HOOK));
  clSetSymbolFunctionValue(clFIXNUMP, clMakeClosure(0, clFixnump, clNULL_HOOK));
  clSetSymbolFunctionValue(clMAKE_SYMBOL, clMakeClosure(0, clMakeSymbol, clNULL_HOOK));
  clSetSymbolFunctionValue(clMAKE_KEYWORD, clMakeClosure(0, clMakeKeyword, clNULL_HOOK));
  clSetSymbolFunctionValue(clSYMBOL_NAME, clMakeClosure(0, clSymbolName, clNULL_HOOK));
  clSetSymbolFunctionValue(clSYMBOL_PLIST, clMakeClosure(0, clSymbolPlist, clNULL_HOOK));
  clSetSymbolFunctionValue(clGETCWD, clMakeClosure(0, clGetcwd, clNULL_HOOK));
  clSetSymbolFunctionValue(clGETENV, clMakeClosure(0, clGetenv, clNULL_HOOK));
  clSetSymbolFunctionValue(clMACHINE_ID, clMakeClosure(0, clMachineId, clNULL_HOOK));
  clSetSymbolFunctionValue(clRUN_TIME, clMakeClosure(0, clRunTime, clNULL_HOOK));
  clSetSymbolFunctionValue(clREAL_TIME, clMakeClosure(0, clRealTime, clNULL_HOOK));

  clSetSymbolFunctionValue(clSET_SYMBOL_PLIST,
			   clMakeClosure(0, clSetSymbolPlist_HELPER, clNULL_HOOK));
  clSetSymbolFunctionValue(clSET_SYMBOL_VALUE_VALUE,
			   clMakeClosure(0, clSetSymbolValueValue_HELPER, clNULL_HOOK));
  clSetSymbolFunctionValue(clSET_SYMBOL_FUNCTION_VALUE,
			   clMakeClosure(0, clSetSymbolFunctionValue_HELPER, clNULL_HOOK));
  clSetSymbolFunctionValue(clSET_SYMBOL_SETF_FUNCTION_VALUE,
			   clMakeClosure(0, clSetSymbolSetfFunctionValue_HELPER, clNULL_HOOK));
  
  clSetSymbolFunctionValue(clFUNCALL_FUNCTION,
			   clMakeClosure(0, clFuncallFunction, clNULL_HOOK));
  clSetSymbolFunctionValue(clAPPLY_FUNCTION,
			   clMakeClosure(0, clApplyFunction, clNULL_HOOK));
  
  clProgress("list"); clInitList(); 
  clProgress("kernel"); clInitKernel(); 
  clProgress("mop-init"); clInitMopInit(); 
  clProgress("types-run"); clInitTypesRun(); 
  clProgress("control-run"); clInitControlRun(); 
  clProgress("symbol"); clInitSymbol(); 
  clProgress("clases"); clInitClasses(); 
  clProgress("clos-run"); clInitClosRun(); 
  clProgress("clos-define"); clInitClosDefine(); 
  clProgress("clos-seq"); clInitClosSeq(); 
  clProgress("common"); clInitCommon(); 
  clProgress("mop"); clInitMop(); 
  clProgress("class-meth"); clInitClassMeth(); 
  clProgress("methods"); clInitMethods(); 
  clProgress("gfunc"); clInitGfunc(); 
  clProgress("method-init"); clInitMethodInit(); 
  clProgress("predicates"); clInitPredicates(); 
  clProgress("arithmetic"); clInitArithmetic(); 
  clProgress("conv"); clInitConv(); 
  clProgress("hash"); clInitHash(); 
  clProgress("type-ops"); clInitTypeOps(); 
  clProgress("type-seq"); clInitTypeSeq(); 
  clProgress("method-comb"); clInitMethodComb(); 
  clProgress("type-mops"); clInitTypeMops(); 
  clProgress("sequence"); clInitSequence(); 
  clProgress("seq-mod"); clInitSeqMod(); 
  clProgress("search"); clInitSearch(); 
  clProgress("sort"); clInitSort(); 
  clProgress("control"); clInitControl(); 
  clProgress("numbers"); clInitNumbers(); 
  clProgress("trig"); clInitTrig(); 
  clProgress("num-conv"); clInitNumConv(); 
  clProgress("array"); clInitArray(); 
  clProgress("string"); clInitString(); 
  clProgress("struct-run"); clInitStructRun(); 
  clProgress("character"); clInitCharacter(); 
  clProgress("tree"); clInitTree(); 
  clProgress("package"); clInitPackage();
  {				/* final package system setup */
    clObject x;
    clSetq(x, clFindType(clPACKAGE, clEOA));
    clSetTaggedInstanceClass(PKG_KEYWORD, x);
    clSetTaggedInstanceClass(PKG_ECLIPSE, x);
    clSetTaggedInstanceWrapper(PKG_KEYWORD, clClassWrapper(x, clEOA));
    clSetTaggedInstanceWrapper(PKG_ECLIPSE, clClassWrapper(x, clEOA));
    clSetSymbolValueValue(clstarKEYWORD_PACKAGEstar, PKG_KEYWORD);
    clSetSymbolValueValue(clstarPACKAGEstar, PKG_ECLIPSE);
    clAddPkg(clPackageName(PKG_KEYWORD, clEOA), PKG_KEYWORD, clEOA);
    clAddPkg(clPackageName(PKG_ECLIPSE, clEOA), PKG_ECLIPSE, clEOA);
    clAddPkg(clConsCar(clPackageNicknames(PKG_ECLIPSE, clEOA)), PKG_ECLIPSE, clEOA);
    clAddPkg(clConsCar(clConsCdr(clPackageNicknames(PKG_ECLIPSE,
						    clEOA))),
	     PKG_ECLIPSE, clEOA);
    clSetq(PKG_USR, clMakePackage(clCharpSimpleBaseString("USER"),
				  keyNICKNAMES,
				  clList(clCharpSimpleBaseString("COMMON-LISP-USER"),
					 clCharpSimpleBaseString("CL-USER"),
					 clEOA), clEOA));
  }

clProgress("c-pkg"); clInitCPkg();

  clProgress("resource"); clInitResource(); 

clProgress("env"); clInitEnv();

  clProgress("condition"); clInitCondition(); 
  clProgress("set"); clInitSet(); 
  clProgress("alist"); clInitAlist(); 
  clProgress("bit-array"); clInitBitArray(); 
  clProgress("bignum"); clInitBignum(); 
  clProgress("bits"); clInitBits(); 
  clProgress("equalp"); clInitEqualp(); 
  clProgress("stream"); clInitStream(); 
  clProgress("file-stream"); clInitFileStream(); 
  clProgress("comp-stream"); clInitCompStream(); 
  clProgress("circle"); clInitCircle(); 
  clProgress("init"); clInitInit(); 
  clProgress("reader"); clInitReader();
  /* This can't be done by clInitReader because it binds *readtable*! */
  clSetSymbolValueValue(clstarREADTABLEstar,
                        clCopyReadtable(clSymbolValueValue(clstarSTANDARD_READTABLEstar),
                                        clEOA));
  clProgress("printer"); clInitPrinter(); 
  clProgress("format"); clInitFormat(); 
  clProgress("pretty"); clInitPretty(); 
  clProgress("print-object"); clInitPrintObject(); 
  clProgress("doc"); clInitDoc(); 
  clProgress("describer"); clInitDescriber(); 
  clProgress("miscel"); clInitMiscel(); 
  clProgress("random"); clInitRandom(); 
  clProgress("pathname"); clInitPathname(); 
  clProgress("file"); clInitFile(); 
  clProgress("enclose"); clInitEnclose(); 
  clProgress("dispatch"); clInitDispatch(); 
  clProgress("parameters"); clInitParameters(); 
  clProgress("pkg"); clInitPkg();
  clProgress("signals");

  clSetq(_clSymbolValue(clstarENTRIESstar), clNIL);/* Testing only */

  {/* final initialization */
    clSetq(_clSymbolValue(clINTERNAL_TIME_UNITS_PER_SECOND), clIntFixnum(clHZ));
    
#ifdef _WIN32
    AllowExceptions();
#endif
#define SetInt(name, symbol)	\
    clSetInterrupt(clIntFixnum(name), clSymbolFunctionValue(symbol), clEOA)
    SetInt(SIGINT, clINTERRUPT_HANDLER);
    SetInt(SIGFPE, clFPE_HANDLER);
    SetInt(SIGILL, clILLEGAL_INSTRUCTION_HANDLER);
#if !(defined linux) & !(defined _WIN32)
    SetInt(SIGSYS, clSYS_ERROR_HANDLER);
#endif
#ifndef INCREMENTAL_GC
    /* The incremental garbage collector we use maintains dirty bits
       information by catching write faults signalled with SIGBUS and
       SIGSEGV, so we'd better not trap them. */
#   if !(defined _WIN32)
      SetInt(SIGBUS, clBUS_ERROR_HANDLER);
#   endif 
    SetInt(SIGSEGV, clSEGV_HANDLER);
#endif

    clSetSymbolValueValue(clstarPACKAGEstar, PKG_USR);
    clSetSymbolValueValue(clIntern(clCharpSimpleBaseString("*EVAL*"), clEOA),
			  clT); /* bootstrap.  Remove this! */
    clProgress("\n");

    /* There should not be any system calls that use malloc(), because
       they can interfere with the garbage collector. Unfortunately,
       operating systems don't always document when they have hidden
       calls to malloc() in system code.  Any such accidental calls
       are less likely to bother us if if we do enough GC work here. */
    GC_gcollect();
  }
}

#ifdef clDEBUG_INIT
/* This can be handy when debugging system inititialization before
   print-object is defined. */ 
extern clObject clMACRO_FUNCTION_CLASSOBJ, clHASH_TABLE_CLASSOBJ,
  clOPEN_ADDRESS_EQ_HASH_TABLE_CLASSOBJ,
  clOPEN_ADDRESS_EQL_HASH_TABLE_CLASSOBJ,
  clOPEN_ADDRESS_EQUALP_HASH_TABLE_CLASSOBJ,
  clSTANDARD_EFFECTIVE_SLOT_DEFINITION_CLASSOBJ,
  clSTANDARD_DIRECT_SLOT_DEFINITION_CLASSOBJ,
  clSTANDARD_SYSTEM_GENERIC_FUNCTION_CLASSOBJ,
  clSTANDARD_GENERIC_FUNCTION_CLASSOBJ,
  clSTANDARD_SYSTEM_CLASS_CLASSOBJ,
  clFUNCALLABLE_STANDARD_SYSTEM_CLASS_CLASSOBJ,
  clBUILT_IN_CLASS_CLASSOBJ, clSTANDARD_CLASS_CLASSOBJ,
  clSTANDARD_METHOD_CLASSOBJ, clSTANDARD_READER_METHOD_CLASSOBJ,
  clSTANDARD_WRITER_METHOD_CLASSOBJ, clCONDITION, clSIMPLE_CONDITION,
  clSIMPLE_CONDITION_FORMAT_CONTROL, 
  clSIMPLE_CONDITION_FORMAT_ARGUMENTS, clCELL_ERROR,
  clCELL_ERROR_NAME, clLIMITED_NUMERIC_TYPE, clTYPE_NAME, clBASE_TYPE,
  clSTRUCTURE_CLASS, clLIMITED_TYPE_MIN, clLIMITED_TYPE_MAX,
  clLIMITED_TYPE_INCLUSIVE_MIN, clLIMITED_TYPE_INCLUSIVE_MAX,
  clConsp(clProto), clStandardInstanceAccess(clProto);

void indent __P((int));
void indent(ind) int ind;
{ int i;
  for (i = 0; i < ind; i++) (void) putchar(' ');
  if (!indent) (void) puts(""); }

void describe_symbol __P((clObject, int));
void describe_symbol(obj, ind) clObject obj; int ind;
{
  if (ind > 2) {
    describe("NAME:", _clSymbolName(obj), ind);
    describe("PACKAGE:", _clSymbolPackage(obj), ind);
    return;
  }
  ind += 2;
  describe("NAME:", _clSymbolName(obj), ind);
  describe("PACKAGE:", _clSymbolPackage(obj), ind);
  describe("VALUE:", _clSymbolValue(obj), ind);
  describe("FUNCTION:", _clSymbolFunction(obj), ind);
  describe("SETF-FUNCTION:", _clSymbolSetfFunction(obj), ind);
  describe("PLIST:", _clSymbolPlist(obj), ind);
}

int describe(name, obj, ind) const char *name; clObject obj; int ind;
{ clObject class, wrapper;
  indent(ind); (void) printf("%s @ %x, pointer-tag %x\n", name, obj,
			     clObjectPointerTag(obj));
  if (ind > 10) return(ind);
  ind += 2; indent(ind);
  if (!_clPresentp(obj)) {
    (void) puts("UNBOUND");
    return(-1);
  }
  clSetq(class, clObjectClass(obj));
  if (_clEq(class, clClassVar(clNULL))) {
    (void) puts("NULL");
  } else if (_clEq(class, clClassVar(clSYMBOL))) {
    (void) puts("SYMBOL");
    if (_clEq(obj, clT))
      describe("NAME:", _clSymbolName(obj), ind);
    else
      describe_symbol(obj, ind);
  } else if (_clEq(class, clClassVar(clSIMPLE_BASE_STRING))) {
    (void) printf("SIMPLE-BASE-STRING: %s\n",
			       clSimpleBaseStringCharp(obj));
  } else if (_clEq(class, clClassVar(clSIMPLE_EXTENDED_STRING))) {
    char buf[1024], *p=buf; wchar_t *wp = clSimpleStringWcharp(obj);
    while (*wp) *p++ = (char) *wp++; *p = '\0';
    (void) printf("SIMPLE-STRING: %s\n", buf);
  } else if (_clEq(class, clClassVar(clCOMPLEX_BASE_STRING))) {
    char p[1024];
    (void) strcpy(p, clSimpleBaseStringCharp(clSimpleArrayContents(obj)));
    p[clFixnumInt(clComplexArrayFillPointer(obj))] = '\0';
    (void) printf("COMPLEX-BASE-STRING: %s\n", p);
  } else if (_clEq(class, clClassVar(clINTEGER))) {
    if (_clFixnump(obj))
      (void) printf("FIXNUM %d\n", clFixnumInt(obj));
    else {
      clObject xint, size; int s, ii;
      clSetq(xint, clBignumXint(obj));
      clSetq(size, clXintSize(xint));
      s = clFixnumInt(size);
      (void) printf("BIGNUM of size %d\n", s);
      ind += 2; 
      for (ii = 0; ii < s; ii++) {
	int dig = clFixnumInt(clDigitref(xint, clIntIndex(ii)));
	indent(ind); (void) printf("%d: #x%4x (%d.)\n", ii, dig, dig);
      }
    }
  } else if (_clEq(class, clClassVar(clSINGLE_FLOAT))) {
    (void) printf("SINGLE-FLOAT %f\n", clSingleFloatFloat(obj));
  } else if (_clEq(class, clClassVar(clDOUBLE_FLOAT))) {
    (void) printf("DOUBLE-FLOAT %f\n", clDoubleFloatDouble(obj));
  } else if (_clEq(class, clClassVar(clBUILT_IN_FUNCTION))) {
    (void) printf("BUILT-IN-FUNCTION 0x%x\n", clClosureFunction(obj));
  } else if (_clEq(class, clClassVar(clMACRO_FUNCTION))) {
    (void) printf("MACRO-FUNCTION 0x%x\n", clClosureFunction(obj));
  } else if (_clEq(class, clClassVar(clCHARACTER))) {
    int c = clCharacterInt(obj);
    (void) printf("CHARACTER '%c' (0x%x)\n", c, c);
  } else if (_clEq(class, clClassVar(clCONS))) {
    (void) puts("LIST");
    while (!_clNull(obj)) {
      if (!clTrue(clConsp(obj, clEOA))) {
	describe(" . ", obj, ind); break;
      }
      describe("", clConsCar(obj), ind);
      clSetq(obj, clConsCdr(obj));
    }
  } else if (_clEq(class, clClassVar(clSIMPLE_VECTOR))) {
    (void) printf("VECTOR, length %d\n", clIndexInt(clVectorSize(obj)));
    describe("class", class, ind);
  } else if (_clEq(class, clClassVar(clSIMPLE_ARRAY))) {
    int i, rank = clIndexInt(clSimpleArrayRank(obj));
    (void) printf("ARRAY, rank %d\n", rank);
    indent(ind); (void) printf("[ ");
    for (i=0; i<rank; i++)
      (void) printf("%d ", clIndexInt(clSimpleArrayDimension(obj, clIntIndex(i))));
    (void) printf("]\n");
    describe("class:", class, ind);
  } else if (_clEq(class, clClassVar(clUNKNOWN_OBJECT))) {
    clSetq(wrapper, clObjectWrapper(obj));
    (void) printf("UNKNOWN-OBJECT 0x%x, wrapper 0x%x\n", clObjectAddress(obj),
		  clObjectAddress(wrapper));
  } else if (_clEq(class, clClassVar(clHASH_TABLE)) ||
	     _clEq(class, clClassVar(clOPEN_ADDRESS_HASH_TABLE)) ||
	     _clEq(class, clClassVar(clOPEN_ADDRESS_EQ_HASH_TABLE)) ||
	     _clEq(class, clClassVar(clOPEN_ADDRESS_EQL_HASH_TABLE)) ||
	     _clEq(class, clClassVar(clOPEN_ADDRESS_EQUAL_HASH_TABLE)) ||
	     _clEq(class, clClassVar(clOPEN_ADDRESS_EQUALP_HASH_TABLE))) {
    (void) printf("HASH TABLE size: %d, n_buckets: %d, count: %d\n",
		  clIndexInt(clOpenAddressHashTableSize(obj)),
		  clIndexInt(clOpenAddressHashTableNBuckets(obj)),
		  clIndexInt(clOpenAddressHashTableCount(obj)));
    describe("class:", class, ind);
  } else if (_clEq(class, clClassVar(clSTANDARD_EFFECTIVE_SLOT_DEFINITION))) {
    (void) puts("effective-slot");
    describe("name:", clStandardInstanceAccess(obj, clIntInteger(2), clEOA), ind); 
    describe("location:", clStandardInstanceAccess(obj, clIntInteger(8), clEOA), ind); 
  } else if (_clEq(class, clClassVar(clSTANDARD_DIRECT_SLOT_DEFINITION))) {
    (void) puts("direct-slot");
    describe("name:", clStandardInstanceAccess(obj, clIntInteger(2), clEOA), ind); 
  } else if (_clEq(class, clClassVar(clSTANDARD_SYSTEM_GENERIC_FUNCTION)) ||
	     _clEq(class, clClassVar(clSTANDARD_GENERIC_FUNCTION)) ||
	     _clEq(class, clClassVar(clSTANDARD_SYSTEM_CLASS)) ||
	     _clEq(class, clClassVar(clFUNCALLABLE_STANDARD_SYSTEM_CLASS)) ||
	     _clEq(class, clClassVar(clBUILT_IN_CLASS)) ||
	     _clEq(class, clClassVar(clSTANDARD_CLASS)) ||
	     _clEq(class, clFindType(clSTRUCTURE_CLASS, clNIL, clEOA))) {
    (void) puts("metaobject");
    describe("name:", clStandardInstanceAccess(obj, clIntInteger(2), clEOA), ind);
    if (ind > 2) return(ind);
    describe("metaclass:", class, ind);
  } else if (_clEq(class, clClassVar(clSTANDARD_METHOD)) ||
	     _clEq(class, clClassVar(clSTANDARD_READER_METHOD)) ||
	     _clEq(class, clClassVar(clSTANDARD_WRITER_METHOD))) {
    (void) puts("method");
    describe("gf:", clStandardInstanceAccess(obj, clIntInteger(6), clEOA), ind);
    describe("qualifiers:", clStandardInstanceAccess(obj, clIntInteger(2), clEOA), ind);
    describe("specializers:", clStandardInstanceAccess(obj, clIntInteger(4), clEOA), ind);
  } else if (_clEq(class, clFindType(clPACKAGE, clNIL, clEOA))) {
    (void) puts("PACKAGE");
    describe("NAME:", clPackageName(obj, clEOA), ind);
  } else if ((clTrue(clSymbolFunctionValue(clTYPEP))) &&
	     (clTrue(clTypep(obj, clCONDITION, clEOA)))) {
    (void) puts("CONDITION");
    describe("metaclass:", class, ind);
    if (clTrue(clTypep(obj, clSIMPLE_CONDITION, clEOA))) {
      describe("control:", 
	       clFuncallFunction(clSymbolFunctionValue(clSIMPLE_CONDITION_FORMAT_CONTROL),
				 obj, clEOA),
	       ind);
      describe("arguments:", 
	       clFuncallFunction(clSymbolFunctionValue(clSIMPLE_CONDITION_FORMAT_ARGUMENTS),
				 obj, clEOA),
	       ind);
    }
    if (clTrue(clTypep(obj, clCELL_ERROR, clEOA)))
      describe("name:",
	       clFuncallFunction(clSymbolFunctionValue(clCELL_ERROR_NAME),
				 obj, clEOA),
	       ind);
    
  } else if (_clEq(class, clFindType(clLIMITED_NUMERIC_TYPE, clNIL, clEOA))) {
    (void) puts("Limited-numeric-type");
    describe("name:",
	     clFuncallFunction(clSymbolFunctionValue(clTYPE_NAME), obj, clEOA),
	     ind);
    describe("base-type:",
	     clFuncallFunction(clSymbolFunctionValue(clBASE_TYPE), obj, clEOA),
	     ind);
    
    describe("min:",
	     clFuncallFunction(clSymbolFunctionValue(clLIMITED_TYPE_MIN), obj, clEOA),
	     ind);
    describe("max:",
	     clFuncallFunction(clSymbolFunctionValue(clLIMITED_TYPE_MAX), obj, clEOA),
	     ind);
    describe("inclusive min:",
	     clFuncallFunction(clSymbolFunctionValue(clLIMITED_TYPE_INCLUSIVE_MIN), obj, clEOA),
	     ind);
    describe("inclusive max:",
	     clFuncallFunction(clSymbolFunctionValue(clLIMITED_TYPE_INCLUSIVE_MAX), obj, clEOA),
	     ind);
  } else {
    clSetq(wrapper, clObjectWrapper(obj));
    (void) printf("OTHER type, class at 0x%x, wrapper 0x%x\n",
		  clObjectAddress(class), clObjectAddress(wrapper));
    describe("metaclass:", class, ind);
  }
  return(0);
}
#endif /* clDEBUG_INIT */
