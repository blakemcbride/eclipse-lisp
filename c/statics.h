#if !(defined __STDC__)
#  include <memory.h>		/* memcpy */
#endif

#if (defined SunOS4)
   typedef unsigned m_size;
#else
#  include <sys/types.h>
   typedef size_t m_size;
#endif

void clClearSymbol __P((clObject, clObject));
clObject _clMakeTaggedInstance __P((m_size, clObject, clObject));

#define clStaticClassVar(name)	 	clPaste2(name,_CLASSOBJ_)
#define clStaticWrapperVar(name)	clPaste2(name,_WRAPPEROBJ_)
#define clStaticVar(name) 		clPaste2(name,_symbol_)
#define clStaticSymbolVar(name) 	clPaste2(name,_string_)

#define clClassVar(name) 		clPaste2(name,_CLASSOBJ)
#define clWrapperVar(name) 		clPaste2(name,_WRAPPEROBJ)

#define clClassDef(name) 	\
  extern clObject clClassVar(name), clWrapperVar(name)

#define clExternDef(name) clClassDef(name); \
  extern clStandardInstanceCell clStaticClassVar(name); \
  extern clWrapperCell clStaticWrapperVar(name)

#define clMakeTaggedInstance(size, class_name)	\
  _clMakeTaggedInstance(size, clClassVar(class_name), clWrapperVar(class_name))
#define clTypeInit(obj,class_name)				\
  clSetTaggedInstanceClass(obj, clClassVar(class_name)),	\
  clSetTaggedInstanceWrapper(obj, clWrapperVar(class_name))


#define _Intern(symbol, table)	\
  clOpenAddressStringSethash(_clSymbolName(symbol), table, symbol);\
  clSymbolPackageSetter(PKG_ECLIPSE, symbol)


#ifdef cl_CTYPECHECKS
   clObject clSimpleBaseStringpObject();
#  define clFlagInit(var, val) 		clObjectWord(var) = val
#else
#  define clSimpleBaseStringpObject(x)	((clObject) x)
#  define clFlagInit(var, val)
#endif

#if (defined __STDC__) || (defined __GNUC__)
#  define clStaticClassInit(name) = clClassObject(name)
#  define clStaticWrapperInit(name) = clWrapperObject(name)
#  define clStaticSymbolInit(name) = clSymbolpObject(&clStaticVar(name))
#  define clStaticSimpleBaseStringFieldInit(length, symbol_name)	\
 = { clStaticTag(clSIMPLE_BASE_STRING), 	\
     clIntIndex(length), symbol_name }
#  define clStaticSymbolFieldInit(name) 	\
 = { clStaticTag(clSYMBOL), 			\
     clUNBOUND,					\
     clSymbolpObject(&clStaticVar(clNIL)),	\
     (clObject) &clStaticSymbolVar(name),	\
     clSymbolpObject(&clStaticVar(clNIL)),	\
     clSymbolpObject(&clStaticVar(clNIL)),	\
     clSymbolpObject(&clStaticVar(clNIL)) }

#  define clClassInit(name)
#  define clSymbolInit(name, length, symbol_name)	\
      _Intern(name, ThisTable)
#else
#  define clStaticClassInit(name)
#  define clStaticWrapperInit(name)
#  define clStaticSymbolInit(name)
#  define clStaticSymbolFieldInit(name)
#  define clStaticSimpleBaseStringFieldInit(length, symbol_name)

#  define clClassInit(name)	\
     clSetq(clClassVar(name), clClassObject(name));	\
     clSetq(clWrapperVar(name), clWrapperObject(name))
#  define clSymbolInit(name, length, symbol_name)	\
     clSetq(name, clSymbolpObject(&clStaticVar(name)));	\
     clTypeInit(name,clSYMBOL);				\
     clClearSymbol(name, clSimpleBaseStringpObject((clSimpleBaseStringCell *)	\
						   &clStaticSymbolVar(name)));	\
     clTypeInit(_clSymbolName(name),clSIMPLE_BASE_STRING);			\
     clISetq(clVectorSize(_clSymbolName(name)), clIntIndex(length));		\
     (void) memcpy(clSimpleBaseStringCharp(_clSymbolName(name)),		\
		   symbol_name, length+1);					\
     _Intern(name, ThisTable)
#endif

#define clClassObject(name) 	clStandardInstancepObject(&clStaticClassVar(name))
#define clWrapperObject(name)	clWrapperpObject(&clStaticWrapperVar(name))
#define clStaticTag(name)	{ { clClassObject(name) }, clWrapperObject(name) }

/* obsolete_slots field is never used for built-in-class wrappers */
#define clStaticStandardInstance(name, nn)			\
 clWrapperCell clStaticWrapperVar(name) = { clIntIndex(nn) }; 	\
 clStandardInstanceCell clStaticClassVar(name); 		\
 clObject clWrapperVar(name) clStaticWrapperInit(name);		\
 clObject clClassVar(name) clStaticClassInit(name)

#define clStaticSymbol(name, length, symbol_name)			\
 clStaticBaseStringCell(length+1) clStaticSymbolVar(name) 		\
    clStaticSimpleBaseStringFieldInit(length, symbol_name);	 	\
 clSymbolCell clStaticVar(name) clStaticSymbolFieldInit(name);		\
 clObject name clStaticSymbolInit(name)


#ifndef cl_ECLIPSE_DEFINITIONS
  extern clObject clOpenAddressStringSethash __P((clObject, clObject, clObject)); 
  extern
#endif
    clObject clECLIPSE_INTERN, clECLIPSE_EXTERN, PKG_ECLIPSE;
