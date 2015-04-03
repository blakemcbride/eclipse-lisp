# Microsoft Developer Studio Generated NMAKE File, Based on eclipse.dsp
!IF "$(CFG)" == ""
CFG=eclipse - Win32 Debug
!MESSAGE No configuration specified. Defaulting to eclipse - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "eclipse - Win32 Release" && "$(CFG)" !=\
 "eclipse - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "eclipse.mak" CFG="eclipse - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "eclipse - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "eclipse - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "eclipse - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\eclipse.exe"

!ELSE 

ALL : "edlib - Win32 Release" "$(OUTDIR)\eclipse.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"edlib - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eclipse.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\eclipse.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D\
 "_MBCS" /Fp"$(INTDIR)\eclipse.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c\
 
CPP_OBJS=.\Release/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\eclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\eclipse.pdb" /machine:I386 /out:"$(OUTDIR)\eclipse.exe" 
LINK32_OBJS= \
	"$(INTDIR)\eclipse.obj" \
	"..\edlib\Release\edlib.lib"

"$(OUTDIR)\eclipse.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "eclipse - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\eclipse.exe"

!ELSE 

ALL : "edlib - Win32 Debug" "$(OUTDIR)\eclipse.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"edlib - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\eclipse.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\eclipse.exe"
	-@erase "$(OUTDIR)\eclipse.ilk"
	-@erase "$(OUTDIR)\eclipse.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /I "c:\howard\eclipse" /D "WIN32" /D\
 "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\eclipse.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\eclipse.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\eclipse.pdb" /debug /machine:I386 /out:"$(OUTDIR)\eclipse.exe"\
 /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\eclipse.obj" \
	"..\edlib\Debug\edlib.lib"

"$(OUTDIR)\eclipse.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "eclipse - Win32 Release" || "$(CFG)" ==\
 "eclipse - Win32 Debug"

!IF  "$(CFG)" == "eclipse - Win32 Release"

"edlib - Win32 Release" : 
   cd "\howard\eclipse\DevStudio\edlib"
   $(MAKE) /$(MAKEFLAGS) /F .\edlib.mak CFG="edlib - Win32 Release" 
   cd "..\eclipse"

"edlib - Win32 ReleaseCLEAN" : 
   cd "\howard\eclipse\DevStudio\edlib"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\edlib.mak CFG="edlib - Win32 Release"\
 RECURSE=1 
   cd "..\eclipse"

!ELSEIF  "$(CFG)" == "eclipse - Win32 Debug"

"edlib - Win32 Debug" : 
   cd "\howard\eclipse\DevStudio\edlib"
   $(MAKE) /$(MAKEFLAGS) /F .\edlib.mak CFG="edlib - Win32 Debug" 
   cd "..\eclipse"

"edlib - Win32 DebugCLEAN" : 
   cd "\howard\eclipse\DevStudio\edlib"
   $(MAKE) /$(MAKEFLAGS) CLEAN /F .\edlib.mak CFG="edlib - Win32 Debug"\
 RECURSE=1 
   cd "..\eclipse"

!ENDIF 

SOURCE=..\..\eclipse.c

!IF  "$(CFG)" == "eclipse - Win32 Release"


"$(INTDIR)\eclipse.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "eclipse - Win32 Debug"

DEP_CPP_ECLIP=\
	"..\..\eclipse.h"\
	

"$(INTDIR)\eclipse.obj" : $(SOURCE) $(DEP_CPP_ECLIP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

