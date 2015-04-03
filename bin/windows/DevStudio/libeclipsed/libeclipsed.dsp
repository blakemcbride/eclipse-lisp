# Microsoft Developer Studio Project File - Name="libeclipsed" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libeclipsed - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libeclipsed.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libeclipsed.mak" CFG="libeclipsed - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libeclipsed - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libeclipsed - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe

!IF  "$(CFG)" == "libeclipsed - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../../../.." /I "../../../../c" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libeclipsed - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "../../../../c" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libeclipsed - Win32 Release"
# Name "libeclipsed - Win32 Debug"
# Begin Source File

SOURCE="..\..\..\..\c\c-walk.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\c.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\clos-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\common-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\cond-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\cont-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\cont-comp2.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\debug.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\default-pp.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\describe.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\dev-meth.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\directives.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\eclipse.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\env-comp.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\evaluation.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\file-walk.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\initd.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\list-comp.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\literal.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\loop.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\macro-comp.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\macro.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\more-compile.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\number-comp.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\opts.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\pretty-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\prim-decs.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\prog-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\struct-comp.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\walk-special.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\walk-top.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\walk-util.c"
# End Source File
# End Target
# End Project
