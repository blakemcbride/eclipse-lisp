# Microsoft Developer Studio Project File - Name="test" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=test - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "test.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "test.mak" CFG="test - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "test - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "test - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe

!IF  "$(CFG)" == "test - Win32 Release"

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
# ADD CPP /nologo /W3 /GX /O2 /I "../../../../c" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "test - Win32 Debug"

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

# Name "test - Win32 Release"
# Name "test - Win32 Debug"
# Begin Source File

SOURCE=..\..\..\..\test\array.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\character.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\clos.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\condition.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\control.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\control2.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\eclipse.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\error.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\file.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\hash.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\list.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\loop.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\macro.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\misc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers2.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers3.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers4.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers5.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\numbers6.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\package.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\parsepath.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\pathname.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\pkg.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\predicates.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\printer.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\program.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\reader.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\rt.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\sequence.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\stream.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\string.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\structure.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\subtypes.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\symbol.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\transpath.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\types.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\test\typespec.c
# End Source File
# End Target
# End Project
