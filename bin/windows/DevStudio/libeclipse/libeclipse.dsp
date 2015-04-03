# Microsoft Developer Studio Project File - Name="libeclipse" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libeclipse - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libeclipse.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libeclipse.mak" CFG="libeclipse - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libeclipse - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libeclipse - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe

!IF  "$(CFG)" == "libeclipse - Win32 Release"

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
# ADD CPP /nologo /W3 /GX /O2 /I "../../../../c" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "SMALL_CONFIG" /D "SILENT" /D "ALL_INTERIOR_POINTERS" /D "__STDC__" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "libeclipse - Win32 Debug"

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
# ADD CPP /nologo /W3 /GX /Z7 /Od /I "../../../../c" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "SMALL_CONFIG" /D "SILENT" /D "ALL_INTERIOR_POINTERS" /D "__STDC__" /YX /FD /c
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "libeclipse - Win32 Release"
# Name "libeclipse - Win32 Debug"
# Begin Group "gc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\..\c\gc\allchblk.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\alloc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\blacklst.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\config.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\dbg_mlc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\dyn_load.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\finalize.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_cpp.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_cpp.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_hdrs.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_mark.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_priv.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_private.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\gc_typed.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\headers.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\mach_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\malloc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\mallocx.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\mark.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\mark_rts.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\misc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\new_hblk.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\obj_map.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\os_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\ptr_chck.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\reclaim.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\stubborn.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gc\typd_mlc.c
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\..\..\c\alist.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\arithmetic.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\array.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\bignum.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\bit-array.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\bits.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\c-pkg.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\character.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\chars.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\circle.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\class-meth.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\classes.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\clos-define.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\clos-run.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\clos-seq.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\common.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\comp-stream.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\condition.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\control-run.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\control.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\conv.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\describer.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\dispatch.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\doc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\eclipse.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\enclose.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\env.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\equalp.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\file-stream.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\file.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\format.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\gfunc.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\hash.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\init.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\interface.c
# ADD CPP /I "../../../../c/gc"
# SUBTRACT CPP /D "SMALL_CONFIG" /D "SILENT" /D "ALL_INTERIOR_POINTERS" /D "__STDC__"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\kernel.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\list.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\method-comb.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\method-init.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\methods.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\miscel.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\mop-init.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\mop.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\num-conv.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\numbers.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\package.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\parameters.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\pathname.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\pkg.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\predicates.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\pretty.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\print-object.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\printer.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\random.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\reader.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\resource.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\search.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\seq-mod.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\sequence.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\set.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\sort.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\static-class.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\statics-add.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\statics-ext.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\statics-int.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\statics.h
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\stream.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\string.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\struct-run.c"
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\symbol.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\tree.c
# End Source File
# Begin Source File

SOURCE=..\..\..\..\c\trig.c
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\type-mops.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\type-ops.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\type-seq.c"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\c\types-run.c"
# End Source File
# End Target
# End Project
