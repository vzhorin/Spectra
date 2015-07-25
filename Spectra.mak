# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Spectra - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Spectra - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Spectra - Win32 Release" && "$(CFG)" !=\
 "Spectra - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Spectra.mak" CFG="Spectra - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Spectra - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Spectra - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "Spectra - Win32 Debug"
MTL=mktyplib.exe
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "Spectra - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Spectra_"
# PROP BASE Intermediate_Dir "Spectra_"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\Spectra.exe" "$(OUTDIR)\ewald.mod"

CLEAN : 
	-@erase ".\Release\ewald.mod"
	-@erase ".\Release/apk.mod"
	-@erase ".\Release\Spectra.exe"
	-@erase ".\Release\data.obj"
	-@erase ".\Release/spinc.mod"
	-@erase ".\Release\pardia.obj"
	-@erase ".\Release\zeeman.obj"
	-@erase ".\Release/dataman.mod"
	-@erase ".\Release\fast.obj"
	-@erase ".\Release\main.obj"
	-@erase ".\Release\toolbar.obj"
	-@erase ".\Release/ctrl.mod"
	-@erase ".\Release\openfile.obj"
	-@erase ".\Release\lattice.obj"
	-@erase ".\Release/openfort.mod"
	-@erase ".\Release\multi.obj"
	-@erase ".\Release/matrix.mod"
	-@erase ".\Release/pardia.mod"
	-@erase ".\Release\matel.mod"
	-@erase ".\Release\fast.mod"
	-@erase ".\Release\ewald.obj"
	-@erase ".\Release\apk.obj"
	-@erase ".\Release\wina.obj"
	-@erase ".\Release/diaproc.mod"
	-@erase ".\Release/toolb.mod"
	-@erase ".\Release\multi.mod"
	-@erase ".\Release\zeeman.mod"
	-@erase ".\Release/lattice.mod"
	-@erase ".\Release/expfit.mod"
	-@erase ".\Release\ctrl.obj"
	-@erase ".\Release\spinc.obj"
	-@erase ".\Release\expfit.obj"
	-@erase ".\Release\matrix.obj"
	-@erase ".\Release\diaproc.obj"
	-@erase ".\Release\matel.obj"
	-@erase ".\Release\Script1.res"
	-@erase ".\Release\Spectra.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Spectra_/" /c /nologo
# ADD F90 /G5 /I "Release/" /c /nologo
# SUBTRACT F90 /Ox /Op /FR /Ob2
F90_PROJ=/G5 /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Script1.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Spectra.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib Comctl32.lib tooltip.lib /nologo /subsystem:windows /incremental:yes /machine:I386
# SUBTRACT LINK32 /profile
LINK32_FLAGS=kernel32.lib Comctl32.lib tooltip.lib /nologo /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/Spectra.pdb" /machine:I386\
 /out:"$(OUTDIR)/Spectra.exe" 
LINK32_OBJS= \
	"$(INTDIR)/data.obj" \
	"$(INTDIR)/pardia.obj" \
	"$(INTDIR)/zeeman.obj" \
	"$(INTDIR)/fast.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/toolbar.obj" \
	"$(INTDIR)/openfile.obj" \
	"$(INTDIR)/lattice.obj" \
	"$(INTDIR)/multi.obj" \
	"$(INTDIR)/ewald.obj" \
	"$(INTDIR)/apk.obj" \
	"$(INTDIR)/wina.obj" \
	"$(INTDIR)/ctrl.obj" \
	"$(INTDIR)/spinc.obj" \
	"$(INTDIR)/expfit.obj" \
	"$(INTDIR)/matrix.obj" \
	"$(INTDIR)/diaproc.obj" \
	"$(INTDIR)/matel.obj" \
	"$(INTDIR)/Script1.res"

"$(OUTDIR)\Spectra.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

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
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\Spectra.exe" "$(OUTDIR)\ewald.mod"

CLEAN : 
	-@erase ".\Debug\ewald.mod"
	-@erase ".\Debug/apk.mod"
	-@erase ".\Debug\Spectra.exe"
	-@erase ".\Debug\diaproc.obj"
	-@erase ".\Debug/spinc.mod"
	-@erase ".\Debug/matrix.mod"
	-@erase ".\Debug/dataman.mod"
	-@erase ".\Debug/pardia.mod"
	-@erase ".\Debug\matel.mod"
	-@erase ".\Debug\fast.mod"
	-@erase ".\Debug/toolb.mod"
	-@erase ".\Debug/ctrl.mod"
	-@erase ".\Debug\multi.mod"
	-@erase ".\Debug/openfort.mod"
	-@erase ".\Debug\zeeman.mod"
	-@erase ".\Debug\fast.obj"
	-@erase ".\Debug\openfile.obj"
	-@erase ".\Debug\lattice.obj"
	-@erase ".\Debug\zeeman.obj"
	-@erase ".\Debug\multi.obj"
	-@erase ".\Debug\ewald.obj"
	-@erase ".\Debug\wina.obj"
	-@erase ".\Debug/diaproc.mod"
	-@erase ".\Debug/expfit.mod"
	-@erase ".\Debug/lattice.mod"
	-@erase ".\Debug\toolbar.obj"
	-@erase ".\Debug\main.obj"
	-@erase ".\Debug\expfit.obj"
	-@erase ".\Debug\matrix.obj"
	-@erase ".\Debug\ctrl.obj"
	-@erase ".\Debug\apk.obj"
	-@erase ".\Debug\spinc.obj"
	-@erase ".\Debug\pardia.obj"
	-@erase ".\Debug\data.obj"
	-@erase ".\Debug\matel.obj"
	-@erase ".\Debug\Script1.res"
	-@erase ".\Debug\Spectra.ilk"
	-@erase ".\Debug\Spectra.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /G5 /Zi /I "Debug/" /c /nologo
# SUBTRACT F90 /FR /Ob2
F90_PROJ=/G5 /Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/Spectra.pdb" 
F90_OBJS=.\Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Script1.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Spectra.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib Comctl32.lib tooltip.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /profile /incremental:no
LINK32_FLAGS=kernel32.lib Comctl32.lib tooltip.lib /nologo /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/Spectra.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Spectra.exe" 
LINK32_OBJS= \
	"$(INTDIR)/diaproc.obj" \
	"$(INTDIR)/fast.obj" \
	"$(INTDIR)/openfile.obj" \
	"$(INTDIR)/lattice.obj" \
	"$(INTDIR)/zeeman.obj" \
	"$(INTDIR)/multi.obj" \
	"$(INTDIR)/ewald.obj" \
	"$(INTDIR)/wina.obj" \
	"$(INTDIR)/toolbar.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/expfit.obj" \
	"$(INTDIR)/matrix.obj" \
	"$(INTDIR)/ctrl.obj" \
	"$(INTDIR)/apk.obj" \
	"$(INTDIR)/spinc.obj" \
	"$(INTDIR)/pardia.obj" \
	"$(INTDIR)/data.obj" \
	"$(INTDIR)/matel.obj" \
	"$(INTDIR)/Script1.res"

"$(OUTDIR)\Spectra.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "Spectra - Win32 Release"
# Name "Spectra - Win32 Debug"

!IF  "$(CFG)" == "Spectra - Win32 Release"

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\wina.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_WINA_=\
	".\Release/spinc.mod"\
	".\Release/diaproc.mod"\
	".\Release/toolb.mod"\
	".\Release/pardia.mod"\
	".\Release/matrix.mod"\
	".\Release/ctrl.mod"\
	".\Release/expfit.mod"\
	".\Release/lattice.mod"\
	

"$(INTDIR)\wina.obj" : $(SOURCE) $(DEP_F90_WINA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\diaproc.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\toolb.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\lattice.mod" "$(INTDIR)\expfit.mod"


!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_WINA_=\
	".\Debug/spinc.mod"\
	".\Debug/diaproc.mod"\
	".\Debug/toolb.mod"\
	".\Debug/pardia.mod"\
	".\Debug/matrix.mod"\
	".\Debug/ctrl.mod"\
	".\Debug/expfit.mod"\
	".\Debug/lattice.mod"\
	

"$(INTDIR)\wina.obj" : $(SOURCE) $(DEP_F90_WINA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\diaproc.mod" "$(INTDIR)\toolb.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\expfit.mod" "$(INTDIR)\lattice.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\spinc.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

F90_MODOUT=\
	"spinc"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\spinc.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\spinc.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

F90_MODOUT=\
	"spinc"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\spinc.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\spinc.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Script1.rc
DEP_RSC_SCRIP=\
	".\icon1.ico"\
	".\main.bmp"\
	".\four.bmp"\
	".\mainhot.bmp"\
	

"$(INTDIR)\Script1.res" : $(SOURCE) $(DEP_RSC_SCRIP) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\matrix.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_MATRI=\
	".\Release/dataman.mod"\
	".\Release\matel.mod"\
	".\Release/pardia.mod"\
	".\Release/spinc.mod"\
	".\Release\fast.mod"\
	
F90_MODOUT=\
	"matrix"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\matrix.obj" : $(SOURCE) $(DEP_F90_MATRI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\pardia.mod"\
 "$(INTDIR)\matel.mod" "$(INTDIR)\fast.mod"
   $(BuildCmds)

"$(INTDIR)\matrix.mod" : $(SOURCE) $(DEP_F90_MATRI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\pardia.mod"\
 "$(INTDIR)\matel.mod" "$(INTDIR)\fast.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_MATRI=\
	".\Debug/dataman.mod"\
	".\Debug\matel.mod"\
	".\Debug/pardia.mod"\
	".\Debug/spinc.mod"\
	".\Debug\fast.mod"\
	
F90_MODOUT=\
	"matrix"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\matrix.obj" : $(SOURCE) $(DEP_F90_MATRI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\pardia.mod"\
 "$(INTDIR)\matel.mod" "$(INTDIR)\fast.mod"
   $(BuildCmds)

"$(INTDIR)\matrix.mod" : $(SOURCE) $(DEP_F90_MATRI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\pardia.mod"\
 "$(INTDIR)\matel.mod" "$(INTDIR)\fast.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\main.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_MAIN_=\
	".\Release/spinc.mod"\
	".\Spectra.fi"\
	

"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"


!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_MAIN_=\
	".\Debug/spinc.mod"\
	".\Spectra.fi"\
	

"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\diaproc.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_DIAPR=\
	".\Release/spinc.mod"\
	".\Release/dataman.mod"\
	".\Release/matrix.mod"\
	".\Release/pardia.mod"\
	".\Release/openfort.mod"\
	".\Release\zeeman.mod"\
	".\Release/ctrl.mod"\
	".\Release/toolb.mod"\
	".\Release\multi.mod"\
	
F90_MODOUT=\
	"diaproc"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\diaproc.obj" : $(SOURCE) $(DEP_F90_DIAPR) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\dataman.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\toolb.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\multi.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\zeeman.mod"
   $(BuildCmds)

"$(INTDIR)\diaproc.mod" : $(SOURCE) $(DEP_F90_DIAPR) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\dataman.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\toolb.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\multi.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\zeeman.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_DIAPR=\
	".\Debug/spinc.mod"\
	".\Debug/dataman.mod"\
	".\Debug/matrix.mod"\
	".\Debug/pardia.mod"\
	".\Debug/openfort.mod"\
	".\Debug\zeeman.mod"\
	".\Debug/ctrl.mod"\
	".\Debug/toolb.mod"\
	".\Debug\multi.mod"\
	
F90_MODOUT=\
	"diaproc"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\diaproc.obj" : $(SOURCE) $(DEP_F90_DIAPR) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\dataman.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\toolb.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\multi.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\zeeman.mod"
   $(BuildCmds)

"$(INTDIR)\diaproc.mod" : $(SOURCE) $(DEP_F90_DIAPR) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod" "$(INTDIR)\dataman.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\toolb.mod" "$(INTDIR)\ctrl.mod"\
 "$(INTDIR)\multi.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\zeeman.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\data.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_DATA_=\
	".\Release/spinc.mod"\
	
F90_MODOUT=\
	"dataman"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\data.obj" : $(SOURCE) $(DEP_F90_DATA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\dataman.mod" : $(SOURCE) $(DEP_F90_DATA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_DATA_=\
	".\Debug/spinc.mod"\
	
F90_MODOUT=\
	"dataman"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\data.obj" : $(SOURCE) $(DEP_F90_DATA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\dataman.mod" : $(SOURCE) $(DEP_F90_DATA_) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pardia.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_PARDI=\
	".\Release/spinc.mod"\
	
F90_MODOUT=\
	"pardia"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\pardia.obj" : $(SOURCE) $(DEP_F90_PARDI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\pardia.mod" : $(SOURCE) $(DEP_F90_PARDI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_PARDI=\
	".\Debug/spinc.mod"\
	
F90_MODOUT=\
	"pardia"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\pardia.obj" : $(SOURCE) $(DEP_F90_PARDI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\pardia.mod" : $(SOURCE) $(DEP_F90_PARDI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\matel.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

F90_MODOUT=\
	"matel"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\matel.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\matel.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

F90_MODOUT=\
	"matel"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\matel.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\matel.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\toolbar.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_TOOLB=\
	".\Release/spinc.mod"\
	".\Release/ctrl.mod"\
	
F90_MODOUT=\
	"toolb"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\toolbar.obj" : $(SOURCE) $(DEP_F90_TOOLB) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\ctrl.mod"
   $(BuildCmds)

"$(INTDIR)\toolb.mod" : $(SOURCE) $(DEP_F90_TOOLB) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\ctrl.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_TOOLB=\
	".\Debug/spinc.mod"\
	".\Debug/ctrl.mod"\
	
F90_MODOUT=\
	"toolb"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\toolbar.obj" : $(SOURCE) $(DEP_F90_TOOLB) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\ctrl.mod"
   $(BuildCmds)

"$(INTDIR)\toolb.mod" : $(SOURCE) $(DEP_F90_TOOLB) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\ctrl.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ctrl.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

F90_MODOUT=\
	"ctrl"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\ctrl.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ctrl.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

F90_MODOUT=\
	"ctrl"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\ctrl.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\ctrl.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\multi.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_MULTI=\
	".\Release/ctrl.mod"\
	".\Release/spinc.mod"\
	".\Release/matrix.mod"\
	
F90_MODOUT=\
	"multi"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\multi.obj" : $(SOURCE) $(DEP_F90_MULTI) "$(INTDIR)"\
 "$(INTDIR)\ctrl.mod" "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod"
   $(BuildCmds)

"$(INTDIR)\multi.mod" : $(SOURCE) $(DEP_F90_MULTI) "$(INTDIR)"\
 "$(INTDIR)\ctrl.mod" "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_MULTI=\
	".\Debug/ctrl.mod"\
	".\Debug/spinc.mod"\
	".\Debug/matrix.mod"\
	
F90_MODOUT=\
	"multi"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\multi.obj" : $(SOURCE) $(DEP_F90_MULTI) "$(INTDIR)"\
 "$(INTDIR)\ctrl.mod" "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod"
   $(BuildCmds)

"$(INTDIR)\multi.mod" : $(SOURCE) $(DEP_F90_MULTI) "$(INTDIR)"\
 "$(INTDIR)\ctrl.mod" "$(INTDIR)\spinc.mod" "$(INTDIR)\matrix.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\openfile.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_OPENF=\
	".\Release/spinc.mod"\
	
F90_MODOUT=\
	"openfort"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\openfile.obj" : $(SOURCE) $(DEP_F90_OPENF) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\openfort.mod" : $(SOURCE) $(DEP_F90_OPENF) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_OPENF=\
	".\Debug/spinc.mod"\
	
F90_MODOUT=\
	"openfort"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\openfile.obj" : $(SOURCE) $(DEP_F90_OPENF) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\openfort.mod" : $(SOURCE) $(DEP_F90_OPENF) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\apk.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

F90_MODOUT=\
	"apk"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\apk.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\apk.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

F90_MODOUT=\
	"apk"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\apk.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\apk.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\lattice.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_LATTI=\
	".\Release/apk.mod"\
	".\Release/openfort.mod"\
	".\Release/spinc.mod"\
	
F90_MODOUT=\
	"lattice"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\lattice.obj" : $(SOURCE) $(DEP_F90_LATTI) "$(INTDIR)"\
 "$(INTDIR)\apk.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\lattice.mod" : $(SOURCE) $(DEP_F90_LATTI) "$(INTDIR)"\
 "$(INTDIR)\apk.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_LATTI=\
	".\Debug/apk.mod"\
	".\Debug/openfort.mod"\
	".\Debug/spinc.mod"\
	
F90_MODOUT=\
	"lattice"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\lattice.obj" : $(SOURCE) $(DEP_F90_LATTI) "$(INTDIR)"\
 "$(INTDIR)\apk.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\spinc.mod"
   $(BuildCmds)

"$(INTDIR)\lattice.mod" : $(SOURCE) $(DEP_F90_LATTI) "$(INTDIR)"\
 "$(INTDIR)\apk.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\spinc.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\zeeman.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_ZEEMA=\
	".\Release/dataman.mod"\
	
F90_MODOUT=\
	"zeeman"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\zeeman.obj" : $(SOURCE) $(DEP_F90_ZEEMA) "$(INTDIR)"\
 "$(INTDIR)\dataman.mod"
   $(BuildCmds)

"$(INTDIR)\zeeman.mod" : $(SOURCE) $(DEP_F90_ZEEMA) "$(INTDIR)"\
 "$(INTDIR)\dataman.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_ZEEMA=\
	".\Debug/dataman.mod"\
	
F90_MODOUT=\
	"zeeman"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\zeeman.obj" : $(SOURCE) $(DEP_F90_ZEEMA) "$(INTDIR)"\
 "$(INTDIR)\dataman.mod"
   $(BuildCmds)

"$(INTDIR)\zeeman.mod" : $(SOURCE) $(DEP_F90_ZEEMA) "$(INTDIR)"\
 "$(INTDIR)\dataman.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\expfit.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_EXPFI=\
	".\Release/spinc.mod"\
	".\Release/dataman.mod"\
	".\Release/matrix.mod"\
	".\Release/pardia.mod"\
	".\Release/openfort.mod"\
	".\Release/diaproc.mod"\
	
F90_MODOUT=\
	"expfit"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\expfit.obj" : $(SOURCE) $(DEP_F90_EXPFI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\matrix.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\diaproc.mod"
   $(BuildCmds)

"$(INTDIR)\expfit.mod" : $(SOURCE) $(DEP_F90_EXPFI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\matrix.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\diaproc.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_EXPFI=\
	".\Debug/spinc.mod"\
	".\Debug/dataman.mod"\
	".\Debug/matrix.mod"\
	".\Debug/pardia.mod"\
	".\Debug/openfort.mod"\
	".\Debug/diaproc.mod"\
	
F90_MODOUT=\
	"expfit"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\expfit.obj" : $(SOURCE) $(DEP_F90_EXPFI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\matrix.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\diaproc.mod"
   $(BuildCmds)

"$(INTDIR)\expfit.mod" : $(SOURCE) $(DEP_F90_EXPFI) "$(INTDIR)"\
 "$(INTDIR)\spinc.mod" "$(INTDIR)\dataman.mod" "$(INTDIR)\matrix.mod"\
 "$(INTDIR)\pardia.mod" "$(INTDIR)\openfort.mod" "$(INTDIR)\diaproc.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\fast.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

F90_MODOUT=\
	"fast"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\fast.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\fast.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

F90_MODOUT=\
	"fast"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\fast.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\fast.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\overlap.txt

!IF  "$(CFG)" == "Spectra - Win32 Release"

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ewald.f90

!IF  "$(CFG)" == "Spectra - Win32 Release"

DEP_F90_EWALD=\
	".\Release/apk.mod"\
	
F90_MODOUT=\
	"ewald"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\ewald.obj" : $(SOURCE) $(DEP_F90_EWALD) "$(INTDIR)"\
 "$(INTDIR)\apk.mod"
   $(BuildCmds)

"$(INTDIR)\ewald.mod" : $(SOURCE) $(DEP_F90_EWALD) "$(INTDIR)"\
 "$(INTDIR)\apk.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Spectra - Win32 Debug"

DEP_F90_EWALD=\
	".\Debug/apk.mod"\
	
F90_MODOUT=\
	"ewald"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\ewald.obj" : $(SOURCE) $(DEP_F90_EWALD) "$(INTDIR)"\
 "$(INTDIR)\apk.mod"
   $(BuildCmds)

"$(INTDIR)\ewald.mod" : $(SOURCE) $(DEP_F90_EWALD) "$(INTDIR)"\
 "$(INTDIR)\apk.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
