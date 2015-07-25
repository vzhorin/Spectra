module openfort
use msfwina
use spinc
type (T_OPENFILENAME)       oflev
type (T_OPENFILENAME)       ofdes
type (T_OPENFILENAME)       oflat
type (T_OPENFILENAME)       ofexp


character*(MAX_PATH)        ofbuffer
character*(MAX_PATH)        filter
character*(MAX_PATH)        CurrentDir
character*(MAX_PATH)        CurrentLev
character*(MAX_PATH)        CurrentDes
character*(MAX_PATH)        CurrentLAT
character*(MAX_PATH)        CurrentExp


contains

integer function Initoflev()
integer ret
ofbuffer(1:1) = char(0)
ret = lstrcpy(filter,"Levels (*.LEV)|*.LEV||"C)
call convertfilterstring(filter)
ret=SetCurrentDirectory(curdirpath)
ret = GetCurrentDirectory(MAX_PATH,CurrentDir)

i=lstrlen(CurrentDir)
CurrentLev=CurrentDir(1:i)//"\\Lev"C
   oflev%lStructSize       =76 !sizeof (OPENFILENAME)
   oflev%hwndOwner         = NULL
   oflev%hInstance         = hInst
   oflev%lpstrFilter       = LOC(filter)
   oflev%lpstrCustomFilter = NULL
   oflev%nMaxCustFilter    = 0
   oflev%nFilterIndex      = 0
   oflev%lpstrFile         = LOC(ofbuffer)
   oflev%nMaxFile          = MAX_PATH
   oflev%lpstrFileTitle    = NULL
   oflev%nMaxFileTitle     = 0
   oflev%lpstrInitialDir   = LOC(CurrentLev)
   oflev%lpstrTitle        = NULL
   oflev%Flags             = OFN_HIDEREADONLY
   oflev%nFileOffset       = 0
   oflev%nFileExtension    = 0
   oflev%lpstrDefExt       = NULL
   oflev%lCustData         = 0
   oflev%lpfnHook          = NULL
   oflev%lpTemplateName    = NULL
   if (.not.GetOpenFileName (oflev)) then              
         Initoflev=0;
		 ret=SetCurrentDirectory(curdirpath)
return 
   end if
 ret=SetCurrentDirectory(curdirpath)
Initoflev=1;return
end function Initoflev

integer function Initofdes(hWnd)
integer ret, hWnd
ofbuffer(1:1) = char(0)
ret = lstrcpy(filter,"Impurity ion (*.DES)|*.DES||"C)
call convertfilterstring(filter)
ret=SetCurrentDirectory(curdirpath)
ret = GetCurrentDirectory(MAX_PATH,CurrentDir)

i=lstrlen(CurrentDir)
CurrentDes=CurrentDir(1:i)//"\\Des"C
   ofdes%lStructSize       =76 !sizeof (OPENFILENAME)
   ofdes%hwndOwner         = hWnd
   ofdes%hInstance         = hInst
   ofdes%lpstrFilter       = LOC(filter)
   ofdes%lpstrCustomFilter = NULL
   ofdes%nMaxCustFilter    = 0
   ofdes%nFilterIndex      = 0
   ofdes%lpstrFile         = LOC(ofbuffer)
   ofdes%nMaxFile          = MAX_PATH
   ofdes%lpstrFileTitle    = NULL
   ofdes%nMaxFileTitle     = 0
   ofdes%lpstrInitialDir   = LOC(CurrentDes)
   ofdes%lpstrTitle        = NULL
   ofdes%Flags             = OFN_HIDEREADONLY
   ofdes%nFileOffset       = 0
   ofdes%nFileExtension    = 0
   ofdes%lpstrDefExt       = NULL
   ofdes%lCustData         = 0
   ofdes%lpfnHook          = NULL
   ofdes%lpTemplateName    = NULL
   if (.not.GetOpenFileName (ofdes)) then              
         Initofdes=0;
		 ret=SetCurrentDirectory(curdirpath)
return 
   end if
 ret=SetCurrentDirectory(curdirpath)
Initofdes=1;return
end function Initofdes


integer function Initofexp(hWnd)
integer ret,hWnd
ofbuffer(1:1) = char(0)
ret = lstrcpy(filter,"Experimental levels (*.exp)|*.exp||"C)
call convertfilterstring(filter)
ret=SetCurrentDirectory(curdirpath)
ret = GetCurrentDirectory(MAX_PATH,CurrentDir)

i=lstrlen(CurrentDir)
CurrentExp=CurrentDir(1:i)//"\\Exp"C
   ofexp%lStructSize       =76 !sizeof (OPENFILENAME)
   ofexp%hwndOwner         = hWnd
   ofexp%hInstance         = hInst
   ofexp%lpstrFilter       = LOC(filter)
   ofexp%lpstrCustomFilter = NULL
   ofexp%nMaxCustFilter    = 0
   ofexp%nFilterIndex      = 0
   ofexp%lpstrFile         = LOC(ofbuffer)
   ofexp%nMaxFile          = MAX_PATH
   ofexp%lpstrFileTitle    = NULL
   ofexp%nMaxFileTitle     = 0
   ofexp%lpstrInitialDir   = LOC(CurrentExp)
   ofexp%lpstrTitle        = NULL
   ofexp%Flags             = OFN_HIDEREADONLY
   ofexp%nFileOffset       = 0
   ofexp%nFileExtension    = 0
   ofexp%lpstrDefExt       = NULL
   ofexp%lCustData         = 0
   ofexp%lpfnHook          = NULL
   ofexp%lpTemplateName    = NULL
   if (.not.GetOpenFileName (ofexp)) then              
         Initofexp=0;
		 ret=SetCurrentDirectory(curdirpath)
return 
   end if
 ret=SetCurrentDirectory(curdirpath)
Initofexp=1;return
end function Initofexp


integer function Initoflat()
integer ret
ofbuffer(1:1) = char(0)
ret = lstrcpy(filter,"Lattice (*.LAT)|*.LAT| Nearest environment (*.xyz)|*.xyz||"C)
call convertfilterstring(filter)
ret=SetCurrentDirectory(curdirpath)
ret = GetCurrentDirectory(MAX_PATH,CurrentDir)

i=lstrlen(CurrentDir)
CurrentLat=CurrentDir(1:i)//"\\Lat"C
   oflat%lStructSize       =76 !sizeof (OPENFILENAME)
   oflat%hwndOwner         = NULL
   oflat%hInstance         = hInst
   oflat%lpstrFilter       = LOC(filter)
   oflat%lpstrCustomFilter = NULL
   oflat%nMaxCustFilter    = 0
   oflat%nFilterIndex      = 0
   oflat%lpstrFile         = LOC(ofbuffer)
   oflat%nMaxFile          = MAX_PATH
   oflat%lpstrFileTitle    = NULL
   oflat%nMaxFileTitle     = 0
   oflat%lpstrInitialDir   = LOC(CurrentLat)
   oflat%lpstrTitle        = NULL
   oflat%Flags             = OFN_HIDEREADONLY
   oflat%nFileOffset       = 0
   oflat%nFileExtension    = 0
   oflat%lpstrDefExt       = NULL
   oflat%lCustData         = 0
   oflat%lpfnHook          = NULL
   oflat%lpTemplateName    = NULL
   if (.not.GetOpenFileName (oflat)) then              
         Initoflat=0;
		 ret=SetCurrentDirectory(curdirpath)
return 
   end if
 ret=SetCurrentDirectory(curdirpath)
Initoflat=1;return
end function Initoflat


end module openfort