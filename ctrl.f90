module ctrl
use msfwina
 integer :: CCS_TOP = #00000001
 integer :: CLR_NONE =#FFFFFFFF

integer :: ILC_MASK=#0001
integer :: ILC_COLOR=#0000
integer :: ILC_COLORDDB=#00FE
integer :: ILC_COLOR4=#0004
integer :: ILC_COLOR8=#0008
integer :: ILC_COLOR16=#0010
integer :: ILC_COLOR24 =#0018
integer :: ILC_COLOR32 =#0020
 
 type T_NMHDR  
    integer hwndFrom 
    integer idFrom 
    integer code 
end type T_NMHDR

type T_TOOLTIPTEXT
    type(T_NMHDR)     hdr 
    integer     lpszText 
    character*80  szText 
    integer  hinst 
    integer      uFlags 
end type T_TOOLTIPTEXT
 
  type T_TBBUTTON 
    integer iBitmap
    integer idCommand
    byte fsState !byte
    byte fsStyle  !byte
    integer bReserved1,bReserved2 !byte
    integer dwData
    integer iString
end type T_TBBUTTON
integer,parameter, public :: WM_NOTIFY   =#004E
integer,parameter, public :: TTN_NEEDTEXT =520

integer,parameter, public :: TB_ADDBUTTONS     = WM_USER + 20  
integer,parameter, public :: TB_INSERTBUTTON   = WM_USER + 21  
integer,parameter, public :: TB_DELETEBUTTON   = WM_USER + 22  
integer,parameter, public :: TB_SETMAXTEXTROWS = WM_USER + 60
integer,parameter, public :: TBSTYLE_TOOLTIPS = #0100
integer,parameter,public :: TB_ADDSTRING=WM_USER + 28

integer,parameter, public ::  TB_SETINDENT   =WM_USER + 47
integer,parameter, public :: TB_SETIMAGELIST =WM_USER + 48
integer,parameter, public ::  TB_SETHOTIMAGELIST =WM_USER + 52

integer,parameter, public :: PBM_SETRANGE=WM_USER+1
integer,parameter, public :: PBM_SETPOS=WM_USER+2
integer,parameter, public :: PBM_DELTAPOS=WM_USER+3
integer,parameter, public :: PBM_SETSTEP=WM_USER+4
integer,parameter, public :: PBM_STEPIT=WM_USER+5

integer,parameter, public :: SB_SETTEXT=WM_USER+1

integer :: PBS_SMOOTH = 1
integer ::  PBS_VERTICAL=4      

interface
  subroutine  DoToolbarNotify(lpphdr)
  !MS$ATTRIBUTES STDCALL, ALIAS : '_DoToolbarNotify@4' :: DoToolbarNotify
    integer lpphdr
   end subroutine DoToolbarNotify



  integer function ImageList_LoadImage(hi,lpbmp, cx, cGrow,  crMask, uType, uFlags)
 !MS$ATTRIBUTES STDCALL, ALIAS : '_ImageList_LoadImage@28' :: ImageList_LoadImage
    integer  hi 	
    character*(*)  lpbmp 	
    integer  cx,  cGrow 	
    integer  crMask, uType,  uFlags
   end function ImageList_LoadImage

integer(4) function	 CreateStatusWindow(style,lpszText,hwndParent,wID)
!MS$ATTRIBUTES STDCALL, ALIAS : '_CreateStatusWindow@16' :: CreateStatusWindow
!MS$ATTRIBUTES REFERENCE ::  lpszText
  integer style,hwndParent, wID
  character*(*) lpszText  
end function   CreateStatusWindow

integer function ImageList_Create(cx,cy,flags, cInitial, cGrow)	
!MS$ATTRIBUTES STDCALL, ALIAS : '_ImageList_Create@20' :: ImageList_Create
    integer  cx, cy, flags,  cInitial, 	 cGrow	
end function   ImageList_Create

integer function  ImageList_Add(himl,hbmImage,hbmMask) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_ImageList_Add@12' :: ImageList_Add
  integer himl, hbmImage, hbmMask 
end function ImageList_Add  

end interface
     
end module ctrl