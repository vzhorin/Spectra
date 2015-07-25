module toolb
use msfwina
use spinc
use ctrl
external   InitCommonControls
!MS$ATTRIBUTES stdcall, ALIAS : '_InitCommonControls@0' ::  InitCommonControls
!MS$OBJCOMMENT LIB: "Comctl32.lib"
interface
integer(4) function  CreateToolbarEx(hwnd,ws, wID,nBitmaps,	&
                        hBMInst, wBMID,  lpButtons,iNumButtons,dxButton, dyButton,&
                         dxBitmap, dyBitmap, uStructSize ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_CreateToolbarEx@52' :: CreateToolbarEx
!MS$ATTRIBUTES REFERENCE ::  lpButtons
 use ctrl
 integer hwnd, ws, wID,wBMID, nBitmaps, hBMInst ,uStructSize
 type(T_TBBUTTON) :: lpButtons
 integer iNumButtons,dxButton, dyButton,dxBitmap, dyBitmap
end function CreateToolbarEx
end interface

contains

integer*4 function InitToolBar(hWnd)
integer*4 hWnd
type(T_TBBUTTON), dimension(4)  :: tbb
integer numbitmap
integer hBMP,ret,isep,hBMPHOT,hlist
type(T_TBBUTTON) :: tbsep

numbitmap=10
call InitCommonControls()
tbb(1).iBitmap=0;tbb(2).iBitmap=1;tbb(3).iBitmap=10;tbb(4).iBitmap=9
tbb(1).idCommand=ID_FILENEW;tbb(2).idCommand=ID_FILEOPEN;tbb(3).idCommand=ID_HELPCONTENT
tbb(4).idCommand=ID_ENERGY_LEVELS
tbb.fsState=4
tbsep.fsState=#4;tbsep.fsStyle=1
isep=LOC(tbsep)
hBMP=LoadBitMap(hInst, LOC("TOOLB"C))
hBMPHOT=LoadBitMap(hInst, LOC("TOOLBHOT"C))

hlist=ImageList_Create(22,22,ILC_COLOR16,0,1)	
ret=ImageList_Add(hlist,hBMPHOT,0)

InitToolBar=CreateToolbarEx(hWnd,IOR(TBSTYLE_TOOLTIPS,IOR(#800,IOR(WS_DLGFRAME,IOR(WS_VISIBLE,WS_CHILD)))), & 
     1,numbitmap,0,hBMP,tbb(1),1,0,0,22,22,1)

ret=SendMessage(InitToolBar,TB_SETINDENT,10,0)
ret=SendMessage(InitToolBar,TB_SETHOTIMAGELIST,0,hlist)
!ret=SendMessage(InitToolBar,TB_SETIMAGELIST,0,hBMP)

ret=SendMessage(InitToolBar,TB_ADDBUTTONS,1,LOC(tbb(2)))	 !TB_ADDBUTTONS =WM_USER + 20      
ret=SendMessage(InitToolBar,TB_ADDBUTTONS,1,isep)
ret=SendMessage(InitToolBar,TB_ADDBUTTONS,1,LOC(tbb(4)))	
ret=SendMessage(InitToolBar,TB_ADDBUTTONS,1,isep)
            
ret=SendMessage(InitToolBar,TB_ADDBUTTONS,1,LOC(tbb(3)))			            
end function InitToolBar

integer*4 function AddToolBar(hWnd)
integer*4 hWnd,isep
type(T_TBBUTTON), dimension(7)  :: tbb
type(T_TBBUTTON) tbsep
tbb(1).iBitmap=2
tbb(1).idCommand=ID_FILECLOSE

tbb(2).iBitmap=3
tbb(2).idCommand=ID_COPY

tbb(3).iBitmap=4
tbb(3).idCommand=ID_PSAVE

tbb(4).iBitmap=5
tbb(4).idCommand=ID_CALCULATE
tbb(5).iBitmap=6
tbb(5).idCommand=ID_FIT

tbb(6).iBitmap=7
tbb(6).idCommand=ID_STOPIT

tbb(7).iBitmap=8
tbb(7).idCommand=ID_MATRIX

tbb.fsState=4
tbsep.fsState=#4;tbsep.fsStyle=1
isep=LOC(tbsep)
!ret=SendMessage(hWnd,WM_USER + 21,2,isep)	   !TB_INSERTBUTTON 


ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(7)))
ret=SendMessage(hWnd,WM_USER + 21,3,isep)
ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(6)))
ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(5)))
ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(4)))
ret=SendMessage(hWnd,WM_USER + 21,3,isep)	   !TB_INSERTBUTTON 

ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(3)))
ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(2)))
ret=SendMessage(hWnd,WM_USER + 21,3,LOC(tbb(1)))


ret=SendMessage(hWnd,WM_USER + 22,1,0)
AddToolBar=1	
end function  AddToolBar

integer*4 function RemoveToolBar(hWnd)
integer*4 hWnd,isep
type(T_TBBUTTON), dimension(1)  :: tbb
type(T_TBBUTTON) tbsep

tbb(1).iBitmap=1;tbb(1).idCommand=ID_FILEOPEN
tbb.fsState=4
tbsep.fsState=#4;tbsep.fsStyle=1
isep=LOC(tbsep)
ret=SendMessage(hWnd,WM_USER + 22,2,0)	  !TB_DELETEBUTTON 
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)
ret=SendMessage(hWnd,WM_USER + 22,2,0)

ret=SendMessage(hWnd,TB_INSERTBUTTON ,1,LOC(tbb(1)))
RemoveToolBar=1	
end function   RemoveToolBar


integer*4 function InitStatusBar(hWnd)
  integer*4 hWnd

  InitStatusBar=CreateStatusWindow(IOR(WS_CHILD,IOR(WS_VISIBLE,WS_CLIPSIBLINGS)),&
   "Ready"C,hWnd,2)
end function InitStatusBar

end module toolb