interface
integer(4) function WinMain (hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
!MS$ATTRIBUTES STDCALL, ALIAS : '_WinMain@16' :: WinMain
integer(4) hInstance,hPrevInstance, lpszCmdLine, nCmdShow
end function WinMain
end interface
end

integer(4) function WinMain( hInstance, hPrevInstance, lpCmdLine, nCmdShow)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_WinMain@' :: WinMain
use msfwina
use spinc
  include 'Spectra.fi'

integer(4) hInstance, hPrevInstance, nCmdShow, lpCmdLine
logical(4) bret
type (T_MSG)            mesg
integer*4 ret
lpCmdLine = lpCmdLine; nCmdShow=nCmdShow
 
if (hPrevInstance == 0) then
    if (InitApplication(hInstance) == 0) then; WinMain = 0; return; end if
end if
ret=InitInstance(hInstance, nCmdShow) 
if (ret == 0) then;WinMain = 0;return;end if
 
do while (GetMessage(mesg,NULL, 0, 0).NEQV. .FALSE.)
    bret = TranslateMessage(mesg); bret = DispatchMessage(mesg)
end do
WinMain = mesg%wParam
return
end 

 integer*4 function InitApplication (hInstance)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_InitApplication@4' :: InitApplication
use msfwina
use spinc

interface 
integer(4) function  MainWndProc (hWnd,message,wParam,lParam) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
integer     hWnd,message,wParam, lParam
end function MainWndProc

end interface

integer             hInstance
type (T_WNDCLASS)     wc
     wc%lpszClassName =LOC( "Speclass"C)
     wc%lpfnWndProc =LOC( MainWndProc)
     wc%hInstance = hInstance
     wc%hIcon =LoadIcon( hInstance, LOC("ANL"C))
     wc%hCursor = LoadCursor( NULL, IDC_ARROW )
     wc%hbrBackground = GetStockObject(LTGRAY_BRUSH)
     wc%lpszMenuName =LOC("MENU"C)
     wc%cbClsExtra = 0
     wc%cbWndExtra = 0
	 wc%style= IOR(CS_HREDRAW,CS_VREDRAW)
     InitApplication =  RegisterClass(wc)
return 
end

integer*4 function InitInstance (hInstance, nCmdShow)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_InitInstance@8' :: InitInstance
use msfwina
use spinc

interface 
integer(4) function  CmdWndProc (hWnd,message,wParam,lParam) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_CmdWndProc@16' :: CmdWndProc
integer     hWnd,message,wParam, lParam
end function CmdWndProc
end interface

integer*4   hInstance, nCmdShow,  hWnd
integer*4  bret
type (T_WNDCLASS)     cm

     cm%lpszClassName =LOC( "cmdclass"C)
     cm%lpfnWndProc =LOC( CmdWndProc)
     cm%hInstance = hInstance
     cm%hIcon =LoadIcon( hInstance, LOC("ANL"C))
     cm%hCursor = LoadCursor( NULL, IDC_ARROW )
     cm%hbrBackground = GetStockObject(WHITE_PEN)
     cm%lpszMenuName =NULL
     cm%cbClsExtra = 0
     cm%cbWndExtra = 0
	 cm%style= IOR(CS_HREDRAW,CS_VREDRAW)
 !open(1,file="dump.txt")
     bret=  RegisterClass(cm)
 !  write(1,'(i16)') bret

hInst = hInstance; nCmdShow=nCmdShow
ihdc=GetDC(NULL)
nDisplayWidth=GetDeviceCaps(ihdc, HORZRES); 
nDisplayHeight=GetDeviceCaps(ihdc, VERTRES);
i=ReleaseDC(NULL,ihdc)

hWnd = CreateWindow("Speclass"C, "Spectra"C,  &
            WS_OVERLAPPEDWINDOW,                            &
             0, & !CW_USEDEFAULT,                         &
             0, & !                                    &
            nDisplayWidth,nDisplayHeight,&
            NULL,                                                 &
            NULL,                                                 &
            hInstance, NULL )                                                          
 !write(1,'(i16)') hWnd

if (hWnd == 0) then;InitInstance = 0; return;end if
!i=MoveWindow(hWnd, 0, 0,  nDisplayWidth,nDisplayHeight/4,  .False. )
bret = ShowWindow(hWnd, SW_SHOWNORMAL);bret = UpdateWindow(hWnd)

hcmdWnd=hcmdWnd
!hcmdWnd = CreateWindow("cmdclass"C, "cmd"C, &
!        WS_OVERLAPPEDWINDOW, CW_USERDEFAULT,  0, & 
!        CW_USERDEFAULT,0,hWnd, NULL,hInstance, NULL )                                       
!write(1,'(i16)') hcmdWnd
!close(1)

! bret = ShowWindow(hcmdWnd, SW_SHOWNORMAL) !;bret = UpdateWindow(hcmdWnd)
! bret=SetFocus(hWnd)
InitInstance = hWnd
return
end 

