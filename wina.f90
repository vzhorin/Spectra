!	 ***********************************************************************
!
!    FUNCTION: MainWndProc(HWND, unsigned, WORD, LONG)
!
!    PURPOSE:  Processes messages
!
!    MESSAGES:
!
!        WM_COMMAND    - application menu (About dialog box)
!        WM_CREATE     - create window and objects
!        WM_PAINT      - update window, draw objects
!        WM_DESTROY    - destroy window

!****************************************************************************/

integer*4 function MainWndProc(hWnd, message, wParam, lParam)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_MainWndProc@16' :: MainWndProc
use msfwina
use spinc
use diaproc
use toolb
use pardia
use matrix
use ctrl
use msflib
use expfit
use lattice

integer*4   hWnd,  wParam, lParam, message
 character*100  lpszHelpFileName, lpszContents, lpszMessage
character*100  lpszHeader
integer*4 hdc,hmenu, handletext
type(T_PAINTSTRUCT) ps
type(T_RECT) rect


!character*60 buffer
!character cie
integer,parameter :: ibrushcolor1=#00b6b094
integer,parameter :: ibrushcolor2=#00eeeddd


interface 
integer(4) function  About (hDlg,message,wParam,lParam) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_About@16' :: About
integer     hDlg,   message,   wParam,     lParam
end function About

end interface

integer*4  ret, hcmdWnd

select case (message) 
	case (WM_CREATE)
 	   hMWindow=hWnd
	  hWndTB=InitToolBar(hWnd)
	  hWndSTB=InitStatusBar(hWnd)
	  hmenu=GetMenu(hWnd)
	  ret=deActivateMenu(hmenu)
      ret=DialogBox(hInst,LOC("NEW"C),hWnd,LOC(newfunc))
	  ibrush=ibrushcolor1
	  ret=GetWindowRect(hWnd,rect)
	!   hcmdWnd = CreateWindow("cmdclass"C, "cmd"C, &
    !    IOR(WS_OVERLAPPEDWINDOW, WS_MINIMIZE), rect.left, rect.bottom-50, rect.left-60, rect.bottom-40,&
!		hWnd, NULL,hInst, NULL )     
!	   hcmd=hcmdWnd     
 !	   ret = ShowWindow(hcmdWnd, SW_SHOWDEFAULT) 
 !      ret = UpdateWindow(hcmdWnd)

	   !ret=AllocConsole()
	   !ret=SetConsoleTitle("Command processor"C)
   	case(WM_PAINT)
	 hdc=BeginPaint(hWnd,ps)
     ret=GetClientRect(hWnd,rect)
   	 ibrushcolor=CreateSolidBrush(ibrush)
	 ret=FillRect(hdc,rect,ibrushcolor)
!	 ret=DrawText(hdc,"Welcome to SPECTRA"C,-1,rect,IOR(DT_TOP,DT_CENTER))
!	 cie='f';if(ie.ne.3) cie='d'
!	 write(buffer,*) 'Number of electrons in ',cie,'-shell is set to',nelec
!	  ret=DrawText(hdc,buffer,-1,rect,IOR(DT_SINGLELINE,IOR(DT_CENTER,DT_VCENTER)))
	 ret=EndPaint(hWnd,ps)
!	  ret=SendMessage(hWndSTB,SB_SETTEXT,0,LOC("Ready"C))
	case(WM_SIZE)
 !******Moving tool bar********************************************
	 ret=SendMessage(hWndTB,WM_USER+33,0,0)
	 ret=SendMessage(hWndSTB,SB_SETTEXT,0,LOC("Ready"C))

 !******Moving status bar********************************************
	 ixParent=INT4(LOWORD(lParam));	 iyParent=INT4(HIWORD(lParam))
	 ix=0;iy=0;inx=0;iny=0
	 ret=GetWindowRect(hWndSTB,rect)
	 iny=rect.bottom-rect.top
	 ix=0;iy=iyParent-iny;inx=ixParent
	 ret=MoveWindow(hWndSTB,ix,iy,inx,iny,.TRUE.)
 !*****************************************************************
    case (WM_COMMAND)
	 	 
	 select case ( INT4(LOWORD(wParam)))
	   case (ID_HELPCONTENT)
               lpszHelpFileName =curdirpath//'\\Spectra.hlp'C
               lpszContents = 'CONTENTS'C
               if (.not.WinHelp(hWnd, lpszHelpFileName, HELP_CONTENTS, &
                               LOC(lpszContents))) then
               lpszMessage = 'Unable to activate help'C
               lpszHeader = 'Spectra'C
               ret = MessageBox(hWnd,lpszMessage,lpszHeader, &
                                 IOR(MB_SYSTEMMODAL,&
                                 IOR(MB_OK, MB_ICONHAND)))
               end if   
       case(ID_ABOUT) 
            ret = DialogBox(hInst, LOC("ABOUT"C), hWnd, LOC(About))
		   ret = UpdateWindow(hWnd)
		case(ID_EXIT)
		    ret=SendMessage(hWnd,WM_CLOSE,0,0);return
	   !case(ID_CONFIGURATION);ret = DialogBox(hInst, LOC("CONFIG"C), hWnd, LOC(Config));ret=InvalidateRect(hWnd,rect,.FALSE.)
	   case(ID_MATRIX)
		   ret=DialogBox(hInst,LOC("REDUCED"C),hWnd,LOC(Reduced))
	   	   ret = UpdateWindow(hWnd)
	   case(ID_ENERGY_LEVELS)
		   ret=CreateDialog(hInst, LOC("RESULTS"C), hWnd, LOC(results))
		   !ret=GetWindowRect(hWnd,rect)
		   !ret=InvalidateRect(hWnd,rect,.TRUE.)
		   !ret=SendMessage(hWnd,WM_PAINT,0,0)
		   ret = UpdateWindow(hWnd)

 	   case(ID_CALCULATE)
	
		 ret=SendMessage(hWndSTB,SB_SETTEXT,0,LOC("Calculation is in progress..."C))
	     ireth=CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(calc),0,0,LOC(IThread))
		 ! ret=calc()
	     ! ret=InvalidateRect(hWnd,rect,.FALSE.)
	 	 !  ret = UpdateWindow(hWnd)
  	   case(ID_STOPIT)
		  ret=MessageBox(NULL,"This action may damage program data"C,"Warning!"C,IOR(MB_OKCANCEL,MB_ICONSTOP))
	      select case(INT4(LoWord(wparam)))
		  case(IDOK)
		   ret=TerminateThread(ireth,i)
		   deallocate(rcrb,respinb,relb)
		  end select
	   case(ID_FIT)
		   hWndfit=hWnd
		   fastfitflag=0
		   ret=SendMessage(hWndSTB,SB_SETTEXT,0,LOC("Fitting is in progress..."C))
		   ireth=CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(fitexp),0,0,LOC(IThread))
		   ret= DELFILESQQ("C:\\"//"Spectra\\"//"Dump\\"//"*"//"."//"*")
	   case(ID_FASTFIT)
		hWndfit=hWnd
		ret=SendMessage(hWndSTB,SB_SETTEXT,0,LOC("Fast Fit is in progress..."C))
		fastfitflag=1
        ireth=CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(fitexp),0,0,LOC(IThread))
!	    ireth=fastmatj()
!		ireth=sortfast()
		ret= DELFILESQQ("C:\\"//"Spectra\\"//"Dump\\"//"*"//"."//"*")
! 		ret=DialogBox(hInst, LOC("RESULTS"C), hWndc, LOC(results))
	   case(ID_FIELD_CALCULATE)
		ret=MessageBox(NULL,"Do you want to build a nearest environment for RE ion?"C,"Site Selection"C,MB_YESNO)
		if(ret.eq.IDYES) then
			ret=makelat()
		else
		end if
		ret=MessageBox(NULL,"Choose the nearest environment file with .xyz extension"C,"Site Selection"C,MB_ICONINFORMATION)
!Insert code for calculating CF parameters	     
! First step		- get information about crystalline environment 
		 ret=getLigands()
		 if(ret.ne.-1) then
! Second step - analyze environment and fill data structures for RE-ligand parameters
			ret=doAnalysis()
			if(ret.ne.-1) then
! Third step    - do calculations
				ret=calculateCFP()
				ret=MessageBox(NULL,"Do you want to save CFP in a record?"C,"CFP record"C,MB_YESNO)
				if(ret.eq.IDYES) then
					ret=DialogBox(hInst,LOC("FILETREENEW"C),hWnd,LOC(BpkCalc))
		        else
		        end if
			 else
			 end if
		 else
		 end if

	     ! ret=MessageBox(NULL,"Not implemented yet"C,""C,MB_OK)
	   case(ID_COPY)
	     ret=MessageBox(NULL,"Not implemented yet"C,""C,MB_OK)
	   case(ID_OPTIONS)
	       ret=DialogBox(hInst,LOC("OPTIONS"C),hWnd,LOC(Options))
	   	   ret = UpdateWindow(hWnd)
	   case(ID_PFREEION_NEW)
	       ret=DialogBox(hInst,LOC("PARAMF"C),hWnd,LOC(Paramf))
	   case(ID_PFREEION_OPEN)
	       call pfreeopen(hWnd)
	   case(ID_PCR_NEW)
	       ret=DialogBox(hInst,LOC("PARAMCR"C),hWnd,LOC(Paramcr))
	   case(ID_PCR_OPEN)
	       call pcropen(hWnd)
	   case(ID_PAD_NEW)
	       ret=DialogBox(hInst,LOC("PARAD"C),hWnd,LOC(Parad))
	   case(ID_PAD_OPEN)
	       call paradopen(hWnd)
	   case(ID_PSAVE)
	      call psave()
	   case(ID_FILECLOSE)
		ifastmat=0
		ret=deActivateMenu(hmenu);ret=RemoveToolBar(hWndTB)
		ibrush=ibrushcolor1
		ret=GetClientRect(hWnd,rect);ret=InvalidateRect(hWnd,rect,.TRUE.)
		deallocate(rcrb,respinb,relb)
		ret=DELFILESQQ("C:\\"//"Spectra\\"//"Dump\\"//"*"//"."//"*")
		menuenabling=0;	ret=SendMessage(hWnd, WM_COMMAND,0,0)
	   case(ID_FILEOPEN)
		   ret=DialogBox(hInst,LOC("FILETREENEW"C),hWnd,LOC(filetree))
		   if(icurrent.ne.0) then 
		    if(menuenabling.ne.2) menuenabling=1;call Initvse(); call FreeIon();call crfpar()
			!handletext=GetDC(hcmd); ret=TextOut(handletext,30,30,vsecurrent.desc//" ",5);ret=ReleaseDC(hcmd,handletext)

		   end if
		   if(menuenabling.eq.1.and.icurrent.ne.0) then
	        hmenu=GetMenu(hWnd);ret=reActivateMenu(hmenu);ret=AddToolBar(hWndTB)
			ibrush=ibrushcolor2
			ret = UpdateWindow(hWnd)
			ret=DialogBox(hInst,LOC("PROGRESSDB"C),hWnd,LOC(progress))
		    menuenabling=2; 
			ret=GetWindowRect(hWnd,rect)
			ret=InvalidateRect(hWnd,rect,.TRUE.)
	    	ret=UpdateWindow(hWnd)
		   end if
		  ret =UpdateWindow(hWnd)
	   case(ID_SHOWCMD)
		  ret=GetClientRect(hWnd,rect)
		  hcmdWnd = CreateWindow("cmdclass"C, "cmd"C, &
         	WS_OVERLAPPEDWINDOW, rect.left, rect.bottom+55, rect.left+60, rect.bottom+155,&
		        hWnd, NULL,hInst, NULL )     
	       hcmd=hcmdWnd; ret = ShowWindow(hcmdWnd, SW_SHOWDEFAULT); ret = UpdateWindow(hcmdWnd)
	   case(ID_FILENEW)
		   ret=DialogBox(hInst,LOC("FILETREENEW"C),hWnd,LOC(filetreen))
	 !  case(ID_FILESAVE)
	 !  case(ID_FILESAVEAS)
	   case default	   
	      MainWndProc = DefWindowProc(hWnd, message, wParam, lParam)
        return
       end select  
 	 case(WM_NOTIFY)
		call DoToolbarNotify(lParam)
    case (WM_DESTROY)                      
           call PostQuitMessage(0)
    case DEFAULT
           MainWndProc = DefWindowProc(hWnd, message, wParam, lParam)
        return
end select
MainWndProc = 0
return
end 

!/****************************************************************************
!
!    FUNCTION: About(HWND, unsigned, WORD, LONG)
!
!    PURPOSE:  Processes messages for "About" dialog box
!
!    MESSAGES:
!
!        WM_INITDIALOG - initialize dialog box
!        WM_COMMAND    - Input received
!
!****************************************************************************/

integer*4 function About( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_About@16' :: About
 use msfwina
use spinc
use diaproc

integer*4   hDlg, message, wParam, lParam
 integer*4   hfontDlg
save        hfontDlg
integer     dwVerHnd, dwVerInfoSize, uVersionLen, bRetCode
character*256   szFullPath, szResult, szGetName,  lpversion
integer*4           lpstrVffInfo,     hMem
character*100       lpszTempBuffer
logical(4)  bret

 !call CenterWindow (hDlg, GetWindow(hDlg, GW_OWNER))
LPARAM = LPARAM
select case (message)
    case (WM_INITDIALOG)
        About = 1
 !****************************************************************************
  ! message: initialize dialog box
         ! Create a font to use
         lpszTempBuffer = ""C
         hfontDlg = CreateFont(14, 0, 0, 0, FW_BOLD, 0, 0, 0, 0, 0, 0, 0,& 
                        INT(FF_SWISS), ""C)
         ! Get version information from the application
         ret = GetModuleFileName (INT(hInst), szFullPath,len(szFullPath))
         dwVerInfoSize = GetFileVersionInfoSize(szFullPath,LOC(dwVerHnd))
         if (dwVerInfoSize .NE. 0) then
            ! If we were able to get the information, process it:
            hMem = GlobalAlloc(GMEM_MOVEABLE, INT(dwVerInfoSize))
            lpstrVffInfo  = GlobalLock(hMem)
            ret = GetFileVersionInfo (szFullPath, dwVerHnd, dwVerInfoSize, lpstrVffInfo)

            ! Walk through the dialog items that we want to replace:
            do i = DLG_VERFIRST, DLG_VERLAST
               bret = GetDlgItemText(hDlg, i, szResult,      &     
                             len(szResult))
               szGetName = "\\StringFileInfo\\000004B0\\"C               
               bret =lstrcat(szGetName,szResult)
           bRetCode=VersionQueryValue(lpstrVffInfo,LOC(szGetName),LOC(lpVersion),       &
                                            LOC(uVersionLen)) ! For MIPS strictness
               if ( bRetCode .NE. 0 ) then
                    ! Replace dialog item text with version info
                  bret = lstrcpy(szResult,lpVersion)
                  retval = SetDlgItemText(hDlg, i,szResult)
                  retval = SendMessage (GetDlgItem (hDlg, i),   &
                                   WM_SETFONT, hfontDlg, TRUE)
               end if
            end do 
!****************************************************************************************
	  retval = GlobalUnlock(hMem)
            retval = GlobalFree(hMem)
         end if 
         About= 1
        return
    case (WM_COMMAND)
        if ((LoWord(wParam) == INT2(IDOK)) .OR.                       &
                (LoWord(wParam) == INT2(IDCANCEL))) then
            bret = EndDialog(hDlg, TRUE)
		  bret = DeleteObject (hfontDlg)
            About = 1
            return
        end if
end select
About = 0
return
end 

integer*4 function CmdWndProc(hcmdWnd, message, wParam, lParam)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_CmdWndProc@16' :: CmdWndProc
use msfwina
use spinc

integer*4  message, wParam, lParam
integer*4 ret, hown, hcmdWnd

select case (message) 
	case (WM_CREATE)
  	 hown=GetWindow(hcmdWnd,GW_OWNER)

 !   case(WM_PAINT)
!    case (WM_COMMAND)

!	 ret=SendMessage(hown,WM_COMMAND,wParam,lParam)
!   	  select case ( INT4(LOWORD(wParam)))
!	   case DEFAULT
 !          CmdWndProc = SendMessage(hown,WM_COMMAND,wParam,lParam)
!DefWindowProc(hcmdWnd, message, wParam, lParam)
!		   return
!	  end select
   case (WM_DESTROY)         
           ret=DestroyWindow(hcmdWnd)     
    case DEFAULT
         !  CmdWndProc =	SendMessage(hown,message,wParam,lParam)
		   CmdWndProc=DefWindowProc(hcmdWnd, message, wParam, lParam)
    return
 end select

CmdWndProc = 0;return
end 

