module diaproc
use msfwina
use spinc
use dataman
use matrix


integer*4 jfromdialog, hDlgModeless
contains
!/****************************************************************************
!
!    FUNCTION: Config(HWND, unsigned, WORD, LONG)
!
!    PURPOSE:  Processes messages for "Config" dialog box
!
!    MESSAGES:
!
!        WM_INITDIALOG - initialize dialog box
!        WM_COMMAND    - Input received
!
!****************************************************************************/

integer*4 function Config( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_Config@16' :: Config
integer*4   hDlg, message, wParam, lParam
logical(4)  bret
integer*4 iedeft,iet,nelect

call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
LPARAM = LPARAM
select case (message)
   case (WM_INITDIALOG)
	    iedeft=iedef;iet=ie;nelect=nelec;
		bret=CheckRadioButton(hDlg,ID_D1,ID_F13,iedeft)
        Config = 1; return
   case (WM_COMMAND)
    select case(INT4(LoWord(wparam)))
   	case(ID_D1);iet=2;nelect=1;iedeft=ID_D1;    case(ID_D2);iet=2;nelect=2;iedeft=ID_D2
	case(ID_D3);iet=2;nelect=3;iedeft=ID_D3;    case(ID_D4);iet=2;nelect=4;iedeft=ID_D4
	case(ID_D5);iet=2;nelect=5;iedeft=ID_D5;    case(ID_D6);iet=2;nelect=6;iedeft=ID_D6
    case(ID_D7);iet=2;nelect=7;iedeft=ID_D7;    case(ID_D8);iet=2;nelect=8;iedeft=ID_D8
    case(ID_D9);iet=2;nelect=9;iedeft=ID_D9;
  	case(ID_F1);iet=3;nelect=1;iedeft=ID_F1;     case(ID_F2);iet=3;nelect=2;iedeft=ID_F2
	case(ID_F3);iet=3;nelect=3;iedeft=ID_F3;     case(ID_F4);iet=3;nelect=4;iedeft=ID_F4
	case(ID_F5);iet=3;nelect=5;iedeft=ID_F5;     case(ID_F6);iet=3;nelect=6;iedeft=ID_F6
	case(ID_F7);iet=3;nelect=7;iedeft=ID_F7;     case(ID_F8);iet=3;nelect=8;iedeft=ID_F8
	case(ID_F9);iet=3;nelect=9;iedeft=ID_F9;     case(ID_F10);iet=3;nelect=10;iedeft=ID_F10
	case(ID_F11);iet=3;nelect=11;iedeft=ID_F11;case(ID_F12);iet=3;nelect=12;iedeft=ID_F12
	case(ID_F13);iet=3;nelect=13;iedeft=ID_F13
	case(IDOK)
     iedef=iedeft;ie=iet;nelec=nelect
	 bret=EndDialog(hDlg,TRUE)
	 Config=1
	 return
	case(IDCANCEL)
	 bret=EndDialog(hDlg,FALSE)
	 Config=1
	 return
   end select
end select
Config = 0;return
end  function Config


!/****************************************************************************
!
!    FUNCTION: Reduced(HWND, unsigned, WORD, LONG)
!
!    PURPOSE:  Processes messages for "Reduced" dialog box
!
!    MESSAGES:
!
!        WM_INITDIALOG - initialize dialog box
!        WM_COMMAND    - Input received
!
!****************************************************************************/

integer*4 function Reduced( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_Reduced@16' :: Reduced

integer*4   hDlg, message, wParam, lParam
type (T_MSG)            mesg
integer(4)  bret
integer i,nt,ne,iflag,iindex,idi,nr
character(3), allocatable :: cht(:)
character(3) cht1,cht2
!character*8 dbfilestr
character*25 szbuf
character*70 buf
real xtem

LPARAM = LPARAM;iindex=0
!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

select case (message)
  case (WM_INITDIALOG)	
    i=1;nt=1;ne=nelec;iflag=0
    select case(ie)
     case(2)
	 !dbfilestr=dbfilecr(1)
	 if(ne.gt.5) then;iflag=1;ne=10-ne;end if;nt=dcon(ne)%con%nst
 	 case(3)
	 !dbfilestr=dbfilecr(2)
	 if(ne.gt.7) then;iflag=1;ne=14-ne;end if;nt=fcon(ne)%con%nst
    end select	

   do i=ID_MTEXT1,ID_MTEXT3
    bret=SetDlgItemText(hDlg,i,' '//char(0))
   end do
   do i=ID_MRESULT1,ID_MRESULT3
    bret=SetDlgItemText(hDlg,i,' '//char(0))
  end do

!***********filling information boxes for selected configuration********************
   allocate(cht(nt))
	 do i=1,nt
	  if(ie.eq.3) then;cht(i)=fcon(ne)%iLS(3*(i-1)+1:3*i);else
	   cht(i)=dcon(ne)%iLS(3*(i-1)+1:3*i)
      end if
	  bret=SendDlgItemMessage(hDlg,ID_MLIST1,LB_ADDSTRING,0,LOC(cht(i)//CHAR(0)))
      bret=SendDlgItemMessage(hDlg,ID_MLIST2,LB_ADDSTRING,0,LOC(cht(i)//CHAR(0)))
	 end do
	 do i=1,numinter
	   bret=SendDlgItemMessage(hDlg,ID_MLIST3,LB_ADDSTRING,0,LOC(inter(i)//CHAR(0)))
	 end do
	 deallocate(cht)
	  bret=SendDlgItemMessage(hDlg,ID_MLIST1,LB_SETCURSEL,iindex,0)
	  bret=SendDlgItemMessage(hDlg,ID_MLIST2,LB_SETCURSEL,iindex,0)
	  bret=SendDlgItemMessage(hDlg,ID_MLIST3,LB_SETCURSEL,iindex,0)
	  buf='Make selection and press Query button'//char(0)
      bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,buf)
 
	  Reduced = 1
	  return
!***************dialog boxes initialized**************************************
	 case (WM_COMMAND)

	  iindex=SendDlgItemMessage(hDlg,ID_MLIST1,LB_GETCURSEL,0,0)
	  bret=SendDlgItemMessage(hDlg,ID_MLIST1,LB_GETTEXT,iindex,LOC(szbuf))
	  cht1=szbuf(1:3)
	 ! cht1=ToLowCase(cht1)
	  iindex=SendDlgItemMessage(hDlg,ID_MLIST2,LB_GETCURSEL,0,0)
	  bret=SendDlgItemMessage(hDlg,ID_MLIST2,LB_GETTEXT,iindex,LOC(szbuf))
	  cht2=szbuf(1:3)
	 ! cht2=ToLowCase(cht2)
	  iindex=SendDlgItemMessage(hDlg,ID_MLIST3,LB_GETCURSEL,0,0)
      idi=iindex+1
	  bret=SendDlgItemMessage(hDlg,ID_MLIST3,LB_GETTEXT,iindex,LOC(szbuf))
	  buf='You selected '//cht1//'-'//cht2//' matrix element for '//szbuf
	  bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,buf)

     select case(INT4(LoWord(wparam)))
  	   case(IDOK)
	    bret = EndDialog(hDlg, TRUE)
		!deallocate(relb)		  ! rcrb,respinb,relb
        Reduced = 1;return
	   case(ID_MQUERY)
!		write(szbuf,'(2i3)') idi,ne
!		 bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,szbuf)
		 bret=SetDlgItemText(hDlg,ID_MTEXT1," "//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2," "//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3," "//char(0))
		 bret=SetDlgItemText(hDlg,ID_MRESULT1," 0 "//char(0)); bret=SetDlgItemText(hDlg,ID_MRESULT2," 0 "//char(0));bret=SetDlgItemText(hDlg,ID_MRESULT3," 0 "//char(0))
		select case(idi)
		 case(1)
		  bret=SetDlgItemText(hDlg,ID_MTEXT1,"F2"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"F4"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"F6"//char(0))
		  do iel=ID_MRESULT1,ID_MRESULT3
		   nr=0;call numel(iel-ID_MRESULT1+1,cht1,cht2,nr,iflag)
		   if(nr.NE.0) then;rel=relb(nr);xtem=rel.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
		   else; bret=SetDlgItemText(hDlg,iel,"0");end if
		  end do
		 case(2)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"alpha"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"beta"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"gamma"//char(0))
		   do iel=ID_MRESULT1,ID_MRESULT3
		    nr=0;call numel(iel-ID_MRESULT1+4,cht1,cht2,nr,iflag)
		    if(nr.NE.0) then;rel=relb(nr); xtem=rel.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
			else;bret=SetDlgItemText(hDlg,iel,"0"); end if
		   end do
		 case(3)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"t2"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"t3"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"t4"//char(0))
		   do iel=ID_MRESULT1,ID_MRESULT3
		    nr=0;call numel(iel-ID_MRESULT1+7,cht1,cht2,nr,iflag)
		    if(nr.NE.0) then;rel=relb(nr); xtem=rel.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
			else; bret=SetDlgItemText(hDlg,iel,"0"); end if
		   end do
		 case(4)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"t6"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"t7"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"t8"//char(0))
		   do iel=ID_MRESULT1,ID_MRESULT3
		    nr=0;call numel(iel-ID_MRESULT1+10,cht1,cht2,nr,iflag)
		    if(nr.NE.0) then;rel=relb(nr); xtem=rel.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
			else;bret=SetDlgItemText(hDlg,iel,"0");end if
		   end do
		 case(5)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"dzeta"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,""//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,""//char(0))
		   bret=SetDlgItemText(hDlg,ID_MRESULT1," 0 "//char(0)); bret=SetDlgItemText(hDlg,ID_MRESULT2,""//char(0));bret=SetDlgItemText(hDlg,ID_MRESULT3,""//char(0))
		   hDlgModeless=CreateDialog(hInst,LOC("J-VALUE"C),hDlg,LOC(jval))
		   do while (GetMessage(mesg, NULL, 0, 0).and.(hDlgModeless.ne.0))
		    if(IsDialogMessage(hDlgModeless,mesg)) then
        	  nr=0;call numspin(jfromdialog,1,cht1,cht2,nr,iflag)
		      if(nr.NE.0) then;rsp=respinb(nr); xtem=rsp.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,ID_MRESULT1,buf)
			  else;bret=SetDlgItemText(hDlg,ID_MRESULT1,"0"); end if
		      write(buf,'(i4)') jfromdialog 
	          bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,"The value of J for this query is "//buf//char(0))
		  	end if
           end do
		 case(6)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"M0"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"M2"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"M4"//char(0))
		   bret=SetDlgItemText(hDlg,ID_MRESULT1," 0 "//char(0)); bret=SetDlgItemText(hDlg,ID_MRESULT2," 0 "//char(0));bret=SetDlgItemText(hDlg,ID_MRESULT3," 0 "//char(0))
		   hDlgModeless=CreateDialog(hInst,LOC("J-VALUE"C),hDlg,LOC(jval))
		   do while (GetMessage(mesg, NULL, 0, 0).and.(hDlgModeless.ne.0))
		    if(IsDialogMessage(hDlgModeless,mesg)) then
			  do iel=ID_MRESULT1,ID_MRESULT3
			   nr=0;call numspin(jfromdialog,iel-ID_MRESULT1+2,cht1,cht2,nr,iflag)
		       if(nr.NE.0) then;rsp=respinb(nr); xtem=rsp.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
			   else; bret=SetDlgItemText(hDlg,iel,"0");end if
			  end do
		      write(buf,'(i4)') jfromdialog 
	          bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,"The value of J for this query is "//buf//char(0))
		  	end if
           end do
		 case(7)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"P2"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"P4"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"P6"//char(0))
		   bret=SetDlgItemText(hDlg,ID_MRESULT1," 0 "//char(0)); bret=SetDlgItemText(hDlg,ID_MRESULT2," 0 "//char(0));bret=SetDlgItemText(hDlg,ID_MRESULT3," 0 "//char(0))
		   hDlgModeless=CreateDialog(hInst,LOC("J-VALUE"C),hDlg,LOC(jval))
		   do while (GetMessage(mesg, NULL, 0, 0).and.(hDlgModeless.ne.0))
		    if(IsDialogMessage(hDlgModeless,mesg)) then
			  do iel=ID_MRESULT1,ID_MRESULT3
			   nr=0;call numspin(jfromdialog,iel-ID_MRESULT1+5,cht1,cht2,nr,iflag)
		       if(nr.NE.0) then;rsp=respinb(nr); xtem=rsp.fk;write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
			   else; bret=SetDlgItemText(hDlg,iel,"0");end if
			  end do
		      write(buf,'(i4)') jfromdialog 
	          bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,"The value of J for this query is "//buf//char(0))
		  	end if
           end do
		 case(8)
		   bret=SetDlgItemText(hDlg,ID_MTEXT1,"U2"//char(0)); bret=SetDlgItemText(hDlg,ID_MTEXT2,"U4"//char(0));bret=SetDlgItemText(hDlg,ID_MTEXT3,"U6"//char(0))
		   bret=SetDlgItemText(hDlg,ID_MRESULT1," 0 "//char(0)); bret=SetDlgItemText(hDlg,ID_MRESULT2," 0 "//char(0));bret=SetDlgItemText(hDlg,ID_MRESULT3," 0 "//char(0))
		   nr=0;call numcr(cht1,cht2,nr,iflag)
		   if(nr.NE.0) then;rcr=rcrb(nr)
		    if(iflag.eq.1) then; buf='lower diagonal case! multiply by (-1)**(L-L)'//char(0);bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,buf);end if
		    do iel=ID_MRESULT1,ID_MRESULT3
   		     xtem=rcr.uk(iel-ID_MRESULT1+1)	
		     write(buf,'(f13.8)') xtem;bret=SetDlgItemText(hDlg,iel,buf)
		    end do
		   else
		    do iel=ID_MRESULT1,ID_MRESULT3
   		     bret=SetDlgItemText(hDlg,iel,"0")
			end do
		   end if
		 case default
		 buf='No data for this query - could be zero'C
		 bret=SetDlgItemText(hDlg,ID_MATRIXVALUE,buf)
 	    end select
  	  ! case(ID_MUPDATE)
	!	bret=update()
	   end select
end select
Reduced = 0
return
end  function Reduced

character*3 function ToLowCase(cht)
character*3 cht 
character c1,c2,c3
c1=cht(1:1);c2=cht(2:2);c3=cht(3:3)
SELECT CASE (c2)
    CASE ( "A":"Z" )
        c2 = CHAR( ICHAR(c2) + ICHAR("a") - ICHAR("A") )
END SELECT
ToLowCase=c1//c2//c3;return
end function ToLowCase

integer*4 function jval(hDlg, message, wParam, lParam )
 !MS$ ATTRIBUTES STDCALL, ALIAS : '_jval@16' :: jval
integer*4  bret
integer*4   hDlg, message, wParam, lParam
integer*4 inelec
integer*4 il,iindex
character*2 buf
character*3 szbuf
lParam=lParam
select case (message)
  case (WM_INITDIALOG)
   inelec=nelec
   select case(ie)
	case(2)
	 if(inelec.gt.5) then;inelec=10-inelec;end if
 	 case(3)
	 if(inelec.gt.7) then;inelec=14-inelec;end if
    end select	
	il=jlistfl(inelec)
	do while(il.le.jlistfu(inelec))
	 write(buf,'(i2)') il 
     bret=SendDlgItemMessage(hDlg,ID_MJLIST,LB_ADDSTRING,0,LOC(buf))
	 il=il+2
	end do
   case (WM_COMMAND)
	  iindex=SendDlgItemMessage(hDlg,ID_MJLIST,LB_GETCURSEL,0,0)
	  bret=SendDlgItemMessage(hDlg,ID_MJLIST,LB_GETTEXT,iindex,LOC(szbuf))
	  buf=szbuf(1:2)
	  read(buf,'(i2)') jfromdialog
   select case(INT4(LoWord(wparam)))
	 case(IDOK)
	  bret=DestroyWindow(hDlg)
	  hDlgModeless=0
	  jval=0
   end select
end select
jval=0
end function jval


!/****************************************************************************
!
!    FUNCTION: calc()
!
!    PURPOSE:  Calculates matrix and finds eigensystem 
!
!    MESSAGES:

!****************************************************************************/

integer function calc( )
use pardia
use matrix


integer*4 hWndc,ret
integer*4 hcur
!character*800 cbuf
!character*1 cad
!character*3 cht

hWndc=hMWindow
open(21,file='debug.txt')

!call FreeIon()
!call crfpar()
if(mansel.eq.1) then
 ret=DialogBox(hInst, LOC("TERMS"C), hWndc, LOC(terms))
 if(ret.eq.0) then 
 calc=1;return
 end if
else
 ret=info()
end if
idatagl=1
!hdc=GetDC(hcmd);ret=TextOut(hdc,30,30,"Building LSJ matrix..."C,22);bret=ReleaseDC(hcmd,hdc)
hcur=hcur !LoadCursor( NULL, IDC_WAIT)
!hcur=SetCursor(hcur)
ret=matj()
write(21,*) "matj passed"

!hdc=GetDC(hcmd);ret=TextOut(hdc,30,30,"Building final matrix..."C,24);bret=ReleaseDC(hcmd,hdc)
if(ioptima.eq.1) then
  ret=matls()
		   write(21,*) "matls passed"

end if

open(1,file='temp.txt')
write(21,*) "file temp opened"

read(1,'(i4)') ic
do i=1,ic
 read(1,'(i2,i2,a1,a3,f9.1)') i1,i2,cad,cht,u
 write(21,'(i2,i2,a1,a3,f9.1)') i1,i2,cad,cht,u
    !i1=4*(i-1); cbuf(i1+1:i1+3)=cht; cbuf(i1+4:i1+4)=' '
end do
close(1) !;close(2)
ret=FreeMem()

idatagl=1

ret=matj()
write(21,*) "matj passed"
!hdc=GetDC(hcmd);ret=TextOut(hdc,30,30,"Building final matrix..."C,24);bret=ReleaseDC(hcmd,hdc)
ifl=0
do i=1,3
 do j=1,7
  if(AIMAG(Bpk(i,j)).ne.0) ifl=1
 end do
end do
if(ifl.eq.0) then
 ret=finmatre()
else
 ret=finmat()
endif
write(21,*) "finmat passed"
hcur=hcur ! LoadCursor( NULL, IDC_ARROW)
!hcur=SetCursor(hcur)
ret=FreeMem()
write(21,*) "memory deallocated"

ret=DialogBox(hInst, LOC("RESULTS"C), hWndc, LOC(results))
write(21,*) "results are shown"
close(21)
!hdc=GetDC(hcmd); ret=TextOut(hdc,30,30,"New task?                                   "C,35);bret=ReleaseDC(hcmd,hdc);ret=UpdateWindow(hWnd)
!ret=SendMessage(hWndc,WM_SIZE, SIZE_RESTORED,0)
idatagl=1
calc=1
return
end function calc

!/****************************************************************************
!
!    FUNCTION: terms( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Manual terms selection for building of natrix to diagonalize
!
!    MESSAGES:
!****************************************************************************

integer*4 function terms( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_terms@16' :: terms
integer*4   hDlg, message, wParam, lParam
integer*4 ret
integer i,j,nt,ne
character*3,allocatable :: cht(:)
character*4 szbuf,szbuf1
integer isel,icount,idel,iStart

LPARAM = LPARAM

!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
 select case (message)
  case (WM_INITDIALOG);	i=1;nt=1;ne=nelec
   select case(ie)
    case(2)
	  if(ne.gt.5) then;iflag=1;ne=10-ne;end if;nt=dcon(ne)%con%nst
	case(3)
	  if(ne.gt.7) then;iflag=1;ne=14-ne;end if;nt=fcon(ne)%con%nst
   end select	
   allocate(cht(nt))
	 do i=1,nt
	  if(ie.eq.3) then;cht(i)=fcon(ne)%iLS(3*(i-1)+1:3*i);else
	   cht(i)=dcon(ne)%iLS(3*(i-1)+1:3*i)
      end if
	  ret=SendDlgItemMessage(hDlg,ID_CTERMSALL,LB_ADDSTRING,0,LOC(cht(i)//CHAR(0)))
	 end do
   deallocate(cht)
 case (WM_COMMAND)

   select case(INT4(LoWord(wparam)))
	 case(ID_CADD)
      icount=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETCOUNT,0,0)
	  if(icount.gt.47) then
	   iStart=47
	  else
	   iStart=nt
	  end if
	  do i=1,iStart
	   isel=SendDlgItemMessage(hDlg,ID_CTERMSALL,LB_GETSEL,i-1,0)
	   if(isel.ne.0) then
	    ret=SendDlgItemMessage(hDlg,ID_CTERMSALL,LB_GETTEXT,i-1,LOC(szbuf))
		icount=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETCOUNT,0,0)
		idel=0
        do j=1,MAX(1,icount)
		  ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETTEXT,j-1,LOC(szbuf1))
		  if(szbuf.eq.szbuf1)    idel=1
		 end do
		if(idel.eq.0) then
		 ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_ADDSTRING,icount,LOC(szbuf))
		end if
	   end if
	  end do
	 case(ID_CADDALL)
	   ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_RESETCONTENT,0,0)
	   iStart=nt !MIN(nt,22)
	   do i=1,iStart
	    ret=SendDlgItemMessage(hDlg,ID_CTERMSALL,LB_GETTEXT,i-1,LOC(szbuf))
	    ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_ADDSTRING,0,LOC(szbuf)) 
	   end do
     case(ID_CREMOVE)
      icount=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETCOUNT,0,0)
	  do i=1,MAX(1,icount)
	   isel=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETSEL,i-1,0)
	   if(isel.ne.0) then
	    ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_DELETESTRING,i-1,0)
	   end if
	  end do
	 case(ID_CREMOVEALL)
	  ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_RESETCONTENT,0,0)
	 case(IDOK)
	  icount=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETCOUNT,0,0)
	  if(icount.ne.0) then
	   allocate(cht(icount)) 
	   do i=1, icount
	    ret=SendDlgItemMessage(hDlg,ID_CTERMSINCLUDED,LB_GETTEXT,i-1,LOC(szbuf))
	    cht(i)=szbuf(1:3)
	   end do
	   ret=infow(icount,cht)
	   ret=EndDialog(hDlg,TRUE);deallocate(cht);terms=1;return
	  end if
	 case(IDCANCEL)
	   ret=EndDialog(hDlg,FALSE);terms=1;return
    end select
 end select
terms=0;return
end function terms

!/****************************************************************************
!
!    FUNCTION: results( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Output of eigensystem
!
!    MESSAGES:

!****************************************************************************/
integer*4 function results( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_results@16' :: results
use openfort
use zeeman

integer*4   hDlg, message, wParam, lParam
integer*4 ret,hmenu
integer i,i1,ivect,j
character*810 buf
character*20 buft
character*20, save, allocatable :: vect(:)
character*810, save, allocatable :: bufar(:)
character*17 szbuf
character*1 vID;logical IsThere;integer FileStatus 
character*40 tasknameo
integer, save :: iRecords
LPARAM = LPARAM
iRecords=0

!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

select case (message)
   case (WM_INITDIALOG)
   ret=SetDlgItemText(hDlg,ID_CSTATICNAME,taskname)
   hmenu=GetMenu(hDlg)
   INQUIRE (FILE = 'out.txt', EXIST = IsThere)
   IF (IsThere) THEN      
    FileStatus=0  
	tasknameo=taskname   
    open(1,file='out.txt',access='direct',form='formatted',recl=810)
   else
	   ret=Initoflev()
	   IF (ret.eq.1) THEN 
	    FileStatus=1
	    open(1,file=ofbuffer,access='direct',form='formatted',recl=810)
		i=lstrlen(CurrentLev)
		i1=lstrlen(ofbuffer)
		tasknameo=taskname
		taskname=ofbuffer(i+2:i1-4)
		ret=SetDlgItemText(hDlg,ID_CSTATICNAME,taskname//char(0))
		ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_GRAYED)
	   else
		 !ret=MessageBox(NULL," File was not found"C,"Error"C,MB_OK)
		 ret=EndDialog(hDlg,FALSE)
		 return
	   end if 
   end if
	ioutpos=1
	open(999,file="outdump.txt")
  	do while (.NOT. EOF(1))
 	 read(1,'(A810)',rec=ioutpos) buf
	 write(999,'(A810)') buf
	 ioutpos=ioutpos+1
	 buft=buf(3:10)
	 write(999,'(a8)') buft
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_RESULTS,LB_ADDSTRING,0,LOC(buft//CHAR(0)))
	 iRecords=iRecords+1
	end do
	close(999)
	allocate(bufar(iRecords))
!	rewind(1)
	do i=1,iRecords
	 read(1,'(a810)',rec=i) bufar(i)
	end do
	if(FileStatus.eq.0) then
	 close(1,status='delete')
	else
	 close(1)
	end if
	ret=SendDlgItemMessage(hDlg,ID_CALCULATE_RESULTS,LB_SETCURSEL,0,0)
   case (WM_COMMAND)
	i=SendDlgItemMessage(hDlg,ID_CALCULATE_RESULTS,LB_GETCURSEL,0,0)
    buf=bufar(i+1)
	buft=buf(1:2)
	read(buft,'(i2)') ivect
	allocate(vect(ivect))
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORSN,LB_RESETCONTENT,0,0)
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS,LB_RESETCONTENT,0,0)
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS2,LB_RESETCONTENT,0,0)

	do i=1,ivect
	 vect(i)=buf(11+(i-1)*(20):10+i*20)
	end do
	do i=1,ivect
	 buft=vect(i)
	 szbuf=buft(2:3)
	 read(szbuf,*) j
	 vID=ToUpCase(toL(j/2))
	 szbuf=buft(1:1)//vid//' '//buft(4:5)//char(47)//'2 '//buft(6:8)//'(2Jm)'//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORSN,LB_ADDSTRING,0,LOC(szbuf))
	 szbuf=buft(9:14)//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS,LB_ADDSTRING,0,LOC(szbuf))
	 szbuf=buft(15:20)//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS2,LB_ADDSTRING,0,LOC(szbuf))
	end do
	deallocate(vect)
    select case(INT4(LoWord(wparam)))
	 case(ID_ZEEMAN_G)
	  iRecords=SIZE(bufar)
	  ret=gfactor(iRecords, bufar)
	 case(ID_CALCULATE_SAVE)
 !!!!!!!!!!!!!!!!!!!!!insert code here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 ret=lstrcpy(buf,taskname(1:lstrlen(taskname))//".lev"C)
	 open(1,file=buf,access='direct',form='formatted',recl=810)
	  iRecords=SIZE(bufar)
	  do i=1,iRecords
	   write(1,'(A810)',rec=i) bufar(i)
	  end do
	 close(1)
	 ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_GRAYED)
	 case(IDOK)
	  taskname=tasknameo
	 	ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_ENABLED)
   	 deallocate(bufar)
	 ret=EndDialog(hDlg,TRUE)
	 results=1;return
	case(IDCANCEL)
	 ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_ENABLED)
   	 deallocate(bufar)
	 ret=EndDialog(hDlg,FALSE)
	 results=1;	 return
   end select
 end select
 results=0
 return
end function results


  
!/****************************************************************************
!
!    FUNCTION: resultsfit( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Output of eigensystem
!
!    MESSAGES:

!****************************************************************************/
integer*4 function resultsfit( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_resultsfit@16' :: resultsfit
use openfort
use zeeman
integer*4   hDlg, message, wParam, lParam
integer*4 ret,hmenu
integer iRecords,i,i1,ivect,j
character*810 buf
character*20 buft
character*20, save, allocatable :: vect(:)
character*810,save, allocatable :: bufar(:)
character*17 szbuf
character*1 vID;logical IsThere;integer FileStatus 
character*40 tasknameo
character*10 expbuf
LPARAM = LPARAM;iRecords=0

!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))

select case (message)
   case (WM_INITDIALOG)
   ret=SetDlgItemText(hDlg,ID_CSTATICNAME,taskname)
   ret=SetDlgItemText(hDlg,ID_FIT_ACCURACY,bave//char(0))
   hmenu=GetMenu(hDlg)
   do i=1,numlevtofit
   write(expbuf,'(f8.1)') tf(i)
   ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_RESULTSE,LB_ADDSTRING,0,LOC(expbuf//CHAR(0)))
   end do
   INQUIRE (FILE = 'out.txt', EXIST = IsThere)
   IF (IsThere) THEN      
    FileStatus=0  
	tasknameo=taskname   
    open(1,file='out.txt',access='direct',form='formatted',recl=810)
   else
	   ret=Initoflev()
	   IF (ret.eq.1) THEN 
	    FileStatus=1
	    open(1,file=ofbuffer,access='direct',form='formatted',recl=810)
		i=lstrlen(CurrentLev)
		i1=lstrlen(ofbuffer)
		tasknameo=taskname
		taskname=ofbuffer(i+2:i1-4)
		ret=SetDlgItemText(hDlg,ID_CSTATICNAME,taskname//char(0))
		ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_GRAYED)
	   else
		 !ret=MessageBox(NULL," File was not found"C,"Error"C,MB_OK)
		 ret=EndDialog(hDlg,FALSE)
		 return
	   end if 
   end if
  	do while (.NOT. EOF(1))
 	 read(1,'(A10)',rec=iRecords+1) buf
	 buft=buf(3:10)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_RESULTS,LB_ADDSTRING,0,LOC(buft//CHAR(0)))
	 iRecords=iRecords+1
	end do
	allocate(bufar(iRecords))
!	rewind(1)
	do i=1,iRecords
	 read(1,'(A810)',rec=i) bufar(i)
	end do
	if(FileStatus.eq.0) then
	 close(1,status='delete')
	else
	 close(1)
	end if
	ret=SendDlgItemMessage(hDlg,ID_CALCULATE_RESULTS,LB_SETCURSEL,0,0)

   case (WM_COMMAND)
	i=SendDlgItemMessage(hDlg,ID_CALCULATE_RESULTS,LB_GETCURSEL,0,0)
    buf=bufar(i+1)
	buft=buf(1:2)
	read(buft,*) ivect
	allocate(vect(ivect))
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORSN,LB_RESETCONTENT,0,0)
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS,LB_RESETCONTENT,0,0)
	ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS2,LB_RESETCONTENT,0,0)

	do i=1,ivect
	 vect(i)=buf(11+(i-1)*(20):10+i*20)
	end do
	do i=1,ivect
	 buft=vect(i)
	 szbuf=buft(2:3)
	 read(szbuf,*) j
	 vID=ToUpCase(toL(j/2))
	 szbuf=buft(1:1)//vid//' '//buft(4:5)//char(47)//'2 '//buft(6:8)//'(2Jm)'//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORSN,LB_ADDSTRING,0,LOC(szbuf))
	 szbuf=buft(9:14)//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS,LB_ADDSTRING,0,LOC(szbuf))
	 szbuf=buft(15:20)//char(0)
	 ret=SendDlgItemMessage(&
	      hDlg,ID_CALCULATE_VECTORS2,LB_ADDSTRING,0,LOC(szbuf))
	end do
	deallocate(vect)
    select case(INT4(LoWord(wparam)))
	 case(ID_ZEEMAN_G)
	  iRecords=SIZE(bufar)
	  ret=gfactor(iRecords, bufar)
	 case(ID_CALCULATE_SAVE)
 !!!!!!!!!!!!!!!!!!!!!insert code here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 ret=lstrcpy(buf,taskname(1:lstrlen(taskname))//".lev"C)
	 open(1,file=buf,access='direct',form='formatted',recl=810)
	  iRecords=SIZE(bufar)
	  do i=1,iRecords
	   write(1,'(A810)',rec=i) bufar(i)
	  end do
	 close(1)
	 ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_GRAYED)
	 case(IDOK)
	  taskname=tasknameo
	 	ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_ENABLED)
   	 deallocate(bufar)
	 ret=EndDialog(hDlg,TRUE)
	 resultsfit=1;return
	case(IDCANCEL)
	 ret=EnableMenuItem(hmenu,ID_CALCULATE_SAVE, MF_ENABLED)
   	 deallocate(bufar)
	 ret=EndDialog(hDlg,FALSE)
	 resultsfit=1;	 return
   end select
 end select
 resultsfit=0
 return
end function resultsfit

character*1 function ToUpCase(cht)
character*1 cht 
SELECT CASE (cht)
    CASE ( "a":"z" )
        cht = CHAR( ICHAR(cht) + ICHAR("A") - ICHAR("a") )
END SELECT
ToUpCase=cht;return
end function ToUpCase



!/****************************************************************************
!
!        FUNCTION: CenterWindow (HWND, HWND)
!
!        PURPOSE:  Center one window over another
!
!        COMMENTS:
!
! Centering the dialog over a particular  window usually results in a better position.
!
!****************************************************************************/


subroutine CenterWindow (hwndChild, hwndParent)

use msfwina 
use spinc
integer         hwndChild, hwndParent
type (T_RECT)     rChild, rParent
integer         wChild, hChild, wParent, hParent
integer         wScreen, hScreen, xNew, yNew
integer         hdc
integer*4       retval

! Get the Height and Width of the child window
   retval = GetWindowRect (hwndChild, rChild)
   wChild = rChild.right - rChild.left
   hChild = rChild.bottom - rChild.top

! Get the Height and Width of the parent window
   retval = GetWindowRect (hwndParent, rParent)
   wParent = rParent.right - rParent.left
   hParent = rParent.bottom - rParent.top

! Get the display limits
   hdc = GetDC (hwndChild)
   wScreen = GetDeviceCaps (hdc, HORZRES)
   hScreen = GetDeviceCaps (hdc, VERTRES)
   retval = ReleaseDC (hwndChild, hdc)

! Calculate new X position, then adjust for screen
   xNew = rParent.left + ((wParent - wChild) /2)
   if (xNew .LT. 0) then
      xNew = 0
   else if ((xNew+wChild) .GT. wScreen) then
      xNew = wScreen - wChild
   end if

! Calculate new Y position, then adjust for screen
   yNew = rParent.top  + ((hParent - hChild) /2)
   if (yNew .LT. 0) then
      yNew = 0
   else if ((yNew+hChild) .GT. hScreen) then
      yNew = hScreen - hChild
   end if

! Set it, and return
   ret = SetWindowPos(hwndChild, NULL, xNew, yNew, 0, 0,            &
                 IOR(SWP_NOSIZE ,SWP_NOZORDER))
end subroutine CenterWindow 

!/****************************************************************************
!
!    FUNCTION: Options( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Allows users to have additional control over the program
!
!    MESSAGES:
!****************************************************************************/


integer*4 function Options( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_options@16' :: options
integer*4   hDlg, message, wParam, lParam
integer*4 ret

LPARAM = LPARAM
!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
select case (message)
  case (WM_INITDIALOG)
    ret=CheckDlgButton(hDlg,ID_OTERMSSEL,mansel)
	ret=CheckDlgButton(hDlg,ID_ONORMW,1)
	ret=SetDlgItemText(hDlg,ID_ONAME,taskname(1:lstrlen(taskname)))
    ret=SetDlgItemInt(hDlg,ID_OVECT,INT(wavefix*100),.FALSE.)
	ret=SetDlgItemInt(hDlg,ID_OVECTNUMB,numfix,.FALSE.)
	ret=SetDlgItemInt(hDlg,ID_OENERGY,efix,.FALSE.)
	ret=SetDlgItemInt(hDlg,ID_FITTING_ACCURACY,genmax,.FALSE.)

  case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
	 case(ID_OTERMSSEL)
	 mansel=IsDlgButtonChecked(hDlg,ID_OTERMSSEL) 
	 case(IDOK)
	  ret=GetDlgItemInt(hDlg,ID_OVECT, NULL,.FALSE.)
	  wavefix=ret/100.
	  ret=GetDlgItemInt(hDlg,ID_OVECTNUMB, NULL,.FALSE.)
	  numfix=ret
	  ret=GetDlgItemInt(hDlg,ID_FITTING_ACCURACY, NULL,.FALSE.)
	  genmax=ret
	  if(genmax.le.0.or.genmax.gt.25) genmax=8
	  ret=GetDlgItemInt(hDlg,ID_OENERGY, NULL,.FALSE.)
	  efix=ret
	  if(efix.gt.70000) then
	   ret=MessageBox(NULL," That's too much! Energy maximum is set to 70000"C,"Error in input"C,MB_ICONSTOP)
	   efix=70000
	  end if
	  ret=GetDlgItemText(hDlg,ID_ONAME,taskname,40)
	  ret=EndDialog(hDlg,TRUE)
	  Options=1;return
   end select
end select
Options=0;return
end function Options

!/****************************************************************************
!
!    FUNCTION: Paramf( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Perform dialog to input free-ion parameters
!
!    MESSAGES:
!****************************************************************************/


integer*4 function Paramf( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_paramf@16' :: paramf
use pardia
integer*4   hDlg, message, wParam, lParam
integer*4 ret
integer i,idzeta
integer,dimension(3) :: ipf
LPARAM = LPARAM
!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
select case (message)
  case (WM_INITDIALOG)
	  ret=SetDlgItemText(hDlg,ID_PFSTATICNAME,taskname)
      ipf(1)=INT(electro(1));ipf(2)=INT(electro(2));ipf(3)=INT(electro(3))
	  idzeta=INT(magnetic(1))
      do i=ID_PF2,ID_PF6
	   ret=SetDlgItemInt(hDlg,i, ipf(i-ID_PF2+1),.FALSE.)
	  end do
	   ret=SetDlgItemInt(hDlg,ID_PFDZETA, idzeta,.FALSE.)
  case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
     case(IDYES)
	  do i=ID_PF2,ID_PF6
	    ipf(i-ID_PF2+1)=GetDlgItemInt(hDlg,i, NULL,.FALSE.)
	   end do
		idzeta=GetDlgItemInt(hDlg,ID_PFDZETA, NULL,.FALSE.)
		do ixe=1,3;electro(ixe)=ipf(ixe);end do
		magnetic(1)=idzeta
     case(IDRETRY)
	   ret=SendMessage(hDlg,WM_INITDIALOG,0,0)
	 case(IDCANCEL)
	   ret=EndDialog(hDlg,FALSE)
	 case(IDOK)
	   do i=ID_PF2,ID_PF6
	    ipf(i-ID_PF2+1)=GetDlgItemInt(hDlg,i, NULL,.FALSE.)
	   end do
		idzeta=GetDlgItemInt(hDlg,ID_PFDZETA, NULL,.FALSE.)
		do ixe=1,3;electro(ixe)=ipf(ixe);end do
		magnetic(1)=idzeta
	    ret=EndDialog(hDlg,TRUE)
	  Paramf=1;return
   end select
end select
Paramf=0;return
end function Paramf

subroutine pfreeopen(hWnd)
use pardia
integer*4 hWnd,ret

 call FreeIon()
 ret=DialogBox(hInst,LOC("PARAMF"C),hWnd,LOC(Paramf))

end subroutine pfreeopen

subroutine paradopen(hWnd)
use pardia
integer*4 hWnd,ret

 call FreeIon()
 ret=DialogBox(hInst,LOC("PARAD"C),hWnd,LOC(Parad))

end subroutine paradopen

!/****************************************************************************
!
!    FUNCTION: Parad( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Perform dialog to input advanced free-ion parameters
!
!    MESSAGES:
!****************************************************************************/


integer*4 function Parad( hDlg, message, wParam, lParam )
 !MS$ ATTRIBUTES STDCALL, ALIAS : '_parad@16' :: parad
 use pardia
 integer*4   hDlg, message, wParam, lParam
 integer*4 ret
 integer*4 i
 
 LPARAM = LPARAM
 select case (message)
  case (WM_INITDIALOG)
	  ret=SetDlgItemText(hDlg,ID_PFSTATICNAME,taskname)
      do i=ID_PA,ID_GA
	    ret=SetDlgItemInt(hDlg,i,Int(electro(i-ID_PA+4)),.True.)
	  end do
	  do i=ID_PT2,ID_PT8
	   ret=SetDlgItemInt(hDlg,i, Int(electro(i-ID_PT2+7)),.True.)
	  end do
	   do i=ID_PM0,ID_PM4
	    ret=SetDlgItemInt(hDlg,i, Int(magnetic(i-ID_PM0+2)*100.01),.True.)
	  end do
	  do i=ID_PP2,ID_PP6
	    ret=SetDlgItemInt(hDlg,i, Int(magnetic(i-ID_PP2+5)),.True.)
	  end do
  case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
	 case(IDOK)
	   do i=ID_PA,ID_GA
		ret=GetDlgItemInt(hDlg,i, NULL,.True.)
 	    electro(i-ID_PA+4)=ret
	   end do
	   do i=ID_PT2,ID_PT8
	   	ret=GetDlgItemInt(hDlg,i, NULL,.True.)
 	    electro(i-ID_PT2+7)=ret
	   end do
	  do i=ID_PM0,ID_PM4
	    ret=GetDlgItemInt(hDlg,i,NULL,.True.) 
		magnetic(i-ID_PM0+2)=ret/100.01
	  end do
	  do i=ID_PP2,ID_PP6
	  	ret=GetDlgItemInt(hDlg,i,NULL,.True.)
 	    magnetic(i-ID_PP2+5)=ret
	  end do
	  ret=EndDialog(hDlg,TRUE);return
	 case(IDRETRY)
	  ret=SendMessage(hDlg,WM_INITDIALOG,0,0)
	 case(IDCANCEL)
	  ret=EndDialog(hDlg,FALSE);return
   end select
 end select
 Parad=0;return
end function parad

subroutine psave()
use pardia
character*600 cbuf
integer ret,i
 inquire(iolength=iRecLenvse) vsecurrent
 vsecurrent.Bpk=Bpk
 vsecurrent.electro=electro
 vsecurrent.magnetic=magnetic
  ret=lstrcpy(cbuf,vsecurrent.ion//" in "//vsecurrent.desc(1:lstrlen(vsecurrent.desc))//".des"C)
	    i=lstrlen(curdirpath)
 	    ret=SetCurrentDirectory(curdirpath(1:i)//"\\Des"C)
	    i=lstrlen(cbuf)
	   open(1,file=cbuf(1:i),access='direct',recl=iRecLenvse);write(1) vsecurrent;close(1)
	   ret=SetCurrentDirectory(curdirpath)
end subroutine psave


!/****************************************************************************
!
!    FUNCTION: Paramcr( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Perform dialog to input crystal field parameters
!
!    MESSAGES:
!****************************************************************************/

integer*4 function Paramcr( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_paramcr@16' :: paramcr
use pardia
integer*4   hDlg, message, wParam, lParam
integer*4 ret
integer i,item
integer xtem2(5),xtem4(9),xtem6(13)
complex(8)  ic
data ic/(0.,1.)/

LPARAM = LPARAM

!call CenterWindow (hDlg, GetWindow (hDlg, GW_OWNER))
select case (message)
  case (WM_INITDIALOG)
      do i=1,3
       xtem2(i)=INT(REAL(Bpk(1,i)))
       xtem2(i+2)=INT(AIMAG(Bpk(1,i)))
      end do
      do i=1,5
       xtem4(i)=INT(REAL(Bpk(2,i)))
       xtem4(i+4)=INT(AIMAG(Bpk(2,i)))
      end do
      do i=1,7
       xtem6(i)=INT(REAL(Bpk(3,i)))
       xtem6(i+6)=INT(AIMAG(Bpk(3,i)))
      end do
	   ret=SetDlgItemText(hDlg,ID_PCR_TASKNAME,taskname)
      do i=ID_PCR20,ID_PCR24
	   item=i-ID_PCR20+1
	   ret=SetDlgItemInt(hDlg,i, xtem2(item),.TRUE.)
	  end do
	  do i=ID_PCR40,ID_PCR48
	   item=i-ID_PCR40+1
	   ret=SetDlgItemInt(hDlg,i, xtem4(item),.TRUE.)
	  end do
	  do i=ID_PCR60,ID_PCR72
	   item=i-ID_PCR60+1
	   ret=SetDlgItemInt(hDlg,i, xtem6(item),.TRUE.)
	  end do
  case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
     case(IDYES)
	 ! do i=ID_PF2,ID_PF6
	 !   ipf(i-ID_PF2+1)=GetDlgItemInt(hDlg,i, NULL,.FALSE.)
	 !  end do
	 case(IDRETRY)
	    ret=SendMessage(hDlg,WM_INITDIALOG,0,0)
	 case(IDCANCEL)
	   ret=EndDialog(hDlg,FALSE)
	 case(IDOK)
	  do i=ID_PCR20,ID_PCR24;item=i-ID_PCR20+1;xtem2(item)=GetDlgItemInt(hDlg,i,NULL,.TRUE.);end do
	  do i=ID_PCR40,ID_PCR48;item=i-ID_PCR40+1;xtem4(item)=GetDlgItemInt(hDlg,i,NULL,.TRUE.);end do
	  do i=ID_PCR60,ID_PCR72;item=i-ID_PCR60+1;xtem6(item)=GetDlgItemInt(hDlg,i,NULL,.TRUE.);end do
	   Bpk(1,1)=xtem2(1); Bpk(2,1)=xtem4(1);Bpk(3,1)=xtem6(1)
	   do i=2,3
        Bpk(1,i)=xtem2(i)+ic*(xtem2(i+2))
       end do
      do i=2,5
	   Bpk(2,i)=xtem4(i)+ic*(xtem4(i+4))
      end do
      do i=2,7
        Bpk(3,i)=xtem6(i)+ic*(xtem6(i+6))
      end do
	   Paramcr=1
	   ret=EndDialog(hDlg,TRUE)
	   return
   end select
end select
Paramcr=0;return
end function Paramcr

subroutine pcropen(hWnd)
use pardia
integer*4 hWnd,ret

 call crfpar()

 ret=DialogBox(hInst,LOC("PARAMCR"C),hWnd,LOC(Paramcr))

end subroutine pcropen

integer*4 function filetree( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_filetree@16' :: filetree
use pardia
use openfort
integer*4   hDlg, message, wParam, lParam
integer*4 ret, RecLenvse
integer iflag,i
type (vse) vserec
character*600 cbuf

LPARAM = LPARAM
select case (message)
  case (WM_INITDIALOG)
  	 iflag=1
	 inquire(iolength=RecLenvse) vserec
	 ret=Initofdes(hDlg)
	 IF (ret.eq.1) THEN 
	  open(1,file=ofbuffer,access='direct',recl=RecLenvse)
	  read(1,rec=1) vserec
	  close(1)
	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("d-ion"C))
  	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("f-ion"C))
	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_SETCURSEL,vserec.ie-2,0)
   	  ret=SetDlgItemText(hDlg,ID_FILENEDIT1,vserec.ion//CHAR(0))
	  ret=SetDlgItemText(hDlg,ID_FILENEDIT2,vserec.desc//char(0))
	  ret=SetDlgItemInt(hDlg,ID_FILENEDIT3,vserec.ne,.FALSE.)
	  icurrent=0
	 else
	  icurrent=0;ret=EndDialog(hDlg,FALSE);return
	 end if
	 ret=ShowWindow(hDlg,SW_SHOWNORMAL)

  case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
	  case(IDYES)
      i=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_GETCURSEL,0,0)
  	  vserec.ie=i+2
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT1,cbuf,5)
	  i=lstrlen(cbuf)
	  vserec.ion=cbuf(1:i)
	  ret=GetDlgItemInt(hDlg,ID_FILENEDIT3,NULL,.False.)
	  vserec.ne=ret
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT2,cbuf,33)
	  vserec.desc=cbuf(1:32)
	  iflag=1
	  if(vserec%ne.EQ.0) iflag=0
	  if((vserec%ie.eq.2.AND.vserec%ne.gt.9).or.(vserec%ie.eq.3.AND.vserec%ne.gt.13)) iflag=0
 	 case(IDOK)
	  if(iflag.eq.0) then; ret=MessageBox(NULL," Please, check your data and press Set button"C,"Error in input"C,MB_OK)
       else
	    ret=lstrcpy(cbuf,vserec.ion//" in "//vserec.desc(1:lstrlen(vserec.desc))//".des"C)
	    i=lstrlen(curdirpath)
 	    ret=SetCurrentDirectory(curdirpath(1:i)//"\\Des"C)
	    i=lstrlen(cbuf)
	   open(1,file=cbuf(1:i),access='direct',recl=RecLenvse);write(1,rec=1) vserec;close(1)
	   ret=SetCurrentDirectory(curdirpath)
	   vsecurrent=vserec
	   filetree=1;icurrent=1;ret=EndDialog(hDlg,TRUE);return
	  end if
	 case(IDCANCEL)
	  filetree=1;ret=EndDialog(hDlg,TRUE);return
	 end select
end select
filetree=0;return
end function filetree

integer*4 function filetreen( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_filetreen@16' :: filetreen
use pardia
integer*4   hDlg, message, wParam, lParam
integer*4 ret
character*600 cbuf
integer RecLenvse, iflag
type (vse) vserec

LPARAM = LPARAM;vserec.Bpk=0.;vserec.electro=0.;vserec.magnetic=0.
select case (message)
  case (WM_INITDIALOG)
	  inquire(iolength=RecLenvse) vserec
      iflag=0
   	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("d-ion"C))
  	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("f-ion"C))
	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_SETCURSEL,1,0)
  case (WM_COMMAND)
  select case(INT4(LoWord(wparam)))
	 case(IDYES)
      i=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_GETCURSEL,0,0)
  	  vserec.ie=i+2
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT1,cbuf,5)
	  i=lstrlen(cbuf)
	  vserec.ion=cbuf(1:i)
	  ret=GetDlgItemInt(hDlg,ID_FILENEDIT3,NULL,.False.)
	  vserec.ne=ret
	  !ret=SetDlgItemInt(hDlg,ID_FILENEDIT2,vserec.ne,.False.)
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT2,cbuf,33)
	  vserec.desc=cbuf(1:32)
	  iflag=1
	  if(vserec%ne.EQ.0) iflag=0
	  if((vserec%ie.eq.2.AND.vserec%ne.gt.9).or.(vserec%ie.eq.3.AND.vserec%ne.gt.13)) iflag=0
 	 case(IDOK)
	  if(iflag.eq.0) then; ret=MessageBox(NULL," Please, check your data and press Set button"C,"Error in input"C,MB_OK)
       else
	   ret=lstrcpy(cbuf,vserec.ion//" in "//vserec.desc(1:lstrlen(vserec.desc))//".des"C)
	   i=lstrlen(curdirpath)
 	   ret=SetCurrentDirectory(curdirpath(1:i)//"\\Des"C)
	   i=lstrlen(cbuf)
	   open(1,file=cbuf(1:i),access='direct',recl=RecLenvse)
	   write(1,rec=1) vserec
	   close(1)
	   ret=SetCurrentDirectory(curdirpath)
	   filetreen=1;ret=EndDialog(hDlg,TRUE);return
	  end if
	 case(IDCANCEL)
	  filetreen=1;ret=EndDialog(hDlg,TRUE);return
	  case default
     end select
end select
filetreen=0;return
end function filetreen


integer*4 function BpkCalc( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_bpkcalc@16' :: bpkcalc
use pardia
integer*4   hDlg, message, wParam, lParam
integer*4 ret
character*600 cbuf
integer RecLenvse, iflag
type (vse) vserec
complex(8)  ic
real Gs
data ic/(0.,1.)/
dimension o2(5)
dimension o4(9)
dimension o6(13)


LPARAM = LPARAM;
 OPEN(3,FILE='out.lat')
      read(3,22) Gs
 22 format(F4.1)  
      read(3,60) (O2(I),I=1,5)
	  read(3,*) 
 !58   format('Fourth-order parameters') 
      read(3,60) (O4(I),I=1,9)
	  read(3,*) 
 !59   format('Sixth-order parameters') 
      read(3,60) (O6(I),I=1,13)
 60   FORMAT(4F15.5)  
	  close(3)  

vserec.Bpk=0.;vserec.electro=0.;vserec.magnetic=0.
vserec.Bpk(1,1)=O2(1); vserec.Bpk(2,1)=O4(1);vserec.Bpk(3,1)=O6(1)

	  do i=2,3
        vserec.Bpk(1,i)=O2(2*i-2)+ic*(O2(2*i-1))
       end do
      do i=2,5
	   vserec.Bpk(2,i)=O4(2*i-2)+ic*(O4(2*i-1))
      end do
      do i=2,7
        vserec.Bpk(3,i)=O6(2*i-2)+ic*(O6(2*i-1))
      end do

select case (message)
  case (WM_INITDIALOG)
	  inquire(iolength=RecLenvse) vserec
      iflag=0
   	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("d-ion"C))
  	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_ADDSTRING,0,LOC("f-ion"C))
	  ret=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_SETCURSEL,1,0)
  case (WM_COMMAND)
  select case(INT4(LoWord(wparam)))
	 case(IDYES)
      i=SendDlgItemMessage(hDlg,ID_FILETREECOMBO,CB_GETCURSEL,0,0)
  	  vserec.ie=i+2
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT1,cbuf,5)
	  i=lstrlen(cbuf)
	  vserec.ion=cbuf(1:i)
	  ret=GetDlgItemInt(hDlg,ID_FILENEDIT3,NULL,.False.)
	  vserec.ne=ret
	  !ret=SetDlgItemInt(hDlg,ID_FILENEDIT2,vserec.ne,.False.)
	  ret=GetDlgItemText(hDlg,ID_FILENEDIT2,cbuf,33)
	  vserec.desc=cbuf(1:32)
	  iflag=1
	  if(vserec%ne.EQ.0) iflag=0
	  if((vserec%ie.eq.2.AND.vserec%ne.gt.9).or.(vserec%ie.eq.3.AND.vserec%ne.gt.13)) iflag=0
 	 case(IDOK)
	  if(iflag.eq.0) then; ret=MessageBox(NULL," Please, check your data and press Set button"C,"Error in input"C,MB_OK)
       else
	   ret=lstrcpy(cbuf,vserec.ion//" in "//vserec.desc(1:lstrlen(vserec.desc))//".des"C)
	   i=lstrlen(curdirpath)
 	   ret=SetCurrentDirectory(curdirpath(1:i)//"\\Des"C)
	   i=lstrlen(cbuf)
	   open(1,file=cbuf(1:i),access='direct',recl=RecLenvse)
	   write(1,rec=1) vserec
	   close(1)
	   ret=SetCurrentDirectory(curdirpath)
	  BpkCalc=1;ret=EndDialog(hDlg,TRUE);return
	  end if
	 case(IDCANCEL)
	  BpkCalc=1;ret=EndDialog(hDlg,TRUE);return
	  case default
     end select
end select
BpkCalc=0;return
end function BpkCalc


integer function deActivateMenu(hmenu)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_deActivateMenu@4' :: deActivateMenu
integer hmenu,ret
  ret=EnableMenuItem(hmenu,ID_PFREEION_NEW, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PFREEION_OPEN, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PSAVE, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PCR_NEW, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PCR_OPEN, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PAD_NEW, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_PAD_OPEN, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_MATRIX, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_CALCULATE, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_FILEOPEN, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_FILECLOSE, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_FIT, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_FASTFIT, MF_GRAYED)
  ret=EnableMenuItem(hmenu, ID_FIELD_CALCULATE, MF_ENABLED)  
  deActivateMenu=1
end function deActivateMenu
integer function reActivateMenu(hmenu)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_reActivateMenu@4' :: reActivateMenu
integer hmenu,ret
  ret=EnableMenuItem(hmenu,ID_PFREEION_NEW, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PFREEION_OPEN, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PSAVE, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PCR_NEW, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PCR_OPEN, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PAD_NEW, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_PAD_OPEN, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_MATRIX, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_CALCULATE, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_FILEOPEN, MF_GRAYED)
  ret=EnableMenuItem(hmenu,ID_FILECLOSE, MF_ENABLED)
  ret=EnableMenuItem(hmenu,ID_FIT, MF_ENABLED)
   ret=EnableMenuItem(hmenu,ID_FASTFIT, MF_ENABLED)	 
    ret=EnableMenuItem(hmenu, ID_FIELD_CALCULATE, MF_GRAYED)  
  reActivateMenu=1
end function reActivateMenu

integer*4 function newfunc( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_newfunc@16' :: newfunc
use msflib
integer ret,hDlg,wparam,lparam,message
integer ifile,handle
TYPE  (FILE$INFO)  info
																									
handle = FILE$FIRST

!call   CenterWindow (hDlg, GetWindow(hDlg, GW_OWNER))
LPARAM = LPARAM;WPARAM = WPARAM  
select case (message)
 case (WM_INITDIALOG)
  ifile=GETFILEINFOQQ('license.txt', info, handle)

  if(ifile.ne.0) then 
    ret=EndDialog(hDlg,TRUE);newfunc=1;return
  end if
	 open(666,file='license.txt')
	  write(666,*) license,disclaimer
	 close(666)
	 ret=SetDlgItemText(hDlg, ID_LICENSE,TRIM(license))
	 ret=SetDlgItemText(hDlg, ID_DISCLAIMER,TRIM(disclaimer))
    !ret=SetTimer(hDlg,1,1500,0,0)
   case (WM_COMMAND)
   select case(INT4(LoWord(wparam)))
      case(IDOK)
	  ret=EndDialog(hDlg,TRUE);newfunc=1;return
	end select
end select
end function newfunc

integer*4 function progress( hDlg, message, wParam, lParam)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_progress@16' :: progress

use pardia
use ctrl
use toolb
use multi
use matrix


integer*4   hDlg,message, wParam, lParam
integer*4 ret, hth1,hth2,hth3
integer ih1,ih2,ih3,ierr

lparam=lparam;wparam=wparam;hDlgPB=hDlg
!call InitCommonControls()
select case (message)

 case (WM_INITDIALOG)
 !ret=ShowWindow(hDlg,SW_SHOWDEFAULT)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETSTEP,1,0)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETPOS,0,0)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSM,PBM_SETSTEP,1,0)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSM,PBM_SETPOS,0,0)
  ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSE,PBM_SETSTEP,1,0)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSE,PBM_SETPOS,0,0)
  ret=SendMessage(hDlgPB,WM_COMMAND,ID_PROGRESSBEGIN,0)
 case (WM_FINISH_EL);ih1=1
 case (WM_FINISH_MA);ih2=1
 case(WM_FINISH_CR);ih3=1
 case (WM_COMMAND)
 select case(INT4(LoWord(wparam)))
case(ID_PROGRESSBEGIN)
  inelec=nelec
  ret=SetCurrentDirectory(curdirpath)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETPOS,1,0)
 ret=ShowWindow(hDlg,SW_SHOWDEFAULT)

  inquire(IOLENGTH=RecLensp)	rsp; i=0;i1=0
  open(UNIT=21,FILE=dbfilespin(ie-1),action='read',status='old',ACCESS='direct', RECL=RecLensp,iostat=ierr)
  DO WHILE (.NOT. EOF(21));read(21,iostat=ierr,REC=i1+1) rsp
  if(rsp%eln.eq.nelec) i=i+1; i1=i1+1; end do
  allocate(respinb(i));allocate(respinbfull(i1));ifull=i1
  DO i=1,ifull;read(21,iostat=ierr,REC=i) respinbfull(i);end do;close(21)	  
 ! hth2=threadm(1)
   hth2= CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(threadm),0,0,LOC(IdTh2))

  select case(ie);case(2);if(nelec.gt.5) inelec=10-inelec;case(3);if(nelec.gt.7) inelec=14-inelec;end select
  inquire(IOLENGTH=RecLencr)	rcr; i=0;i1=0
  open(UNIT=20,FILE=dbfilecr(ie-1),action='read',status='old',ACCESS='direct', RECL=RecLencr,iostat=ierr)
  DO WHILE (.NOT. EOF(20));read(20,iostat=ierr,REC=i1+1) rcr
  if(rcr%eln.eq.inelec) i=i+1; i1=i1+1; end do
  allocate(rcrb(i));allocate(rcrbfull(i1));ifull=i1;DO i=1,ifull;read(20,iostat=ierr,REC=i) rcrbfull(i);end do;close(20)
  ih1=0;ih2=0;ih3=0
 ! hth3=threadc(1)
  ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETPOS,0,0)
   hth3=CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(threadc),0,0,LOC(IdTh3))

  inquire(IOLENGTH=RecLenel)	rel; i=0;i1=0
  open(UNIT=22,FILE=dbfileels(ie-1),action='read',status='old',ACCESS='direct', RECL=RecLenel,iostat=ierr)
  DO WHILE (.NOT. EOF(22));read(22,iostat=ierr,REC=i1+1) rel
  if(rel%eln.eq.nelec) i=i+1; i1=i1+1; end do
  allocate(relb(i));allocate(relbfull(i1));ifull=i1;DO i=1,ifull;read(22,iostat=ierr,REC=i) relbfull(i);end do;close(22)
!  hth1=threade(1)
   hth1=CreateThread(NULL_SECURITY_ATTRIBUTES,0,LOC(threade),0,0,LOC(IdTh1))
 
  case(3)
 	if(ih1*ih2*ih3.eq.1) then
      ret=EndDialog(hDlg,TRUE);progress=1;return
	end if
   end select
 end select
end function progress

end module diaproc
