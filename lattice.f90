module lattice
  use apk
  use openfort
  use spinc

 CHARACTER(3),allocatable:: AT(:)
 character*10 sitetitle
 character*3 chRE
 character*2 chLig	
 integer ksub, nrel
 integer,save :: hlocalInst
 type(T_RECT) rectan

contains

integer*4 function latdialog( hDlg, message, wParam, lParam )
 !MS$ ATTRIBUTES STDCALL, ALIAS : '_latdialog@16' :: latdialog

 integer*4   hDlg, message, wParam, lParam
 integer ret,i, iindex  
 character*4 chtemp
  character*3 chtempL

 character*11 titletemp
 character*4 chgpar
 character*5 chpartemp


 LPARAM = LPARAM
 write(chgpar,'(f4.1)') Gs
 select case (message)
	case (WM_INITDIALOG)
	   do i=1,ksub
			ret=SendDlgItemMessage(hDlg,IDL_SITE,LB_ADDSTRING,0,LOC(at(i)//CHAR(0)))
		end do
  	   ret=SendDlgItemMessage(hDlg,IDL_SITE,LB_SETCURSEL,1,0)
	   ret=SetDlgItemText(hDlg,ID_SELECTTITLE,sitetitle//char(0))
	   ret=SetDlgItemText(hDlg,ID_SELECTRE,chRE//char(0))
	   ret=SetDlgItemText(hDlg,ID_LIGAND,chLig//char(0))
	   ret=SetDlgItemText(hDlg,ID_GPAR,chgpar//char(0))
!	 case(WM_PAINT)
!	   ret=UpdateWindow(hDlg)
!      ret=UpdateWindow(hMWindow)
	case (WM_COMMAND)
	 !ret=InvalidateRgn(hMWindow,NULL,.TRUE.)
!	 ret=InvalidateRgn(hDlg,NULL,.TRUE.)
	 
	  iindex=SendDlgItemMessage(hDlg,IDL_SITE,LB_GETCURSEL,0,0)
	  nrel=iindex+1	
	select case(INT4(LoWord(wparam)))
  	   case(IDOK)
	     ret=GetDlgItemText(hDlg,ID_SELECTTITLE,titletemp,11)
		 sitetitle=titletemp(1:ret)
		 ret=GetDlgItemText(hDlg,ID_SELECTRE,chtemp,4)
		 chRE=chtemp(1:3)
	 	 ret=GetDlgItemText(hDlg,ID_LIGAND,chtempL,3)
		 chLig=chtempL(1:2)
		 ret=GetDlgItemText(hDlg,ID_GPAR,chpartemp,5)
		 read(chpartemp(1:4),*) Gs
	     ret = EndDialog(hDlg, TRUE)
		  latdialog = 1;return
		case(IDCANCEL)
		   ret = EndDialog(hDlg, FALSE)
		   latdialog =-1;return
	end select
 end select
 latdialog = 1
 return
end function latdialog				  

 integer function makelat()

	IMPLICIT REAL*8(A-H,O-Z)

	 integer i,i1,i2,i3,l
	 DIMENSION T1(3),T2(3),T3(3),Y(3) 
	 allocatable DEC(:,:),RR(:,:),X(:,:,:),IND(:,:),chr(:)
     integer ret
	 character*14 temptitle

	hlocalInst=hInst
	 sitetitle="defaultnam"
	 chRE="Nd3"
	 chLig="O2"
	 Gs=8
	 ret=Initoflat()
  IF (ret.eq.1) THEN 
    ret = UpdateWindow(hMWindow)
	FileStatus=1
	open(1,file=ofbuffer, mode='read')
    READ(1,5) KSUB                
  5   FORMAT(24X,I3)
      ALLOCATE (AT(KSUB),DEC(KSUB,3),chr(KSUB)) 
      READ(1,*) 
      READ(1,'(3f7.4)') T1,T2,T3
      READ(1,10) (AT(I),(DEC(I,J),J=1,3),chr(I),I=1,KSUB) 
  10  FORMAT(A3,3F10.6,f6.2)                
	   close(1)
!      WRITE(*,*) 'Enter site for field calculation(I4)' 
!      READ(*,15) NREL                  
	   nrel=1
	   ret=DialogBox(hlocalInst,LOC("SITESELECT"C),hMWindow,LOC(latdialog))
	   if(ret.eq.TRUE) then 
  
!******************the number of translations ****************************
		ktr=13
!********************************************************************      
		KCELL=ktr*ktr*ktr             
		KSUM=KCELL*KSUB
		ALLOCATE (RR(KSUB,3),X(KSUB,KCELL,3),IND(KSUM,2))
		do I=1,KSUM; IND(I,1)=0; IND(I,2)=0; end do
		do I=1,KSUB;do L=1,3;RR(I,L)=DEC(NREL,L)-DEC(I,L);end do; end do
		ktt=(ktr+1)/2
		DO 100 I=1,KSUB
			IS=0
			DO 100 I1=1,ktr                  
				II1=I1-ktt
				DO 100 I2=1,ktr
					II2=I2-ktt
					DO 100 I3=1,ktr
						IS=IS+1;II3=I3-ktt   
						do L=1,3
							Y(L)=T1(L)*II1+T2(L)*II2+T3(L)*II3             
							X(I,IS,L)=DEC(NREL,L)-DEC(I,L)-Y(L) 
						end do
 100  continue
	     CALL SORT(KSUB,KCELL,X,IND,IS) 
		 i=lstrlen(curdirpath)
 	     ret=SetCurrentDirectory(curdirpath(1:i)//"\\Lat"C)
		 temptitle=TRIM(sitetitle)//".xyz"
		 i=LEN_TRIM(temptitle)
	     open(3,file=temptitle(1:i))
	     write(3,'(a10)') sitetitle
		 write(3,'(i2)') is
		 write(3,'(a3)') chRE
         write(3,'(x,a2)') chLig
		 write(3,'(f4.1)') Gs
		 do  I=1,IS
			 write(3,101) AT(IND(I,1)),(X(IND(I,1),IND(I,2),L),L=1,3),&
							chr(IND(I,1)),&
				SQRT(X(IND(I,1),IND(I,2),1)**2+X(IND(I,1),IND(I,2),2)**2+&
							 X(IND(I,1),IND(I,2),3)**2)
		101  FORMAT(a3,f10.6,f10.6,f10.6,f6.2,f10.6)   
		
		 end do
		close(3)
	  	 i=lstrlen(curdirpath)
 	     ret=SetCurrentDirectory(curdirpath(1:i))
	 deallocate(RR,X,IND)
     deallocate(AT,DEC,chr)
	 makelat=1
	 return
	else
	  deallocate(AT,DEC,chr)
	  makelat=-1
	  return
	end if
  else
	ret=MessageBox(NULL,"You didn't opened any file"C,""C,MB_ICONSTOP)
	makelat=-1
	return
  endif
end function makelat

!***** subroutine to sort lattice atoms position*********************

 subroutine SORT(KSUB,KCELL,X,IND,IS)
      IMPLICIT REAL*8(A-H,O-Z)
!      CHARACTER*4 AT(KSUB)
      DIMENSION X(KSUB,KCELL,3)
	  integer i,j
      allocatable XN(:,:)
      DIMENSION IND(KSUB*KCELL,2)
      ALLOCATE (XN(KSUB,KCELL))
      DO  I=1,KSUB
       DO  J=1,KCELL
         XN(I,J)=0.0D00
         do  L=1,3
           XN(I,J)=XN(I,J)+X(I,J,L)*X(I,J,L)
		 end do
		 XN(I,J)=DSQRT(XN(I,J))
		end do
	   end do
      AM=1.D30                                 
!*********building coordination spheres up to RMAX************************
      RMAX=3.1D00         
!******************************************************************      
      IS=0
	  where(xn.gt.rmax)
		 xn=-1
	  end where
	  do i=1,ksub
	   do j=1,kcell
	    if(xn(i,j).gt.0) then
		 is=is+1
		 ind(is,1)=i;ind(is,2)=j
		end if  
	   end do
	  end do

       deallocate(XN)
    end subroutine sort



integer function getLigands()
 integer ret,i
 character*3, allocatable :: tmpid(:)
 real r

   ret=Initoflat()
   IF (ret.eq.1) THEN 
    FileStatus=1
    open(1,file=ofbuffer)
	read(1,'(a40)') envname
	read(1,'(i2)') numat
	allocate(tmpid(numat),xLig(numat,3),qLig(numat))
	tmpid=' ';xLig=0.;qLig=0.
	read(1,'(a3)') 	idRE;	read(1,'(a3)') 	idLig
	read(1,'(f4.1)') Gs
	do  i=1,numat
	read(1,10) tmpid(i),xLig(i,1), xLig(i,2), xLig(i,3),qLig(i),r
 10	format(a3,f10.6,f10.6,f10.6,f6.2,f10.6)
	end do

!	open(2,file='debug.txt'); !	write(2,'(i2)') numat;!	write(2,'(a3)') idRE
!	do  i=1,numat;!	write(2, 10) idLig,xLig(i,1), xLig(i,2), xLig(i,3);!	end do; !	close(2)

	deallocate(tmpid)
	close(1)
    getLigands=1
   else
	ret=MessageBox(NULL,"You didn't opened any file"C,""C,MB_ICONSTOP)
	getLigands=-1
	return
   endif
end function   getLigands


integer function doAnalysis()
    integer iostat, ret,i,j,icount 
	character*3 id1,id2
	character*80 chtemp
	character*6 coeftemp
	integer itwasfound

	!open(11, file='debug.txt')
 	!write(11,*) "enter the module doAnalysis"

	open(10,file='overlap.txt', ACTION='READ',IOSTAT=iostat)
	if(iostat.ne.0) then
		ret=MessageBox(NULL,"Problems with file overlap.txt\nIt should be in your Spectra directory\n"C,""C,MB_ICONSTOP)
		return
	end if
	!write(11,*) "file overlap.txt is opened successfully"
	read(10,*);read(10,*)
	!write(11,*) "begin reading"
	do	while (.not. EOF(10))
		read(10,'(a80)') chtemp
		icount=scan(chtemp,'$')
		id1=chtemp(icount-3:icount-1)
		chtemp=chtemp(icount+1:)
		icount=scan(chtemp,'$')
		id2=chtemp(icount-3:icount-1)
		chtemp=chtemp(icount+1:)
		if(idRE.eq.id1.and.idLig.eq.id2) then
		 do i=1,2
			do j=1,3
				icount=scan(chtemp,'$')
				coeftemp=chtemp(icount-6:icount-1)
				read(coeftemp,'(f6.3)') SS(i,j)
				chtemp=chtemp(icount+1:)
		  	 end do
		  end do
		  do i=1,2
		   		icount=scan(chtemp,'$')
				coeftemp=chtemp(icount-4:icount-1)
				read(coeftemp,'(f4.1)') rlimits(i)
				chtemp=chtemp(icount+1:)
	   	  end do
		  itwasfound=1
		 exit
		else
		end if
	end do
!	write(11,*) id1, id2, ((SS(i,j),j=1,3),i=1,2),(rlimits(i),i=1,2)
   	close(10)
!	close(11)
	if(itwasfound.ne.1) then
	  ret=MessageBox(NULL,"SPECTRA is unable to find built-in values for the case you specified! "//idRE//" -"//idLig//" pair is not in the database"C,"BAD CASE"C,MB_ICONSTOP)
	  deallocate(xLig,qLig)
	  doAnalysis=-1;return
	else
		doAnalysis=1;return
	endif
end function   doAnalysis


integer function calculateCFP()

integer ret

ret=parex()
calculateCFP=1
deallocate(xLig,qLig)

end function   calculateCFP
   

end module lattice