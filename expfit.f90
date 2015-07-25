module expfit
use spinc
use msfwina
use dataman
use matrix
use pardia
use msimsl
use openfort
use diaproc

!*************parameters for genetic module***************************
real,  save ::  crossover=0.1, noise=0.5
!*********************************************************************


real, allocatable,save :: xguess(:)
integer,allocatable,save ::  tfresult(:)
integer*4,save :: numpartofit
integer,save :: icrystal,itefit
type sootv
 integer(1) tip	 !type of interaction: 1-electro;2-magnetic;3-crystal field
 integer nom	 !index of the parameter inside specified interaction
 integer rank    !rank of crystal field parameters:1-2;2-4;3-6
 integer constr  !indicate whether to use constraints: 1-f2/f4/f6;2-cubic;3-p2/p4/p6
end type sootv

type(sootv), allocatable,save :: icorr(:)

contains

subroutine fcn(m,n,x,f)
 integer m,n
 real :: x(n)
 real :: f(m)
 
 integer iRecords,i
 character*810 buf
 character*20 buft
 character*810,allocatable,save :: bufar(:)
 integer ibuft
 integer ind,ret

 itefit=itefit+1
 if(itefit.eq.1) then
   open(20,file='results.txt',status='Replace')
 end if
 write(20,1) "Iteration number ",itefit
 1 format(/,A16,i3)
 do i=1,n
  ind=icorr(i).tip
  select case(ind)
   case(1)
    electro(icorr(i).nom)=x(i)
	if(icorr(i)%constr.eq.1) then
	 electro(2)=electro(1)*fconstraint4
     electro(3)=electro(1)*fconstraint6
	end if
   case(2)
    magnetic(icorr(i).nom)=x(i)
	write(20,5) " spin-orbit=",x(i)
	5 format(a12,f9.2)
   case(3)
    Bpk(icorr(i).rank,icorr(i).nom)=x(i)
	write(20,7) " cf ",icorr(i).rank*2,icorr(i).nom-1,x(i)
	7 format(a4,i2,i2,f9.2)
	if(icorr(i)%constr.eq.2) then
	 Bpk(2,5)=ratcu4440*Bpk(2,1)
	 Bpk(3,5)=ratcu6460*Bpk(3,1)
	end if
  end select
 end do

	 ret=matj()
	 ret=finmatre()
	 ret=FreeMem()
	 iRecords=0
open(133,file='out.txt',access='direct',recl=810,form='formatted')
ioutpos=1
	do while (.NOT. EOF(133))
 	 read(133,'(A10)',rec=ioutpos) buf
	 ioutpos=ioutpos+1
	 buft=buf(3:8)
     read(buft,*) ibuft
	 !write(20,10) ibuft
	 !10 format(i6)
	 iRecords=iRecords+1
	end do
 allocate(bufar(iRecords),tfresult(iRecords))
!	rewind(133)
	do i=1,iRecords
	 read(133,'(A810)',rec=i) bufar(i)
	 buft=bufar(i)(3:8)
	 read(buft,*) tfresult(i)
	end do
 close(133)
 ave=0   
 do i=1,m
  f(i)=ABS(tf(i)-tfresult(i))
  ave=ave+f(i)
 end do
 ave=ave/m

 write(20,10) ' number ', 'exp.','calculated','deviation'
 10 format(/,a7,6X,a4,5X,a10,3X,a10)
 do i=1,m
   write(20,11) i, tf(i),tfresult(i),f(i)
 11 format(i4,4x,f8.1,4x,i8,4x,f7.1)
 end do
   write(20,20) ave
 20 format(' Average deviation is ',f9.2)
   deallocate(bufar,tfresult)
  !close(2)
end subroutine fcn


integer function setcorr()
 integer i, j,num
 setcorr=0
 num=0
 do i=1,6
  if(ielectro(i).eq.1) then
   num=num+1
   icorr(num).tip=1;icorr(num).nom=i
   if(ielectro(i+1).eq.101) icorr(num).constr=1
  end if
 end do
 if(imagnetic(1).eq.1) then
  num=num+1
  icorr(num).tip=2;icorr(num).nom=1
 end if
 if(icrystal.eq.0) return 
  do i=1,3
   	do j=1,7
	 if(iBpk(i,j).eq.1) then
	  num=num+1
	  icorr(num).tip=3
	  icorr(num).nom=j
	  icorr(num).rank=i
	  if(iBpk(2,5).eq.101.and.i.eq.2) icorr(num).constr=2
	 end if
	end do
  end do
 setcorr=1
 return
end function setcorr

subroutine setbounds(n,xguess,xlb,xub)
 integer n
 real, dimension(n) :: xguess,xlb,xub
 integer i,ind,indrank
 do i=1,n
  ind=icorr(i).tip
  indrank=icorr(i).rank
  select case(ind)
   case(1)
    xlb(i)=xguess(i)*(1-rangeelectro)-10.
	xub(i)=xguess(i)*(1+rangeelectro)+10.
   case(2)
    xlb(i)=xguess(i)*(1-rangemagnetic)-10.
	xub(i)=xguess(i)*(1+rangemagnetic)+10.
   case(3)
	select case(indrank)
	 case(1)
	  xlb(i)=xguess(i)*(1-rangecrystal2)-20.
	  xub(i)=xguess(i)*(1+rangecrystal2)+20.
	 case(2)
	  xlb(i)=xguess(i)*(1-rangecrystal4)-30.
	  xub(i)=xguess(i)*(1+rangecrystal4)+30.
     case(3)
	  xlb(i)=xguess(i)*(1-rangecrystal6)-30.
      xub(i)=xguess(i)*(1+rangecrystal6)+30.
	end select
  end select
 end do
end subroutine setbounds

subroutine setinitguess(n,xguess,fscale)
 integer n
 real, dimension(n) :: xguess,fscale
 integer i,ind

 do i=1,n
  ind=icorr(i).tip
  select case(ind) 
   case(1) !eletro
    xguess(i)=electro(icorr(i).nom)
   case(2) !magnetic
    xguess(i)=magnetic(icorr(i).nom)
   case(3) !crystal field
	xguess(i)=Real(Bpk(icorr(i).rank,icorr(i).nom))
  end select
  fscale(i)=xguess(i)
 end do
end subroutine setinitguess



integer function datastart()
  integer hdc,ret	
  character*10 buf
	hdc=GetDC(hWndfit)
	itemp=0
	jtemp=60
	do i=1,numlevtofit
	 write(buf,'(f8.1)') tf(i)
	 itemp=itemp+80
	 if(itemp.ge.nDisplayWidth-80) then
	  jtemp=jtemp+60 
	  itemp=0
	 end if
   	 ret=TextOut(hdc,itemp,jtemp,buf,10)
	end do
    ret=ReleaseDC(hWndfit,hdc)

	datastart=1
end function datastart  

integer*4 function fitexp()
!MS$ ATTRIBUTES STDCALL, ALIAS : '_fitexp@0' :: fitexp
integer i,ret
real tfit
real bret				
real,allocatable,save :: rr(:,:),xlb(:),xub(:),xscale(:),fscale(:),fvec(:),x(:),fjac(:,:)
integer, dimension(6) :: iparam
real, dimension(7) 	:: rparam


!maskcoulomb=0;maskcrystal=0;maskspin=0;maskgreek=0;maskelectro=0;maskmagnetic=0
  ! write(chartemp,'(i1)') fastfitflag
  ! ret=MessageBox(0, chartemp,"Message"C,MB_OK)
   numlevtofit=0
   ret=Initofexp(hWndfit)
   idatagl=1
   IF (ret.eq.1) THEN 
	    FileStatus=1
	    open(1,file=ofbuffer)
   else
	 ret=MessageBox(0,"File with experimental levels \nis needed for fitting"C,"Failure"C,MB_ICONINFORMATION)
	 return
   end if
     !open(2,file="dataout.txt")
   do while (.NOT. EOF(1)); read(1,*) tfit; numlevtofit=numlevtofit+1;end do
   allocate(tf(numlevtofit))
   rewind(1)
	 do i=1,numlevtofit
	  read(1,*) tf(i)
	!  	 write(2,*) tf(i)
	 end do;close(1) !;close(2)
	 itemp=1
	 do i=1,numlevtofit
	 itemp=itemp*tf(i) 
	 end do
	 if(tf(1).eq.-1) then !calculate free-ion spectrum	 and exit
	  ret=info(); ret=matj(); ret=matls();  ret=FreeMem()
	  ret=DialogBox(hInst, LOC("RESULTS"C), NULL, LOC(results))
	  idatagl=1
	  deallocate(tf)
	 return
  	 end if

	 if(itemp.eq.1) then			!calculate CF spectrum and exit
	  ret=info(); ret=matj(); ret=matls(); ret=FreeMem(); ret=matj(); ret=finmatre()
	  ret=FreeMem()
	  ret=DialogBox(hInst, LOC("RESULTS"C), NULL, LOC(results))
	  idatagl=1
	  deallocate(tf)
	 return
	 end if

!	ireth=datastart()


	 
!****************Dialog Box for parameters slection********************************************
	ret=DialogBox(hInst, LOC("MASK"C),hWndfit, LOC(mask))
!**********************************************************************************************
	if(ret.eq.0) then
	 deallocate(tf)
	  return
	 else
	 !  open(2,file="dataout.txt");write(2,*) " Coulomb=",maskcoulomb;write(2,*) " spin=",maskspin
	  ! write(2,*) " Crystal=",maskcrystal; write(2,*) " Greek interaction=", maskgreek

!***********call to subroutine for selection of parameters for fitting********************************
									ret=changepar()
!********************dialog processed, begin input checking*******************************************

	   !write(2,*) "number of parameters ",numpartofit," number of levels ",numlevtofit
	  !close(2)
!	  if(numlevtofit.lt.numpartofit) then
!	   ret=MessageBox(0,"Number of levels is less then the number of parameters to fit!\nSupply additional levels or change the number of parameters."C,"WARNING!"C,MB_ICONSTOP)
!	   deallocate(tf)
!	   return
!	  end if
	  if(numpartofit.eq.0) then
	   ret=MessageBox(0,"Select at least one parameter for fitting"C,"WARNING!"C,MB_ICONSTOP)
	   deallocate(tf)
	   return
	  end if
	  if(numlevtofit.le.1) then
	   ret=MessageBox(0,"Number of levels should be greater then one"C,"WARNING!"C,MB_ICONSTOP)
	   deallocate(tf)
	   return
	  end if
	  allocate(icorr(numpartofit))
	  do i=1,numpartofit;icorr(i).tip=0;icorr(i).nom=0;icorr(i).rank=0;icorr(i).constr=0;end do
!***************BEGIN assignment of arguments for fitting subroutine******************************
	  m=numlevtofit		   !number of levels to fit
	  n=numpartofit 	   !number of parameters to fit
   	  allocate (rr(n,2),xguess(n),xlb(n),xub(n),xscale(n),fscale(m),fvec(m),x(n),fjac(m,n))
!**************set correlation between vector x and interaction parameters************************
	  ret=setcorr()
!	  write(2,'(4i5)') (icorr(i).tip,icorr(i).nom,icorr(i).rank,icorr(i).constr,i=1,numpartofit)
!*************************************************************************************************
	  call setinitguess(n,xguess,fscale)
	  !write(2,'(f10.2)') (xguess(i),i=1,numpartofit);close(2)
      ibtype=0             !user supplied bounds
	  call setbounds(n,xguess,xlb,xub)
	 !   write(2,'(2f10.2)') (xlb(i),xub(i),i=1,numpartofit)
	   do j=1,n
	    rr(j,1)=xlb(j)
		rr(j,2)=xub(j)
	   end do
	  
	  Bpk=Real(Bpk)  
	  !fscale=1.            !scaling parameters for function gradient, change if tolerance is different for levels
	  iparam(1)=0          !default values for iparam,rparam - preferred option
	  ldfjac=m             !leading dimension for differential jacobian matrix fjac 
	  if(fastfitflag.eq.0) then
		ret=info()
		ret=matj()
		ret=matls()
		ret=FreeMem()
	  else
	    ret=fastmatj()
		ret=sortfast()
	  end if
	  itefit=0
	  nd=numpartofit
	  np=5*nd
	  ret=genev(nd,np,crossover,noise,genmax,rr)
!  CALL UNLSF (FCN, M, N, XGUESS, XSCALE, FSCALE, IPARAM,RPARAM, X, FVEC, FJAC, LDFJAC)
!	  call BCLSF(fcn,m,n,xguess,ibtype,xlb,xub,xscale,fscale,iparam,rparam,x,fvec,fjac,ldfjac)
!	  do i=1,5;!	   ret=matj(); ret=finmatre(); ret=FreeMem();call fcn(m,n,x,fscale);!	  end do;!      write(2,50) x, fvec,iparam(3),iparam(4);!50 	  format(' The solution is',<numpartofit>f7.1,//,' The function evaluated at solution is ',//, &;!      18x,<numlevtofit>f6.2,//' The	number of iterations is ',10x, i3,/,&;!       'The number of function evaluation is ',i3,/);	  !close(2)
		 do i=8,10,2; ifrequency = 400*(11-i); iduration = 200;CALL BEEPQQ(ifrequency, iduration); end do
		 666	    deallocate (rr,xguess,xlb,xub,xscale,fscale,fvec,x,fjac)
			  !write(2,'(7i3)') ((iBpk(i,j),j=1,7),i=1,3); 	!write(2,*) " ielectro=",ielectro;write(2,*) " imagnetic=", imagnetic;				!close(2)
 		  deallocate(tf,icorr)
		 idatagl=1
		 return
	    end if
		fitexp=1
		call ExitThread(True)
		return

  end function fitexp

!**************prepare parameters for fitting****************************
 integer function changepar()

 changepar=0
 ielectro=0;imagnetic=0;iBpk=0
 numpartofit=0;icrystal=0
 select case(maskcrystal)

  case(1)			!cubic 
   iBpk(2,1)=1;iBpk(3,1)=1;iBpk(2,5)=101;iBpk(3,5)=101 !fixing the ratio for B44 and B64
   numpartofit=2;icrystal=1
  case(2)           !tetragonal  
   iBpk(1,1)=1;iBpk(2,1)=1;iBpk(3,1)=1
   iBpk(2,5)=1;iBpk(3,5)=1 
   numpartofit=5;icrystal=1
  case(3)			!trigonal
   iBpk(1,1)=1;iBpk(2,1)=1;iBpk(3,1)=1
   iBpk(2,4)=1;iBpk(3,4)=1;iBpk(3,7)=1
   numpartofit=6;icrystal=1
  case(4)			!hexagonal
   iBpk(1,1)=1;iBpk(2,1)=1;iBpk(3,1)=1;iBpk(3,7)=1
   numpartofit=4;icrystal=1
   case(5)
   numpartofit=0;icrystal=0

 end select 

 if(maskspin.eq.1) then
  imagnetic(1)=1;numpartofit=numpartofit+1
 end if

 do i=1,3
  if(maskgreek(i).eq.1) then
   ielectro(i+3)=1;numpartofit=numpartofit+1
  end if
 end do
 if(maskcoulomb.eq.1) then
  ielectro(1)=1;ielectro(2)=101;ielectro(3)=101
  numpartofit=numpartofit+1
 end if

 changepar=1; return
 end function changepar  

!*********************will be used in genetic module*********************

 real function costev(nd,x,iprint)
 !MS$ ATTRIBUTES STDCALL, ALIAS : '_costev@8' :: costev
 integer nd,iprint
 real :: x(nd)

! integer m,n
! real :: x(n)
! real :: fre(nd)
 real ave
 integer iRecords,i
 character*810 buf
 character*20 buft
 character*810,allocatable :: bufar(:)
 integer ibuft
 integer ind,ret
 integer hdc
 character*30 baveout

 n=nd
 m=numlevtofit
 itefit=itefit+1
! if(itefit.eq.1) then
!   open(20,file='results.txt',status='Replace')
! end if
 !write(20,1) "Iteration number ",itefit
 !1 format(/,A16,i5)
 do i=1,n
  ind=icorr(i).tip
  select case(ind)
   case(1)
    electro(icorr(i).nom)=x(i)
	if(icorr(i)%constr.eq.1) then
	 electro(2)=x(i)*fconstraint4
     electro(3)=x(i)*fconstraint6
	end if
   case(2)
    magnetic(icorr(i).nom)=x(i)
!	write(20,5) " spin-orbit=",x(i)
	5 format(a12,f9.2)
   case(3)
    Bpk(icorr(i).rank,icorr(i).nom)=x(i)
!	write(20,7) " cf ",icorr(i).rank*2,icorr(i).nom-1,x(i)
!	7 format(a4,i2,i2,f9.2)
	if(icorr(i)%constr.eq.2) then
	 Bpk(2,5)=ratcu4440*Bpk(2,1)
	 Bpk(3,5)=ratcu6460*Bpk(3,1)
	end if
  end select
 end do
 if(fastfitflag.eq.0) then
	 ret=matj()
	 ret=finmatre()
	 ret=FreeMem()
  else
	   ret=fastmatj()
       ret=sortfast()
  end if
	 iRecords=0
open(133,file='out.txt',access='direct',recl=810,form='formatted')
ioutpos=1
	do while (.NOT. EOF(133))
 	 read(133,'(A10)',rec=ioutpos) buf
	 ioutpos=ioutpos+1
	 buft=buf(3:8)
     read(buft,*) ibuft
	 !write(20,10) ibuft
	 !10 format(i6)
	 iRecords=iRecords+1
	end do
 allocate(bufar(iRecords),tfresult(iRecords))
	!rewind(133)
	do i=1,iRecords
	 read(133,'(A810)',rec=i) bufar(i)
	 buft=bufar(i)(3:8)
	 read(buft,*) tfresult(i)
	end do
 close(133)
 ave=0
 !fre=0.   
 itemp=0
 do i=2,m
  if(tf(i).ne.1) then
   !fre(i)=ABS(tf(i)-tfresult(i))
   ave=ave+ABS(tf(i)-tfresult(i))
   itemp=itemp+1
  end if
 end do
 ave=ave/itemp

! write(20,10) ' number ', 'exp.','calculated','deviation'
! 10 format(/,a7,6X,a4,5X,a10,3X,a10)
! do i=1,m
!  write(20,11) i, tf(i),tfresult(i),fre(i)
! 11 format(i4,4x,f8.1,4x,i8,4x,f7.1)
! end do
  ! write(20,20) ave
! 20 format(' Average deviation is ',f9.2)
 
  deallocate(bufar,tfresult)
  !close(20)
  if(iprint.eq.1) then
   hdc=GetDC(hWndfit)
	 write(bave,'(f8.1)') ave
	 baveout="Average deviation is "//bave
   	 ret=TextOut(hdc,220,100,baveout,30)
   ret=ReleaseDC(hWndfit,hdc)
   ret=DialogBox(hInst, LOC("RESULTSF"C), NULL, LOC(resultsfit))
   return
  end if
  costev=ave
  return
end function costev



!/****************************************************************************
!
!    FUNCTION: mask( hDlg, message, wParam, lParam )
!
!    PURPOSE:  Manual  selection of parameters to fit
!
!    MESSAGES:
!****************************************************************************

integer*4 function mask( hDlg, message, wParam, lParam )
!MS$ ATTRIBUTES STDCALL, ALIAS : '_mask@16' :: mask
integer*4   hDlg, message, wParam, lParam
integer*4 ret,i 

lParam=lParam
select case (message)
   case (WM_INITDIALOG)
	  ret=UpdateWindow(hDlg)
	 ret=CheckDlgButton(hDlg,IDC_PARC,maskcoulomb)
	 do i=IDC_RADIO_CF1,IDC_RADIO_CF5; ret=CheckDlgButton(hDlg,i,0); end do
	 do i=IDC_PARM1,IDC_PARM5; ret=CheckDlgButton(hDlg,i,0); end do
	 do i=IDC_PAR1,IDC_PAR3; ret=CheckDlgButton(hDlg,i,maskgreek(i-IDC_PAR1+1)); end do
     ret=CheckDlgButton(hDlg,IDC_PARM5,maskspin)
	 if(maskcrystal.ne.0)  then
	  ret=CheckDlgButton(hDlg,IDC_RADIO_CF1+maskcrystal-1,1)
	 end if
    case(WM_COMMAND)	
	  ret=UpdateWindow(hDlg)
	 select case(INT4(LoWord(wparam)))
	   case(IDOK)
	  	maskcoulomb=IsDlgButtonChecked( hDlg, IDC_PARC)
   		maskspin=IsDlgButtonChecked( hDlg, IDC_PARM5)
		do i=IDC_PAR1,IDC_PAR3
		 maskgreek(i-IDC_PAR1+1)=IsDlgButtonChecked( hDlg, i)
		end do
		do i=IDC_RADIO_CF1,IDC_RADIO_CF5
		 ret=IsDlgButtonChecked( hDlg,i)
		 if(ret.ne.0)  maskcrystal=i-IDC_RADIO_CF1+1
		end do   
	    ret=EndDialog(hDlg,TRUE)
	    mask=1;return
	   case(IDCANCEL)
	    ret=EndDialog(hDlg,FALSE)
	    mask=0; return

	 end select
end select
mask=1;return
end function mask



integer*4 function genev(nd,np,cr,noise,genmax,rr)

integer ::  nd,np,genmax
real :: cr,noise,score 
real,dimension(nd,2) :: rr

real, dimension(nd) :: trial
real, dimension(np) :: cost
real, dimension(np,nd) :: x1,x2

integer,save  :: nc,i,j,k,ia,ib,ic
integer,save :: iml(1)

!open(33,file="dataout.txt")
!write(33,*) "np=",np,"nd=",nd,"genmax=",genmax
genev=0
call parange(nd,np,x1,rr)
do i=1,np
 do j=1,nd
  trial(j)=x1(i,j)
 end do
 cost(i)=costev(nd,trial,0)
end do

!write(33,'(f15.3)') cost

do nc=1,genmax
  do i=1,np
    ia=(np-1)*RNUNF()+1.01;do while (ia.eq.i); ia=(np-1)*RNUNF()+1.01; end do
    ib=(np-1)*RNUNF()+1.01;do while (ib.eq.i.or.ib.eq.ia); ib=(np-1)*RNUNF()+1.01; end do
    ic=(np-1)*RNUNF()+1.01;do while (ic.eq.i.or.ic.eq.ia.or.ic.eq.ib); ic=(np-1)*RNUNF()+1.01; end do
     !  write(*,*) 'i=',i,' ia=',ia,' ib=',ib,' ic=',ic
    !j=INT((nd-1)*RNUNF()+1.01)

   j=1
   do k=1,nd
   	!  if(j.eq.0) 	  j=INT((nd-1)*RNUNF()+1.01)
	!   write(33,*) j
  	  if(RNUNF().lt.cr.or.k.eq.nd) then
	   trial(j)=x1(ic,j)+noise*(x1(ia,j)-x1(ib,j))
	   if(trial(j).gt.rr(j,2).or.trial(j).lt.rr(j,1)) trial(j)=x1(ic,j)
	  else
	   trial(j)=x1(i,j)
	  end if
	  j=j+1
	 ! j=MODULO(j+1,nd)
   end do
   score=costev(nd,trial,0)
   if(score.le.cost(i)) then
    cost(i)=score;do k=1,nd;x2(i,k)=trial(k);end do
   else
	do k=1,nd; x2(i,k)=x1(i,k); end do
   end if
  end do
  x1=x2
 !write(33,'(A20,i5,a15)') 'generation number ',nc,' okuklilos'
end do
iml=MINLOC(cost)
do j=1,nd
 !write(33,*) 'parameter ',j,'=',x1(iml,j)
 trial(j)=x1(iml(1),j)
end do
! write(33,*) 'cost value=',cost(iml)
!write(33,*) 'cost value=',score
!close(33)

cost=costev(nd,trial,1)
genev=1;return
end function genev




subroutine parange(nd,np,x,r)
real,dimension(np,nd) :: x
real,dimension(nd,2) :: r
integer i,j,nd,np

do i=1,np
 do j=1,nd
  x(i,j)=RNUNF()*(r(j,2)-r(j,1))+r(j,1)
 end do
end do
end subroutine parange 


 


end module expfit