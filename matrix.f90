!/***********matj - formation of interaction matrix for LS-J coupling*********
!************num - database search***********************************
!**********finmat - make final matrix and eigensystem*******************
!***********fromj - array jm from given j*******************************
!*********conel,concr - electrostatic and cr.field interactions***************
!*********FreeMem    - free memory before another configuration***********
module matrix
use dataman
use matel
use pardia
use msimsl
use spinc


type full
 real*4 :: els	     !coefficients at fk +albega +tk
 real*4     spin          		 !coefficient at spin-orbit parameter +mk +pk				
 real*4,dimension(3) :: cr     ! crystal field coefficients  
end type full

type elinfo
 integer, dimension(3) :: i	   !2*L, 2*S, 2*J
 character(1) c							!additional identificator  
end type elinfo

type (elinfo), allocatable,save :: inf(:)
type (full), allocatable,save :: fu(:,:)

type(recr),allocatable :: rcrb(:),rcrbfull(:)
type(reel),allocatable :: relb(:),relbfull(:)
type(respin), allocatable :: respinb(:),respinbfull(:)
type(recr) rcr
type(reel) rel
type(respin) rsp

integer,save   :: irez,jrez
integer,save :: idatagl=1

contains

integer function matj()
!use msfwina 
  integer, allocatable :: gtot(:)
  integer, allocatable :: iS(:),iL(:)
  character(1), allocatable :: cad(:)
  real, allocatable :: grh(:), grl(:)
  character(3), allocatable :: cLS(:)
  integer, allocatable :: ic(:)
  type (elinfo), allocatable :: ti(:),tinf(:)
  integer  nt,iflag,nr,i,j,id,k,item,itmp		!err
  character(3)  c1,c2
  character*3 cinst

  integer idumpname
  real  xtem
                                                        ! character(3) ci1,ci2;! real a
  character(1)  ct
 character*7 FileStatus 
 character*22 dumpfilename
 logical IsThere     
  !type(red) r
  !type(recr) rcr
  !type(reel) rel
  !type(respin) rsp
  integer inelec

 ! character*8, dimension(3) ::   dbfilestr
  data nt,iflag,nr,i,j,id,k,item,itmp/9*0/

  irez=0; jrez=0;  matj=0
  inelec=nelec
   select case(ie);
    case(2);if(nelec.gt.5) inelec=10-inelec
    case(3);if(nelec.gt.7) inelec=14-inelec
   end select
   !dbfilestr(1)=dbfilecr(1);case(3);dbfilestr=dbfile(2);end select

!		open(23,file="matj.txt")

!********read information about fn configuration*********************************
  open(77,file='temp.txt',status='old'); rewind(77)
  read(77,'(I4)') nt; 

!		write(23,*) nt

  !write(*,*) nt; 
  allocate(iS(nt),iL(nt),cad(nt),cLS(nt))
  allocate (grl(1:nt),grh(1:nt),gtot(1:nt))
  iS=0;iL=0;cad=' ';cLS=' ';grl=0.;grh=0.;gtot=0.
 
  allocate(ti(1),inf(1)); ti(1).i=0;inf(1).i=0;inf(1).c=' '

!		write(23,*) "Memory allocated"

  do i=1,nt
   read(77,20) iS(i),iL(i),cad(i),cLS(i)
!   write(23,20)  iS(i),iL(i),cad(i),cLS(i)
!   write(*,20) iS(i),iL(i),cad(i),cLS(i)
 20 format(I2,I2,A1,A3)
   grl(i)=ABS(iL(i)-iS(i)/2.)     !lower limit for J states in a given term
   grh(i)=ABS(iL(i)+iS(i)/2.)	   !upper limit for J states in a given term
   gtot(i)=INT(grh(i)-grl(i)+1)		   !number of J states
   allocate(tinf(1:gtot(i)))
   jrez=jrez+gtot(i)
   do k=1,gtot(i)
    tinf(k).i(1)=2*iL(i);tinf(k).i(2)=iS(i);tinf(k).c=cad(i)
   end do
  !write(*,*) iS(i)/2.,iL(i),grl(i), grh(i),gtot(i)
  !read(*,*)
   allocate(ic(0:gtot(i)-1)); ic=0
   do ij=0,gtot(i)-1
    ic(ij)=2*(grl(i)+ij)+1		    !number of MJ states in a given J state
    irez=irez+ic(ij)
	tinf(ij+1).i(3)=(grl(i)+ij)*2
   end do
   deallocate(inf)
   allocate(inf(1:jrez))
	 do k=1,MAX(1,jrez-gtot(i))
	  inf(k)=ti(k)
	 end do
     do k=jrez-gtot(i)+1,jrez
	  inf(k)=tinf(k-jrez+gtot(i))
     end do
   deallocate(ti)
   allocate(ti(1:jrez))
	do k=1,jrez
     ti(k)=inf(k)
	end do
   deallocate(ic,tinf)
 end do	
!		write(23,*)   "Beginning database search"  
 
 close(77); deallocate(ti); 
 deallocate (grl,grh,gtot);deallocate(iS,iL,cad,cLS)

!************finish reading information from unit 77****************************
!**********finish building information vector inf*******************
! write(*,*) ' number of J states=',jrez,' number of jm states=',irez

 allocate(fu(jrez,jrez))
 do i=1,jrez;do j=1,jrez;fu(i,j).spin=0.;fu(i,j).els=0.;fu(i,j).cr=0.;end do;end do
 
 idumpname=ABS(MODULO(hMWindow,999))
 if(idumpname.lt.100) idumpname=idumpname+100
 write(cinst,'(I3)') idumpname
 write(c2,'(I1)') inelec

!	  write(23,*) hMWindow
!	  write(23,*)   "checking dump files"

 dumpfilename="C:"//"\\Spectra\\"//"Dump\\"//cinst
 INQUIRE (FILE =dumpfilename//"."//"1", EXIST = IsThere,iostat=irr)

!			write(23,*)   IsThere, " dump 1 checked "//dumpfilename//"."//"1",irr

      IF (IsThere) THEN   
	   if(idatagl.eq.1) then
	    FileStatus = 'REPLACE'                ! it isn't there, so create it
	    ifilee=0
       else 
        FileStatus = 'OLD'          ! erase and start again
	    ifilee=1
	   end if
      ELSE
       FileStatus = 'NEW'                ! it isn't there, so create it
	   ifilee=0
     END IF
	
   !open(1,file=dumpfilename//"."//"1",status=FileStatus,access='direct',iostat=irr,RECL=RecLenel)
   idump1=1
   open(1,file="675.1",access='direct',RECL=RecLenel)

	 !100   write(23,*)   "file is not created! error=",irr;close(23);return
!	 write(23,*)   "file 1 created ",irr,FileStatus

INQUIRE (FILE =dumpfilename//"."//"2", iostat=irr,EXIST = IsThere)
	
!	  write(23,*)   IsThere, " dump 2 checked",irr

      IF (IsThere) THEN 
       if(idatagl.eq.1) then
	    FileStatus = 'REPLACE'                ! it isn't there, so create it
	    ifiles=0
       else 
        FileStatus = 'OLD'          ! erase and start again
	    ifiles= 1
	   end if
      ELSE
       FileStatus = 'NEW'                ! it isn't there, so create it
	   ifiles=0
     END IF
   !open(2,file=dumpfilename//"."//"2",status=FileStatus,access='direct',iostat=irr,RECL=RecLensp)
   open(2,file="675.2",access='direct',RECL=RecLensp)
   idump2=1

!			   write(23,*)   "file 2 created ",irr,FileStatus


  INQUIRE (FILE =dumpfilename//"."//"3",iostat=irr, EXIST = IsThere)
!	  	  write(23,*)   IsThere," dump 3 checked",irr

      IF (IsThere) THEN    
       if(idatagl.eq.1) then
	    FileStatus = 'REPLACE'                ! it isn't there, so create it
	    ifilec=0
       else
        FileStatus = 'OLD'          ! erase and start again
	    ifilec=1
	   end if
      ELSE
       FileStatus = 'NEW'                ! it isn't there, so create it
	   ifilec=0
     END IF
!   open(3,file=dumpfilename//"."//"3",status=FileStatus,iostat=irr,access='direct',RECL=RecLencr)
   idump3=1
   open(3,file="675.3",access='direct',RECL=RecLencr)

	!write(23,*)   "file 3 created",irr,FileStatus

					idatagl=1



  do i=1,jrez
  do j=1,jrez
   itmp=inf(i).i(2)+1
   write(ct,'(i1)') itmp
   c1=ct//toL(inf(i).i(1)/2)//inf(i).c
   itmp=inf(j).i(2)+1
   write(ct,'(i1)') itmp
   c2=ct//toL(inf(j).i(1)/2)//inf(j).c
!		  write(23,*)   "spin-orbit", i,j

!***************filling spin-orbit interaction*******************************
   if(inf(i)%i(3).NE.inf(j)%i(3))  then
    fu(i,j).spin=0.
   else
	do id=1,7
	 if(ifiles.eq.0) then
	  call numspin(inf(i)%i(3),id,c1,c2,nr,iflag)
      if(nr.NE.0) then; rsp=respinb(nr); else;  rsp.fk=0.; end if
	  write(2,rec=idump2) rsp
	  idump2=idump2+1
	 else
	  read(2,rec=idump2) rsp
	  idump2=idump2+1
	 end if
!     write(*,*) c1,c2,nr, r.fc, depr(r.fc,r.pr)
      xtem=rsp.fk
	  fu(i,j).spin=fu(i,j).spin+xtem*magnetic(id)
	end do
   end if
!*******************end filling spin-orbit**********************************

!			  	  write(23,*)   "crystal-field",i,j


!*********crystal field matrix elements <LSJ||Uk||L'S'J'>********************
   if(inf(i)%i(2).EQ.inf(j)%i(2)) then
    if(ifilec.eq.0) then
      call numcr(c1,c2,nr,iflag)
       if(nr.NE.0) then; rcr=rcrb(nr); else;  rcr.uk=0.; end if
 	    write(3,rec=idump3) rcr
		idump3=idump3+1
	   else 
	    read(3,rec=idump3) rcr
		idump3=idump3+1
     end if
     do id=1,3
       if(iflag.NE.0) then
	    item=inf(i).i(1)-inf(j).i(1)
	    if(MOD(item/2,2).ne.0) rcr.uk(id)=-rcr.uk(id) 
	   end if
 	   xtem=rcr.uk(id)*&
	   SIXJ(inf(i).i(3),inf(j).i(3),4*id,inf(j).i(1),inf(i).i(1),inf(i).i(2))*&
	   SQRT((inf(i).i(3)+1.)*(inf(j).i(3)+1.))
	   item=inf(i).i(2)+inf(j).i(1)+inf(i).i(3)+4*id
	   if(MOD(item/2,2).ne.0) xtem=-xtem
	   fu(i,j).cr(id)=xtem
	 end do
	else
	  do id=1,3; fu(i,j).cr(id)=0.;end do
    end if

!*******************end crystal field filling********************************
!			   	  write(23,*)   "electrostatic",i,j

!*******************electrostatic energy*************************************
	do id=1,12
	 if(inf(i)%i(3).NE.inf(j)%i(3))  then
	  fu(i,j).els=0.
     else
      if(ifilee.eq.0) then
	   call numel(id,c1,c2,nr,iflag)
       if(nr.NE.0) then; rel=relb(nr); else;  rel.fk=0.; end if
 	    write(1,rec=idump1) rel
		idump1=idump1+1
       else 
	    read(1,rec=idump1) rel
		idump1=idump1+1
       end if
       fu(i,j).els=fu(i,j).els+rel.fk*electro(id)
	  end if	
    end do
  !if(c1.eq.ci1.and.c2.eq.ci2) 	write(*,*) c1,' ',c2,' ',INT1(inf(i).i(3)/2),' ',INT1(inf(j).i(3)/2),'energy',a
   end do
!   write(*,'(A1)',advance='NO') CHAR(177)
  end do
  close(1);close(2); close(3)
!					 	  write(23,*)   "files closed"
!						  close(23)

! write(*,*)
 matj=1
end function matj

!*****************************FASTMATJ******************************
integer function fastmatj
!*********************************************************************
 use fast

 implicit none
 integer inelec
 real(8), allocatable :: A(:,:), Am(:,:),Redsp(:,:,:),Redel(:,:,:),Redcr(:,:,:),evec(:,:),evecm(:,:)
 real(8), allocatable :: eval(:),evect(:),evalm(:),evectm(:)
 real(8) xtem
 real(8),dimension(3) :: redcom
 integer  lda,ldevec,i,j2value,k,l,ierr,jnum, lsrez, iflag,isearch,id,nr,m,n,n1
 integer item, ich1,ich2, iS1,iS2
 character*3 chj
 character(3), allocatable :: LSdes(:)
 CHARACTER(10) t,t1
 CHARACTER(5) z
 character*3  ch1,ch2
  integer, allocatable :: jm(:)

!CALL DATE_AND_TIME(TIME=t,ZONE=z)
  jnum=0 
  inelec=nelec
open(1,file='debug.txt', access='append')
write(1,*) 'fastmat accessed'
close(1)
open(13,file='mat.txt')
open(33,file="val.txt")
!write(1,*) 'time started(hhmmss)=', t
   select case(ie);
    case(2);if(nelec.gt.5) inelec=10-inelec
    case(3);if(nelec.gt.7) inelec=14-inelec
   end select
   jnum=(blJ(inelec).Jmax-blJ(inelec).Jmin)/2+1
 ! write(1,*) jnum
iflag=0
write(33,'(i4)') jnum
do i=1,jnum
  !write(1,*) 'i begin',i
 j2value=blJ(inelec).Jmin+2*(i-1)		!the 2J value for the matrix of LS-terms
  !write(1,*) 'j2value',j2value
  !write(1,*) '3i-2', 3*i-2,'3i',3*i
 chj=blJ(inelec).number(3*i-2:3*i)
  !write(1,*) blJ(inelec).number(3*i-2:3*i),chj
 read(chj,'(i3)') lsrez
 write(33,'(3i4)') i,j2value,lsrez
 allocate(LSdes(lsrez))
!starting the main loop for J-matrixes diagonalization********************
 do	 k=1,lsrez
	chj=blJ(inelec).list(3*k-2+iflag:3*k+iflag)
	LSdes(k)=chj
 end do
 iflag=iflag+3*lsrez
 !do k=1,lsrez;	
 !write(1,'(a3)') LSdes(k); 
 !end do 
 allocate(Redsp(lsrez,lsrez,7),Redel(lsrez,lsrez,12),Redcr(lsrez,lsrez,3))
 if(ifastmat.eq.0) then
  do k=1,lsrez
   do l=1,lsrez
	do id=1,7
	  call numspin(j2value,id,LSdes(k),LSdes(l),nr,isearch)
      if(nr.NE.0) then; rsp=respinb(nr); else;  rsp.fk=0.; end if
      Redsp(k,l,id)=rsp.fk
	end do
	do id=1,12
	   call numel(id,LSdes(k),LSdes(l),nr,isearch)
       if(nr.NE.0) then; rel=relb(nr); else;  rel.fk=0.; end if
 	   Redel(k,l,id)=rel.fk
    end do
   end do
  end do
write(13,*) lsrez;write(13,*) Redsp;write(13,*) Redel
else
 read(13,*) lsrez; read(13,*) Redsp; read(13,*) Redel
end if
	allocate(A(lsrez,lsrez));A=0.0d00
	do k=1,lsrez
	 do l=1,lsrez
!here goes the code to fill A matrix
	  do id=1,7
       A(k,l)=A(k,l)+Redsp(k,l,id)*magnetic(id)
	  end do
  	  do id=1,12
       A(k,l)=A(k,l)+Redel(k,l,id)*electro(id)
	  end do
	 end do  
	end do
	do k=1,lsrez
		do l=1,lsrez
			if(DABS(A(k,l)).lt.1.d-4) A(k,l)=0.d00
		end do
	end do
	lda=lsrez;	ldevec=lsrez
	allocate(eval(lda),evec(lda,lda),evect(lda))
	eval=0.d00;evec=0.d00;evect=0.d00
	CALL DEVCSF (lsrez, A,lda, eval, evec,ldevec)
	do m=1,lda
!		 write(1,*) 'm begin',m
		write(33,'(f20.6)') eval(m)
    !    write(33,*) '-------------------------------------------------------------------------'
	   do n=1,lda
		 write(33,'(f20.6,x,a3)') evec(n,m),LSdes(n)
	   end do
     !   write(33,*) '-------------------------------------------------------------------------'
	end do
    if(ifastmat.eq.0) then
		do k=1,lsrez
		 do l=1,lsrez
			ch1=LSdes(k);ch2=LSdes(l)
			call numcr(ch1,ch2,nr,isearch)
			if(nr.NE.0) then; rcr=rcrb(nr); else;  rcr.uk=0.; end if
			ich1=2*idL(ch1(2:2));ich2=2*idL(ch2(2:2))
			do id=1,3
				if(isearch.NE.0) then
					item=ich1-ich2
					if(MOD(item/2,2).ne.0) rcr.uk(id)=-rcr.uk(id) 
				end if
				read(ch1(1:1),'(i1)') iS1;	 read(ch2(1:1),'(i1)') iS2
				iS1=iS1-1;	 iS2=iS2-1
				if(iS1.eq.iS2) then
 					xtem=rcr.uk(id)*&
					SIXJ(j2value,j2value,4*id,ich2,ich1,iS1)*SQRT((j2value+1.)*(j2value+1.))
					item=iS1+ich2+j2value+4*id
					if(MOD(item/2,2).ne.0) xtem=-xtem
					Redcr(k,l,id)=xtem
				 else
					Redcr(k,l,id)=0
				 end if
		     end do
!			 write(1,*) ch1,ch2,(Redcr(k,l,id),rcr.uk(id),id=2,2)
		end do
	   end do
	   write(13,*) Redcr
	 else
	   read(13,*) Redcr
	 end if
	item=j2value+1
	do m=1,lsrez
	   redcom=0.
	   do n=1,lsrez
	    do n1=1,lsrez
			do id=1,3
			  redcom(id)=redcom(id)+Redcr(n,n1,id)*evec(n,m)*evec(n1,m)
			end do
!			write(1,*) 'n=',n,' n1=',n1, 'redcom=',redcom
		end do
	   end do
!		write(1,*) 'redcom ',redcom
		allocate(Am(item,item),evalm(item),evecm(item,item),evectm(item))
		allocate(jm(item))
		call fromj(j2value,jm)	
		do k=1,item
			do l=1,item
				Am(k,l)=concrfast(j2value,jm(k),jm(l),0,redcom)
			end do
		end do
	   evalm=0.d00;evecm=0.d00;evectm=0.d00
	   CALL DEVCSF (item, Am,item, evalm, evecm,item)
	   write(33,'(f20.6)') eval(m)
	   do k=1,item
		write(33,'(f20.6)') evalm(k)
 !       write(33,*) '-------------------------------------------------------------------------'
		do l=1,item
			write(33,'(f20.6,i4)') evecm(l,k),jm(l)
		end do
!        write(33,*) '-------------------------------------------------------------------------'
	   end do

 		deallocate(evalm,evecm,evectm,stat=ierr)
		deallocate(Am,stat=ierr)
		deallocate(jm)
!	   write(1,*) 'm finished',m
	 end do
	deallocate(LSdes,Redsp,Redel,Redcr)
	deallocate(eval,evec,evect,stat=ierr)
	deallocate(A,stat=ierr)
!	write(1,*) 'i finished',i
 end do
 !*****	main loop finished	*******************************************
! CALL DATE_AND_TIME(TIME=t1,ZONE=z)
!read(t1(3:4),*) k;read(t(3:4),*) l
!if(k.lt.l) l=l-60
!read(t1(5:6),*) m;read(t(5:6),*) n
!if(m.lt.n) n=n-60
!write(1,*) 'time finished(hhmmss)=',t1,' elapsed',INT2(k-l),'min',INT2(m-n),'sec'
!close(1)
close(13)
close(33)
ifastmat=1
fastmatj=1

end function fastmatj

!!!!!!!!!!!!!!!!!!!!!!!!		FUNCTION SORTFAST		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function sortfast()
implicit none
  integer,save ::  jnum
  integer i,i1,i2,i3, i4,ir,j,j2value,lsrez,item
  integer l,m,n,k,ltmp
  character(3), allocatable :: LSdes(:)
  real(8), allocatable :: evect(:)

  real(8), allocatable :: 	 evalfin(:),  elvecfin(:,:),evaltemp(:),elvectemp(:,:)
  logical IsThere
  integer ioutpos,ntemp, ntemp1
  character*7 FileStatus
  integer, dimension(1) :: imloc=(/0/)
  real*8 u,xtem,elow
  real etemp
  character*20 sbuft
  character*810 sbuf
  character*3 LSt
  CHARACTER(10) t,t1
  CHARACTER(5) z


  type desform
   integer S
   integer L
   integer J
   integer Jm
  end type desform

 type(desform), allocatable :: form(:,:)
 integer, allocatable :: arrayofj2(:), arrayofls(:)
 integer totstark, totwave, icounter, iwavecounter

	CALL DATE_AND_TIME(TIME=t,ZONE=z)
	open(33,file="val.txt")
!	open(34,file='valprim.txt')
!	write(34,*) 'time started(hhmmss)=', t

	read(33,'(i4)') jnum !the number of J-multiplets
	allocate(arrayofj2(jnum), arrayofls(jnum))
	do i=1,jnum
	 	read(33,'(3i4)') ir,j2value,lsrez 		!ir-number of multiplet, j2value-2j value,lsrez-number of LS multiplets
		arrayofj2(i)=j2value;arrayofls(i)=lsrez
		item=j2value+1
	 	do i1=1,lsrez
			read(33,'(f20.6)') etemp
		   	do i2=1,lsrez;read(33,'(f20.6,x,a3)') etemp,LSt;end do
		 end do
         do i1=1,lsrez
			read(33,'(f20.6)') etemp
			do i2=1,item
				read(33,'(f20.6)') etemp
				do i3=1,item;read(33,'(f20.6,i4)') etemp,ntemp;	end do
     		end do
		  end do
	end do
	rewind(33)
	read(33,'(i4)') jnum

	totstark=0

	do i=1,jnum
	 totstark=totstark+(arrayofj2(i)+1)*arrayofls(i)
	end do

	totwave=MAXVAL(arrayofls)*(arrayofj2(jnum)+1)
!	write(34,'(3i5)') MAXVAL(arrayofls),arrayofj2(jnum)+1,totwave
	allocate(evalfin(totstark), elvecfin(totwave,totstark),form(totwave,totstark)) 
	evalfin=0.;elvecfin=0.
	icounter=0		   ! counter for Stark levels
	do i=1,jnum
	 
	   !iwavecounter=0 !	counter for wavefunctions components
	   read(33,'(3i4)') ir,j2value,lsrez
	   item=j2value+1
	   allocate(evaltemp(lsrez),elvectemp(lsrez,lsrez),LSdes(lsrez))
	   do i1=1,lsrez
			read(33,'(f20.6)') 	   etemp
		   	do i2=1,lsrez
				read(33,'(f20.6,x,a3)') elvectemp(i2,i1), LSdes(i2)
			end do
		end do
!		write(34, *) LSdes
        do i1=1,lsrez
			read(33,'(f20.6)')	  evaltemp(i1)
			do i2=1,item
				read(33,'(f20.6)')	etemp
				icounter=icounter+1
				evalfin(icounter)=evaltemp(i1)+etemp
				iwavecounter=0
		   		do i3=1,item
	   				read(33,'(f20.6,i4)') etemp, ntemp
					do i4=1,lsrez
						iwavecounter=iwavecounter+1
						elvecfin(iwavecounter,icounter)=etemp*elvectemp(i4,i1)
						LSt=LSdes(i4)
						form(iwavecounter,icounter).L=idL(LSt(2:2))*2
						read(LSt(1:1),'(i1)') ntemp1
						form(iwavecounter,icounter).S=ntemp1-1
					  	form(iwavecounter,icounter).Jm=ntemp
				 		form(iwavecounter,icounter).J=j2value
!					   	write(34,'(6(i3),2(f20.6))')  iwavecounter,icounter, form(iwavecounter,icounter).L,form(iwavecounter,icounter).S,form(iwavecounter,icounter).J,form(iwavecounter,icounter).Jm,evalfin(icounter),elvecfin(iwavecounter,icounter)
					 end do
				end do						      
     		end do
		  end do
		  deallocate(evaltemp,elvectemp,LSdes)	  
!	  write(34,*) iwavecounter, icounter
	end do
	close(33)
!CALL DATE_AND_TIME(TIME=t1,ZONE=z)
!read(t1(3:4),*) k;read(t(3:4),*) l;if(k.lt.l) l=l-60;read(t1(5:6),*) m
!read(t(5:6),*) n;	if(m.lt.n) n=n-60
!write(34,*) 'cycle ',i,'finished(hhmmss)=',t1,' elapsed',INT2(k-l),'min',INT2(m-n),'sec'
	deallocate(arrayofj2, arrayofls)
	


!******begin output formatting******************************************
	where(evalfin.eq.0.) evalfin=1.d10
	elow=MINVAL(evalfin)
	do k=1,totstark; evalfin(k)=evalfin(k)-elow; end do
	where(DABS(evalfin).LT.1.d-3) evalfin=0.d00

	INQUIRE (FILE = 'out.txt', EXIST = IsThere)
	IF (IsThere) THEN            
		FileStatus = 'REPLACE'          ! erase and start again
	ELSE
		FileStatus = 'NEW'                ! it isn't there, so create it
	END IF
	open(111,file='out.txt',status=FileStatus,access='direct',recl=810,form='formatted')
	ioutpos=1
	do k=1,totstark
		imloc=MINLOC(evalfin); u=evalfin(imloc(1))
		ltmp=imloc(1)
		where(DABS((evalfin-u)/(u+0.01)).LT.1.d-8)  evalfin=evalfin+1.d9
		allocate(evect(totwave))
		evect=0.
!****************OUTPUT eigenvalues***************************************
		 if(u<1.d9) then
			sbuft='';sbuf=''
			j=0
			do i=1,totwave 
				evect(i)=REAL(elvecfin(i,ltmp))
				xtem=DABS(evect(i))*DABS(evect(i))
				if(xtem.gt.wavefix) then
					j=j+1
					if(j.le.numfix) then
						write(sbuft,5)  form(i,ltmp).S+1,form(i,ltmp).L,&
								form(i,ltmp).J, form(i,ltmp).Jm,evect(i),0.
						5     format(I1,I2,I2,I3,f6.3,f6.3)  
					end if
				end if
				sbuf(11+(j-1)*(20):10+j*20)=sbuft
			end do
			write(sbuft,'(F8.1)') u
			sbuf(3:10)=sbuft
			write(sbuft,'(I2)') j
			sbuf(1:2)=sbuft
			write(111,'(A810)',rec=ioutpos) sbuf
			ioutpos=ioutpos+1
		 end if
		 deallocate(evect)
	end do
close(111)
!CALL DATE_AND_TIME(TIME=t1,ZONE=z)
!read(t1(3:4),*) k;read(t(3:4),*) l
!if(k.lt.l) l=l-60
!read(t1(5:6),*) m;read(t(5:6),*) n
!if(m.lt.n) n=n-60
!write(34,*) 'time finished(hhmmss)=',t1,' elapsed',INT2(k-l),'min',INT2(m-n),'sec'
!close(34)
deallocate(evalfin, elvecfin,form)

!**************here ends the old version*******************************
666 continue
	sortfast=1
end function sortfast


real function concrfast(j2value,m,m1,iflag,redcom)
implicit none
integer j2value,m,m1,iflag;integer :: im,j,j1,it,k,itt,item=0
real :: x,xtem=0.;real, dimension(3) :: u,w=0.;logical, dimension(3) :: lt1,lt2
integer, dimension(3) :: ifmask
real*8 , dimension(3) :: redcom

lt1=.False.; lt2=.False.
ifmask=0
concrfast=0.;xtem=(m-m1)/2.;
if(xtem-INT(xtem).NE.0.) return

do im=1,3
 if(redcom(im).ne.0) ifmask(im)=1
end do
if(ifmask(1)+ifmask(2)+ifmask(3).eq.0) return

it=INT(xtem);j=j2value;j1=j !!!!!!!!!!!!!!!!! change if J-J' mixing inf(i1).i(3)
if(ABS(it).gt.6) return
do k=1,3; itt=ABS(it)+1; u(k)=REAL(Bpk(k,itt)); w(k)=AIMAG(Bpk(k,itt));end do
where(ABS(u).LT.0.5) lt1=.True.;where(ABS(w).LT.0.5) lt2=.True.
if(ALL(lt1).AND.ALL(lt2)) return
select case(iflag)
 case(0)
  if(it.ge.0) then
   x=0.
   do k=1,3
     xtem=redcom(k)*u(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	 item=j-m+2*it
	 if(MOD(item/2,2).ne.0) xtem=-xtem; x=x+xtem
   end do
   concrfast=x;return
  else
   x=0.
   do k=1,3
      xtem=redcom(k)*u(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	   item=j-m
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concrfast=x;return
  end if
 case(1)
  if(it.ge.0) then
   x=0.
   do k=1,3
      xtem=redcom(k)*w(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
      item=j-m+2*it
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concrfast=x
  else
   x=0.
   do k=1,3
	 xtem=redcom(k)*w(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	item=j-m+2
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concrfast=x;return
  end if
end select

end function concrfast


!****************************************************************
!*Block of primitive searching procedures********************************
!*hash tables are not useful here - too many random absent(zero value) records
!***************************************************************
!****************************************************************

subroutine numel(id,cha1,cha2,nr,flag)
integer id,nr,flag
character*3 cha1,cha2 ;	integer :: i,irec=0
type (reel) r
  nr=0;irec=0
  do i=1,SIZE(relb)
   r=relb(i)
	if(r%id.eq.id) then
     if(r%iLS(1).EQ.cha1.AND.r%iLS(2).EQ.cha2) then
	  nr=irec+1; flag=0; return
	  else			   
	   if(r%iLS(1).EQ.cha2.AND.r%iLS(2).EQ.cha1) then
	    nr=irec+1; flag=1; return
	   end if
	 end if
	end if
   irec=irec+1
  end do
end subroutine numel

subroutine numcr(cha1,cha2,nr,flag)
integer nr,flag
character*3 cha1,cha2 ;	integer :: i,irec=0
type (recr) r
  nr=0;irec=0
  do i=1,SIZE(rcrb)
   r=rcrb(i)
     if(r%iLS(1).EQ.cha1.AND.r%iLS(2).EQ.cha2) then
	  nr=irec+1; flag=0; return
	  else			   
	   if(r%iLS(1).EQ.cha2.AND.r%iLS(2).EQ.cha1) then
	    nr=irec+1; flag=1; return
	   end if
	 end if
   irec=irec+1
  end do
end subroutine numcr

subroutine numspin(ij,id,cha1,cha2,nr,flag)
integer ij,id,nr,flag
character*3 cha1,cha2 ;	integer :: i,irec=0
type (respin) r
  nr=0;irec=0
  do i=1,SIZE(respinb)
   r=respinb(i)
     if(r%j.eq.ij) then
	  if(r%id.eq.id) then
       if(r%iLS(1).EQ.cha1.AND.r%iLS(2).EQ.cha2) then
	    nr=irec+1; flag=0; return
	   else			   
	    if(r%iLS(1).EQ.cha2.AND.r%iLS(2).EQ.cha1) then
	     nr=irec+1; flag=1; return
	    end if
	   end if
	  end if
	 end if
   irec=irec+1
  end do
end subroutine numspin

!****************optimization of matrix elements***************
integer function matls()
real(8), allocatable :: A(:,:), evec(:,:)
real(8), allocatable :: eval(:),evect(:)
real(8) elow,u
 integer  lda,ldevec,i,j,k
 character*7 FileStatus, FileStatusTemp 
 logical IsThere                                            
 integer, dimension(1) :: imloc=(/0/)
 real xtem
 character*20 sbuft
 character*810 sbuf
 character*1 cad,cbuf
 character*1,allocatable :: cadnew(:)
 character*3 cht
 character*3,allocatable :: chtnew(:)
 integer, allocatable :: in1(:),in2(:)
 real, allocatable :: en(:)


 lda=jrez;	ldevec=jrez
  allocate(A(jrez,jrez));A=0.0d00
 do i=1,jrez
 do j=1,jrez
     A(i,j)=fu(i,j).spin+fu(i,j).els
 end do  
end do
do i=1,jrez
 do j=1,jrez
	if(DABS(A(i,j)).lt.1.d-4) A(i,j)=0.d00
 end do
end do

! where(DABS(A).lt.1.d-3) A=0.d00 
allocate(eval(lda),evec(lda,lda),evect(lda))
eval=0.d00;evec=0.d00;evect=0.d00

CALL DEVCSF (jrez, A,lda, eval, evec,ldevec)
 elow=MINVAL(eval)
do k=1,lda; eval(k)=eval(k)-elow; end do
where(DABS(eval).LT.1.d-2) eval=0.d00

INQUIRE (FILE = 'out.txt', EXIST = IsThere)
IF (IsThere) THEN            
 FileStatus = 'REPLACE'          ! erase and start again
ELSE
 FileStatus = 'NEW'                ! it isn't there, so create it
END IF
INQUIRE (FILE = 'tempold.txt', EXIST = IsThere)
IF (IsThere) THEN; FileStatusTemp = 'REPLACE' ;ELSE; FileStatusTemp = 'NEW';END IF

open(1,file='out.txt',status=FileStatus,access='direct',recl=810,form='formatted')
open(2,file='tempold.txt',status=FileStatusTemp)
ioutpos=1
do k=1,lda
 imloc=MINLOC(eval); u=eval(imloc(1))
 where(DABS((eval-u)/(u+0.01)).LT.1.d-8)  eval=eval+1.d9
  if(u.lt.1.d9) then
   sbuft='';sbuf=''
   j=0
   do i=1,lda
    evect(i)=evec(i,imloc(1));  xtem=DABS(evect(i))
	if(xtem.gt.wavefix) then;	 j=j+1
	 if(j.le.numfix) then
	  write(sbuft,5) inf(i).i(2)+1,inf(i).i(1), inf(i).i(3), 0,evect(i),0.
5     format(I1,I2,I2,I3,f6.3,f6.3)  
	    if(u.lt.efix) then
	     cad=inf(i).c
	     write(cbuf,'(i1)')  inf(i).i(2)+1
	     cht=cbuf//toL(inf(i).i(1)/2)//cad
         write(2,6)  inf(i).i(2),inf(i).i(1)/2,cad,cht,u
6        format(I2,I2,A1,A3,f9.1)
	    end if
     end if
	end if
	sbuf(11+(j-1)*(20):10+j*20)=sbuft
   end do
    write(sbuft,'(F8.1)') u;	sbuf(3:10)=sbuft
	write(sbuft,'(I2)') j;	sbuf(1:2)=sbuft; 
	write(1,'(A810)',rec=ioutpos) sbuf
	ioutpos=ioutpos+1
 end if
end do
close(1)
deallocate(eval,evec,evect,stat=ierr)
deallocate(A,stat=ierr)
 rewind(2)
 INQUIRE (FILE = 'temp.txt', EXIST = IsThere)
 IF (IsThere) THEN; FileStatusTemp = 'REPLACE' ;ELSE; FileStatusTemp = 'REPLACE';END IF
 open(3,file='temp.txt',status=FileStatusTemp)
 allocate(chtnew(lda),cadnew(lda),in1(lda),in2(lda),en(lda))
 icount=0
 do	while (.NOT. EOF(2))
  read(2,6) im1,im2,cad,cht,u
   isign=0
   do i=1,lda
      	if(cht.eq.chtnew(i)) isign=1
   end do
   if (isign.ne.1) then
	icount=icount+1
    in1(icount)=im1;in2(icount)=im2;cadnew(icount)=cad;chtnew(icount)=cht;en(icount)=u
   end if
 end do
 write(3,'(I4)') icount
  do i=1,icount
     write(3,6) in1(i),in2(i),cadnew(i),chtnew(i),en(i)
  end do
close(2,status='delete');close(3)
deallocate(chtnew,cadnew,in1,in2,en)
idatagl=1

! ret=DELFILESQQ("C:\\"//"Spectra\\"//"Dump\\"//"*"//"."//"*")
 matls=1
end function matls



!************************final matrix manipulations*******************
integer function finmat()
 complex(8), allocatable :: A(:,:),evec(:,:)
 integer  nm, lda,ldevec,i,j,it1,it2,k
 real(8), allocatable :: eval(:),evect(:),evecti(:)
 real(8)  elow,u,w
 integer, allocatable :: jm(:)
 character*7 FileStatus 
 logical IsThere                                                               !character(91) tuda
 integer, dimension(1) :: imloc=(/0/)
 complex(8)  ic
 real xtem
 character*20 sbuft
 character*810 sbuf

type finfo
 integer lsj,jm
end type finfo
type(finfo), allocatable :: ffi(:)

data  nm,lda,ldevec,i,j,it1,it2,k/8*0/
data  elow,u,w/3*0.d00/
data ic/(0.,1.)/
                                                                 !write(*,*) irez,jrez
finmat=0;nm=irez;lda=irez;ldevec=irez;elow=0.
allocate(ffi(nm));ffi.lsj=0;ffi.jm=0

it1=0;it2=0
do i=1,jrez
 j=inf(i).i(3);it1=it1+1;it2=it2+j+1;allocate(jm(j+1)); call fromj(j,jm)
 do k=it1,it2;ffi(k).lsj=i; ffi(k).jm=jm(k-it1+1);end do
 deallocate(jm);it1=it2
end do
 !*****some diagnostics*******do i=1,irez;write(*,*) inf(ffi(i).lsj).i(1),inf(ffi(i).lsj).i(2),inf(ffi(i).lsj).i(3),ffi(i).jm; read(*,*);!end do
allocate(A(nm,nm));A=(0.0d00,0.0d00)
do i=1,nm
 do j=1,nm
   if(ffi(i)%jm.NE.ffi(j)%jm) then
   u=concr(ffi(i).lsj,ffi(j).lsj,ffi(i).jm,ffi(j).jm,0)
   w=concr(ffi(i).lsj,ffi(j).lsj,ffi(i).jm,ffi(j).jm,1)
   A(i,j)=u+ic*w
  else
   u=fu(ffi(i).lsj,ffi(j).lsj).els+fu(ffi(i).lsj,ffi(j).lsj).spin
   u=u+concr(ffi(i).lsj,ffi(j).lsj,ffi(i).jm,ffi(j).jm,0)
   A(i,j)=u
  end if
                                                     !  if(inf(ffi(i).lsj).i(1).eq.8.and.inf(ffi(j).lsj).i(1).eq.8) write(*,*) A(i,j); if(inf(ffi(i).lsj)%i(1).eq.10.and.inf(ffi(j).lsj)%i(1).eq.10) then;  !  write(*,*) inf(ffi(i).lsj)%i(1),inf(ffi(i).lsj)%i(3),inf(ffi(j).lsj)%i(1),inf(ffi(j).lsj)%i(3),a(i,j); read(*,*); ! end if 
 end do                                           ! write(*,'(A1)',advance='NO') CHAR(177)
end do
                                                    !write(*,*)  !write(*,*) 'matrix formation passed';open(unit=666,file='mat.txt')!tuda=''!do i=1,n! do j=1,n! z=A(i,j)!write(666, 10) inf(ffi(i).lsj).i(1),inf(ffi(i).lsj).i(2),inf(ffi(i).lsj).i(3),	inf(ffi(j).lsj).i(1),inf(ffi(j).lsj).i(2),inf(ffi(j).lsj).i(3),z!write(666, '(f9.1)') z! if(z.ne.0.) then ! tuda(j:j)='*'! else ; tuda(j:j)='0'! end if! end do!write(666,*) tuda!tuda=''!end do  !close(666)
!where(CDABS(A).lt.1.d-3) A=0.d00 
do i=1,nm
 do j=1,nm
	if(CDABS(A(i,j)).lt.1.d-4) A(i,j)=0.d00
 end do
end do
allocate(eval(nm),evec(nm,nm),evect(nm),evecti(nm));eval=0.d00
evec=0.d00;evect=0.d00;evecti=0.d00
call devchf(nm,A,lda,eval,evec,ldevec)
elow=MINVAL(eval)
do k=1,nm; eval(k)=eval(k)-elow; end do
where(DABS(eval).LT.1.d-3) eval=0.d00

INQUIRE (FILE = 'out.txt', EXIST = IsThere)
IF (IsThere) THEN            
 FileStatus = 'REPLACE'          ! erase and start again
ELSE
 FileStatus = 'NEW'                ! it isn't there, so create it
END IF
open(1,file='out.txt',status=FileStatus,access='direct',recl=810,form='formatted')
ioutpos=1
do k=1,nm
 imloc=MINLOC(eval); u=eval(imloc(1))
 where(DABS((eval-u)/(u+0.01)).LT.1.d-8)  eval=eval+1.d9
!****************OUTPUT eigenvalues***************************************
 if(u<1.d9) then
   sbuft='';sbuf=''
   j=0
   do i=1,nm 
    evect(i)=REAL(evec(i,imloc(1)))
    evecti(i)=AIMAG(evec(i,imloc(1)))
	xtem=Sqrt(evect(i)**2+evecti(i)**2)
	if(xtem.gt.wavefix) then
	 j=j+1
	 if(j.le.numfix) then
	  write(sbuft,5) inf(ffi(i).lsj).i(2)+1,inf(ffi(i).lsj)%i(1),&
       inf(ffi(i).lsj).i(3), ffi(i).jm,evect(i),evecti(i)
5     format(I1,I2,I2,I3,f6.3,f6.3)  
     end if
	end if
	sbuf(11+(j-1)*(20):10+j*20)=sbuft
   end do
    write(sbuft,'(F8.1)') u
	sbuf(3:10)=sbuft
	write(sbuft,'(I2)') j
	sbuf(1:2)=sbuft
    write(1,'(A810)',rec=ioutpos) sbuf
	ioutpos=ioutpos+1
 !imloc=MAXLOC(evect)
 !  write(1,10) u,inf(ffi(imloc).lsj).i(2)+1,inf(ffi(imloc).lsj)%i(1)/2,&
 ! inf(ffi(imloc).lsj).i(3),&
 ! ffi(imloc).jm,evect(imloc)
 ! 10 format(f7.1,3X,I2,I2,I2,I2,f6.3)

 end if
end do
close(1)

!************************END OF OUTPUT***********************************(
deallocate(eval,evec,evect,evecti,stat=ierr)
deallocate(A,stat=ierr)
deallocate(ffi,stat=ierr)
finmat=1
end function finmat




!************************final matrix(real) manipulations*******************
integer function finmatre()
 real(8), allocatable :: A(:,:),evec(:,:)
 integer  nm, lda,ldevec,i,j,it1,it2,k
 real(8), allocatable :: eval(:),evect(:), evecti(:)
 real(8)  elow,u,w
 integer, allocatable :: jm(:)
 character*7 FileStatus 
 logical IsThere                                                               !character(91) tuda
 integer, dimension(1) :: imloc=(/0/)
 complex(8)  ic
 real xtem
 character*20 sbuft
 character*810 sbuf

type finfo
 integer lsj,jm
end type finfo
type(finfo), allocatable :: ffi(:)

data  nm,lda,ldevec,i,j,it1,it2,k/8*0/
data  elow,u,w/3*0.d00/
data ic/(0.,1.)/
                                                                 !write(*,*) irez,jrez
finmatre=0;nm=irez;lda=irez;ldevec=irez;elow=0.
allocate(ffi(nm));ffi.lsj=0;ffi.jm=0

it1=0;it2=0;w=w;ic=ic
do i=1,jrez
 j=inf(i).i(3);it1=it1+1;it2=it2+j+1;allocate(jm(j+1)); call fromj(j,jm)
 do k=it1,it2;ffi(k).lsj=i; ffi(k).jm=jm(k-it1+1);end do
 deallocate(jm);it1=it2
end do
 !*****some diagnostics*******do i=1,irez;write(*,*) inf(ffi(i).lsj).i(1),inf(ffi(i).lsj).i(2),inf(ffi(i).lsj).i(3),ffi(i).jm; read(*,*);!end do
allocate(A(nm,nm));A=0.0d00
do i=1,nm
 do j=1,nm
   if(ffi(i)%jm.NE.ffi(j)%jm) then
   u=concr(ffi(i).lsj,ffi(j).lsj,ffi(i).jm,ffi(j).jm,0)
   
   A(i,j)=u
  else
   u=fu(ffi(i).lsj,ffi(j).lsj).els+fu(ffi(i).lsj,ffi(j).lsj).spin
   u=u+concr(ffi(i).lsj,ffi(j).lsj,ffi(i).jm,ffi(j).jm,0)
   A(i,j)=u
  end if
                                                     !  if(inf(ffi(i).lsj).i(1).eq.8.and.inf(ffi(j).lsj).i(1).eq.8) write(*,*) A(i,j); if(inf(ffi(i).lsj)%i(1).eq.10.and.inf(ffi(j).lsj)%i(1).eq.10) then;  !  write(*,*) inf(ffi(i).lsj)%i(1),inf(ffi(i).lsj)%i(3),inf(ffi(j).lsj)%i(1),inf(ffi(j).lsj)%i(3),a(i,j); read(*,*); ! end if 
 end do                                           ! write(*,'(A1)',advance='NO') CHAR(177)
end do
                                                    !write(*,*)  !write(*,*) 'matrix formation passed';open(unit=666,file='mat.txt')!tuda=''!do i=1,n! do j=1,n! z=A(i,j)!write(666, 10) inf(ffi(i).lsj).i(1),inf(ffi(i).lsj).i(2),inf(ffi(i).lsj).i(3),	inf(ffi(j).lsj).i(1),inf(ffi(j).lsj).i(2),inf(ffi(j).lsj).i(3),z!write(666, '(f9.1)') z! if(z.ne.0.) then ! tuda(j:j)='*'! else ; tuda(j:j)='0'! end if! end do!write(666,*) tuda!tuda=''!end do  !close(666)
!where(DABS(A).LT.1.d-03) A=0.d00 
do i=1,nm
 do j=1,nm
	if(DABS(A(i,j)).lt.1.d-4) A(i,j)=0.d00
 end do
end do


allocate(eval(nm),evec(nm,nm),evect(nm),evecti(nm));eval=0.d00
evec=0.d00;evect=0.d00
call DEVCSF (nm, A, LDA, EVAL, EVEC, LDEVEC)

elow=MINVAL(eval)
do k=1,nm; eval(k)=eval(k)-elow; end do
where(DABS(eval).LT.1.d-3) eval=0.d00

INQUIRE (FILE = 'out.txt', EXIST = IsThere)
IF (IsThere) THEN            
 FileStatus = 'REPLACE'          ! erase and start again
ELSE
 FileStatus =  'NEW'                ! it isn't there, so create it
END IF
open(1,file='out.txt',status=FileStatus,access='direct',recl=810,form='formatted')
ioutpos=1
do k=1,nm
 imloc=MINLOC(eval); u=eval(imloc(1))
 where(DABS((eval-u)/(u+0.01)).LT.1.d-8)  eval=eval+1.d9
!****************OUTPUT eigenvalues***************************************
 if(u<1.d9) then
   sbuft='';sbuf=''
   j=0
   do i=1,nm 
    evect(i)=REAL(evec(i,imloc(1)))
    evecti(i)=0
	xtem=Sqrt(evect(i)**2+evecti(i)**2)
	if(xtem.gt.wavefix) then
	 j=j+1
	 if(j.le.numfix) then
	  write(sbuft,5) inf(ffi(i).lsj).i(2)+1,inf(ffi(i).lsj)%i(1),&
       inf(ffi(i).lsj).i(3), ffi(i).jm,evect(i),evecti(i)
5     format(I1,I2,I2,I3,f6.3,f6.3)  
     end if
	end if
	sbuf(11+(j-1)*(20):10+j*20)=sbuft
   end do
    write(sbuft,'(F8.1)') u
	sbuf(3:10)=sbuft
	write(sbuft,'(I2)') j
	sbuf(1:2)=sbuft
    write(1,'(A810)',rec=ioutpos) sbuf
	ioutpos=ioutpos+1

 !imloc=MAXLOC(evect)
 !  write(1,10) u,inf(ffi(imloc).lsj).i(2)+1,inf(ffi(imloc).lsj)%i(1)/2,&
 ! inf(ffi(imloc).lsj).i(3),&
 ! ffi(imloc).jm,evect(imloc)
 ! 10 format(f7.1,3X,I2,I2,I2,I2,f6.3)

 end if
end do
close(1)

!************************END OF OUTPUT***********************************(
deallocate(eval,evec,evect,evecti,stat=ierr)
deallocate(A,stat=ierr)
deallocate(ffi,stat=ierr)
finmatre=1
end function finmatre


!******subroutine to make a vector of 2JM's from given 2J********
!*********size of jm is j+1 - should be allocated, j=2J**********
subroutine fromj(j,jm)
 integer :: i=0,j; integer, dimension(:) :: jm; real :: jr=0.
 jr=j/2.; do i=1,j+1; jm(i)=(-jr+i-1)*2; end do;return
end subroutine fromj

!real function conel(a)
!real a(3)
!conel=a(1)*(toe1(1)*fp2+toe1(2)*fp4+toe1(3)*fp6)+&
!      a(2)*(toe2(1)*fp2+toe2(2)*fp4+toe2(3)*fp6)+&
!      a(3)*(toe3(1)*fp2+toe3(2)*fp4+toe3(3)*fp6)
!return
!end function conel

!******Crystal field matrix elements calculation********************
real function concr(i,i1,m,m1,iflag)
!iflag =0 => real part of matrix; flag=1 => Im part of matrix
!************************************************************
integer i,i1,m,m1,iflag;integer :: j,j1,it,k,itt,item=0
real :: x,xtem=0.;real, dimension(3) :: u,w=0.;logical, dimension(3) :: lt1,lt2
integer, dimension(3) :: ifmask

lt1=.False.; lt2=.False.
ifmask=0
concr=0.;xtem=(m-m1)/2.;
if(xtem-INT(xtem).NE.0.) return

do im=1,3
 if(fu(i,i1)%cr(im).ne.0) ifmask(im)=1
end do
if(ifmask(1)+ifmask(2)+ifmask(3).eq.0) return

it=INT(xtem);j=inf(i).i(3);j1=inf(i1).i(3)
if(ABS(it).gt.6) return
do k=1,3; itt=ABS(it)+1; u(k)=REAL(Bpk(k,itt)); w(k)=AIMAG(Bpk(k,itt));end do
where(ABS(u).LT.0.5) lt1=.True.;where(ABS(w).LT.0.5) lt2=.True.
if(ALL(lt1).AND.ALL(lt2)) return
select case(iflag)
 case(0)
  if(it.ge.0) then
   x=0.
   do k=1,3
     xtem=fu(i,i1).cr(k)*u(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	 item=j-m+2*it
	 if(MOD(item/2,2).ne.0) xtem=-xtem; x=x+xtem
   end do
   concr=x;return
  else
   x=0.
   do k=1,3
      xtem=fu(i,i1).cr(k)*u(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	   item=j-m
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concr=x;return
  end if
 case(1)
  if(it.ge.0) then
   x=0.
   do k=1,3
      xtem=fu(i,i1).cr(k)*w(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
      item=j-m+2*it
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concr=x
  else
   x=0.
   do k=1,3
	 xtem=fu(i,i1).cr(k)*w(k)*threej(j,4*k,j1,-m,2*it,m1)*ff(k)
	item=j-m+2
	if(MOD(item/2,2).ne.0) xtem=-xtem;x=x+xtem
   end do
   concr=x;return
  end if
end select
end function concr

logical function FreeMem()
 integer ierr
 FreeMem=.False.
 irez=0;jrez=0;
! fp2=0.; fp4=0.;  fp6=0.;dze=0.;Bpk=0.
 deallocate(inf,fu,stat=ierr)	   !rcrb,respinb,relb
 if(ierr.eq.0) Freemem=.True.
end function FreeMem
end module matrix