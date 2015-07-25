module zeeman
use msfwina
use dataman

type quanum
 integer L			  !quantum number 2L
 integer S			  !quantum number 2S
 integer J			   !quantum number 2J
 integer M			  !quantum number 2JM
end type quanum

type gval
 integer L  ! quantum numbers for LSJJM vector having
 integer S  !    largest contribution into 
 integer J	 !			composition of 
 integer M   !			 a given Stark level with
 real percent
 real sten		    !               energy sten and
 real gfacz			!                g-factors correponding to the z axes and
 real gfacxy	    !                  perpendicular plane
end type gval

 contains
 
 integer*4 function gfactor(iRecords,bufar)
  integer*4 iRecords
  character*810 :: bufar(iRecords)
  character*810 buf
  character*20, allocatable :: vect(:)
  real, allocatable :: vre(:),vim(:)
  type(quanum), allocatable :: 	quazee(:)
  type(gval), allocatable :: g(:)
  character*20 buft
  character*17 szbuf
  character*1 vID
  integer inrec,ivect,i,j
  real en,vmax,vnorm
  real*8 gz,gztemp

   allocate(g(iRecords))
   open(11,file="vect.txt")	  
!******main loop, for each energy levels analyze wavefunctions and compute g-factor
   do inrec=1,iRecords
    write(11,'(i4)') inrec
    buf=bufar(inrec);buft=buf(3:10)
	!write(11,'(a30)') buft
	read(buft,'(f10.1)') en
	g(inrec).sten=en
!	write(11,*) en,g(inrec).sten

	buft=buf(1:2)
	read(buft,'(i2)') ivect

	allocate(vect(ivect),vre(ivect),vim(ivect),quazee(ivect))
	 do i=1,ivect;vect(i)=buf(11+(i-1)*(20):10+i*20); end do
	do i=1,ivect
	 buft=vect(i); szbuf=buft(2:3)
	 read(szbuf,*) j
	 vID=CharUpCase(toL(j/2))
	 szbuf=buft(1:1)//vID//' '//buft(4:5)//char(47)//'2 '//buft(6:8)//'(2Jm)'
	 write(11,'(a30)') szbuf
	 szbuf=buft(9:14)
	 read(szbuf,'(f6.3)') vre(i)
 !****real part************
	 write(11,'(f6.3)') vre(i)
	 szbuf=buft(15:20)		  
 	 read(szbuf,'(f6.3)') vim(i)
  !****imaginary part*******
	 write(11,'(f6.3)') vim(i)
	 quazee(i).L=j
	 read(buft(1:1),'(i1)')   quazee(i).S
	 quazee(i).S=quazee(i).S-1
	 read(buft(4:5),'(i2)')   quazee(i).J
	 read(buft(6:8),'(i3)')   quazee(i).M 
	! write(11,*) "checking quazee type"
	! write(11,'(4i5)') quazee(i).L, quazee(i).S, quazee(i).J,quazee(i).M
 	end do
	 vmax=0.;imax=0;vnorm=0.
	 do i=1,ivect
	  vnorm=vre(i)**2+vim(i)**2
	  if(vnorm.gt.vmax) then
		vmax=vnorm;imax=i
	  end if
	 end do
	 g(inrec).L=quazee(imax).L
	 g(inrec).S=quazee(imax).S
	 g(inrec).J=quazee(imax).J
	 g(inrec).M=quazee(imax).M
	 g(inrec).percent=vmax*100.
	 write(11,*)	"-------------------------------------------------------------------------------"
	 write(11,*) "The nominal value of LSJJm is"
	 write(11,'(4i5)')  g(inrec).L, g(inrec).S, g(inrec).J,g(inrec).M
	 write(11,*) "taken from state, which contribute ", g(inrec).percent,"% to eigenvector"
 	 write(11,*) 
	 gz=0.d00
	 do i=1,ivect
	  gztemp=1.d00+& 
	  (&
	    (0.5*quazee(i).J+1.)*0.5*quazee(i).J+&
	    (0.5*quazee(i).S+1.)*0.5*quazee(i).S-&
	    (0.5*quazee(i).L+1.)*0.5*quazee(i).L&
	   )/&
	   ((0.5*quazee(i).J+1.)*quazee(i).J)
	  gz=gz+(gztemp*quazee(i).M)*(vre(i)**2+vim(i)**2)
	 end do
	  g(inrec).gfacz=gz
	 write(11,*) "G-factor equals", g(inrec).gfacz
 	 write(11,*)	"-------------------------------------------------------------------------------"
  	 write(11,*) 
 	 write(11,*) 

	deallocate(vect,vre,vim,quazee)
   end do

!***************end of main loop**********************************
   close(11)
   deallocate(g)
   gfactor=1
 end function gfactor

character*1 function CharUpCase(cht)
character*1 cht 
SELECT CASE (cht)
    CASE ( "a":"z" )
        cht = CHAR( ICHAR(cht) + ICHAR("A") - ICHAR("a") )
END SELECT
CharUpCase=cht;return
end function CharUpCase

end module zeeman