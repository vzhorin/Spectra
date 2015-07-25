module genetic



contains

!********************************************************************
!          Genetic algorithm (differential evolution) for optimization
!********************************************************************
!genmax - maximum number of generations; 
!NP - number of population vectors (10*ND rec);  
!ND  - dimension of population vector(number of parameters to find); 
!CR<=1.  - crossover constant (0.1 recommended, but larger the better);
!noise - scaling factor: Xcnew=Xcold+noise(Xa-Xb) (noise=0.5 rec);
!x1,x2 - old and new generations, cost(NP) - cost values for a population vectors;
!score - cost function value for trial population vector "trial";
!costeval - function to give cost value
!******************created 01/17/98***********************************
!******************modified 05/11/99*************************************
!****Photochemistry and Photophysics Group, ANL**************************
integer*4 function genev(nd,np,cr,noise,genmax,rr)
use MSIMSL		   !random number generation

interface 
real function  costev(nd,par)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_costev@8' :: costev
 integer nd
 real,dimension(nd) :: par
end function costev

end interface


integer nd,np,genmax
real cr,noise,score 
real,dimension(nd,2) :: rr

real, dimension(nd) :: trial
real, dimension(np) :: cost
real, dimension(np,nd) :: x1,x2

integer nc,i,j,k,ia,ib,ic
integer iml(1)

open(33,file="dataout.txt")
!write(33,*) "np=",np,"nd=",nd,"genmax=",genmax
genev=0
call parange(nd,np,x1,rr)
do i=1,np
 do j=1,nd
  trial(j)=x1(i,j)
 end do
 cost(i)=costev(nd,trial)
end do

write(33,'(f15.3)') cost

do nc=1,genmax
  do i=1,np
   
   ia=(np-1)*RNUNF()+1.1;do while (ia.eq.i); ia=(np-1)*RNUNF()+1.1; end do
   ib=(np-1)*RNUNF()+1.1;do while (ib.eq.i.or.ib.eq.ia); ib=(np-1)*RNUNF()+1.1; end do
   ic=(np-1)*RNUNF()+1.1;do while (ic.eq.i.or.ic.eq.ia.or.ic.eq.ib); ic=(np-1)*RNUNF()+1.1; end do
 !  write(*,*) 'i=',i,' ia=',ia,' ib=',ib,' ic=',ic
   j=(nd-1)*RNUNF()+1.1
   do k=1,nd
	 if(RAND(1).lt.cr.or.k.eq.nd) then
	  trial(j)=x1(ic,j)+noise*(x1(ia,j)-x1(ib,j))
	  if(trial(j).gt.rr(j,2).or.trial(j).lt.rr(j,1)) trial(j)=x1(ic,j)
	 else
	  trial(j)=x1(i,j)
	 end if
	j=MODULO(j+1,nd)
   end do
   score=costev(nd,trial)
   if(score.le.cost(i)) then
    cost(i)=score;do k=1,nd;x2(i,k)=trial(k);end do
   else
	do k=1,nd; x2(i,k)=x1(i,k); end do
   end if
  end do
  x1=x2
 write(33,'(A20,i5,a15)') 'generation number ',nc,' okuklilos'
end do
iml=MINLOC(cost)
do j=1,nd
 write(33,*) 'parameter ',j,'=',x1(iml,j)
end do
 write(33,*) 'cost value=',cost(iml)
do j=1,nd
 trial(j)=x1(iml(1),j)
end do
score=costev(nd,trial)
 !write(33,*) 'cost value=',score

close(33)
genev=1;return
end function genev



subroutine parange(nd,np,x,r)
USE MSIMSL		  !random number generation
real,dimension(np,nd) :: x
real,dimension(nd,2) :: r
integer i,j,nd,np

do i=1,np
 do j=1,nd
  x(i,j)=RNUNF()*(r(j,2)-r(j,1))+r(j,1)
 end do
end do
end subroutine parange 


 

end module genetic