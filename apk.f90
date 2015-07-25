module apk
use msfwina

 !real*8,dimension(5) :: o2,op2
 !real*8,dimension(9) :: o4,op4
 !real*8,dimension(13) :: o6,op6

 integer, save :: numat  !number of atoms in RE environment
character*40, save :: envname !name of the ion and ligands to study (optional, can be blank)
character*3, save :: idRE,idLig		  !	identification for rare-eaarth ion and ligand two symbols -chemical, third - valency
real, allocatable, save :: xLig(:,:),qLig(:)  !cart.coordinates of ligands in ang., charge (can be fractional)
real, save :: SS(2,3)
real, save :: rlimits(2)
real, save :: Gs


 real, dimension(5) :: a2k=(/0.5, 1.2247449,-1.2247449,0.61237244,0.61237244/)
 real, dimension(9) :: a4k=(/0.125,0.55901699,-0.55901699,0.39528471,0.39528471,1.4790199,-1.4790199,0.52291252, 0.52291252/)
 real, dimension(13) :: a6k=(/0.0625,0.40504629,-0.40504629, 0.32021721,0.32021721, 0.64043442,-0.64043442,	0.35078038,0.35078038, 1.6453058,-1.6453058, 0.47495888,0.47495888/)

!real*8 coefS, coefA

real :: rmed2=10/7.,rmed4=18/7.,rmed6=26/7.
real :: CON=1.16e05
real*8 :: PI=0.3141592654D1
contains
  
      FUNCTION OVERSS(R,cs,as)
       IMPLICIT REAL*8(A-H,O-Z)
	   real cs,as
    	 OVERSS=cs*DEXP(-as/0.5297*R)
       RETURN
	  end function OVERSS


	 SUBROUTINE POL2(A,B,C,O2)
	   IMPLICIT REAL*8(A-H,O-Z)
	   DIMENSION O2(5)
	   O2(1)=3.*C*C-1.
       O2(2)=A*C; O2(3)=B*C
	   O2(4)=(A*A-B*B);  O2(5)=2.*A*B
       return
      END subroutine pol2
                   
      SUBROUTINE POL4(A,B,C,O4)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION O4(9)
       O4(1)=35.*C*C*C*C-30.*C*C+3.D00
       O4(2)=A*C*(7.*C*C-3.D00);O4(3)=B*C*(7.*C*C-3.D00)
       O4(4)=(A*A-B*B)*(7.*C*C-1.D00);O4(5)=2.*A*B*(7.*C*C-1.D00)
       O4(6)=A*C*(A*A-3.*B*B); O4(7)=B*C*(3.*A*A-B*B)
       O4(8)=A*A*A*A-6.*A*A*B*B+B*B*B*B; O4(9)=4.*A*B*(A*A-B*B)
	   return
      END subroutine pol4

      SUBROUTINE POL6(A,B,C,O6)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION O6(13)
       O6(1)=231.*c*c*c*c*c*c-315.*c*c*c*c+105.*c*c-5.
       O6(2)=A*C*(33.*c*c*c*c-30.*c*c+5.)
       O6(3)=B*C*(33.*c*c*c*c-30.*C*C+5.)
       O6(4)=(A*A-B*B)*(33.*c*c*c*c-18.*c*c+1.)  
       O6(5)=2.*A*B*(33.*c*c*c*c-18.*c*c+1.) 
       O6(6)=A*C*(A*A-3.*B*B)*(11.*c*c-3.)
       O6(7)=B*C*(3.*A*A-B*B)*(11.*c*c-3.)
       O6(8)=(A*A*A*A-6.*A*A*B*B+B*B*B*B)*(11.*c*c-1.)
       O6(9)=4.*A*B*(A*A-B*B)*(11.*c*c-1.)
       o6(10)=a*c*(a*a*a*a-10.*a*a*b*b+5.*b*b*b*b)
	   o6(11)=b*c*(5.*a*a*a*a-10.*a*a*b*b+b*b*b*b)
	   o6(12)=(a*a-b*b)*(a*a*a*a-14.*a*a*b*b+b*b*b*b)
	   o6(13)=2.*a*b*(3.*a*a*a*a-10.*a*a*b*b+3.*b*b*b*b)
	   return
      END subroutine pol6

   integer function parcou()

   IMPLICIT REAL*8(A-H,O-Z)
    allocatable X(:,:),chr(:)
    dimension o2(5),op2(5)
	dimension o4(9),op4(9)
	dimension o6(13),op6(13)
    CHARACTER*4,allocatable :: AT(:) 
    data o2/5*0.D00/,op2/5*0.D00/  
	data o4/9*0.d00/,op4/9*0.d00/
	data o6/13*0.d00/,op6/13*0.d00/
          
!**********************************************************
!**********sh - Sternheimer constant***********************
	sh2=1.
	sh4=1.
    sh6=1.
!**********************************************************
      OPEN(3,FILE='out.LAT')
      OPEN(2,FILE='cubic.lat',MODE='READ') 
      READ(2,10) NTOT 
 10   FORMAT(I6)               
	WRITE(3,10) NTOT 
      ALLOCATE (AT(NTOT),X(NTOT,3),chr(NTOT))
      READ(2,20) (AT(I),(X(I,L),L=1,3),chr(I),I=1,NTOT)
 20   FORMAT(A3,2X,3F12.8,f5.2)                         
      
      WRITE(3,20) (AT(I),(X(I,L),L=1,3),chr(I),I=1,NTOT)
      WRITE(*,*) 'Beginning summation'
      do I=1,NTOT
		RM=0.D00
		do L=1,3; RM=X(I,L)*X(I,L)+RM; end do
		RM=DSQRT(RM)
	    A=X(I,1)/RM;  B=X(I,2)/RM; C=X(I,3)/RM 
		call pol2(a,b,c,o2)
		do K=1,5;  OP2(K)=OP2(K)+O2(K)*qLig(i)*a2k(K)/(RM*RM*RM);  end do
		call pol4(a,b,c,o4)
		do K=1,9;  OP4(K)=OP4(K)+O4(K)*qLig(i)*a4k(K)/(RM*RM*RM*RM*RM);  end do
		if(rm.lt.7.) then
			call pol6(a,b,c,o6)
			do K=1,13
			  OP6(K)=OP6(K)+O6(K)*qLig(i)*a6k(K)/(RM*RM*RM*RM*RM*RM*RM)
		    end do    
		else
		end if
      end do  
      write(3,56)
56	format('Second-order parameters')  
      write(3,60) (-OP2(I)*CON*RMED2*sh2,I=1,5)
	  write(3,57)
57  format('Fourth-order parameters')
      write(3,60) (-OP4(I)*CON*RMED4*sh4,I=1,9)
	  write(3,58)
58  format('Sixth-order parameters')
      write(3,60) (-OP6(I)*CON*RMED6*sh6,I=1,13)
 60 format(4F15.2)  
      
	  parcou=1
      end function parcou

	integer function parex()

    IMPLICIT REAL*8(A-H,O-Z)
 	dimension o2(5),op2(5)
	dimension o4(9),op4(9)
	dimension o6(13),op6(13)
    real*8 tempSS1,tempSS2,tempSS3

	integer i,k,l

 !   data o2/5*0.D00/,op2/5*0.D00/  
!	data o4/9*0.d00/,op4/9*0.d00/
!	data o6/13*0.d00/,op6/13*0.d00/
	 
!parameters for f-electrons
!    rmed2=10/7.
!	rmed4=18/7.
!	rmed6=26/7.
!parameters for d-electrons
!   rmed2=10/5.
!	rmed4=18/5.
!	rmed6=26/5.
!***2 RANK RMED=2 ************************************************      
 	  o2=0.;o4=0.;o6=0.;op2=0.;op4=0.;op6=0.
      OPEN(3,FILE='out.lat')
!	WRITE(*,*) 'ENTER Gs'
!	  Gs=8.
      write(3,22) Gs
 22 format(F4.1)  
      do  I=1,numat
	    RM=0.D00
		do L=1,3;    RM=xLig(I,L)*xLig(I,L)+RM;  end do
		RM=DSQRT(RM)
		if(rm.gt.rlimits(1).and.rm.lt.rlimits(2)) then
		 continue
		else
    	  ret=MessageBox(NULL,"Distance to ligands is outside recommended range!\nResults of CF calcualtions are unreliable"C,"Bad Distance"C,MB_ICONSTOP)
		end if
		A=xLig(I,1)/RM;  B=xLig(I,2)/RM; C=xLig(I,3)/RM   
		tempSS1=OVERSS(RM,SS(1,1),SS(2,1))**2
		tempSS2=OVERSS(RM,SS(1,2),SS(2,2))**2
		tempSS3=OVERSS(RM,SS(1,3),SS(2,3))**2
		OVER2=Gs*(tempSS1+	tempSS2+3./2*tempSS3)
		OVER4=Gs*(tempSS1+	tempSS2+1/3.*tempSS3)
		OVER6=Gs*(tempSS1+	tempSS2-3./2.*tempSS3)
		call pol2(a,b,c,o2)
		do K=1,5;  OP2(K)=OP2(K)+O2(K)*OVER2*a2k(K)/RM;  end do
		call pol4(a,b,c,o4)
		do K=1,9;  OP4(K)=OP4(K)+O4(K)*OVER4*a4k(K)/RM;  end do
		call pol6(a,b,c,o6)
		do K=1,13;  OP6(K)=OP6(K)+O6(K)*OVER6*a6k(K)/RM; end do    
 	  end do
 57   format('Second-order parameters') 
      WRITE(3,60) (OP2(I)*CON*rmed2,I=1,5)
	  write(3,58) 
 58   format('Fourth-order parameters') 
      WRITE(3,60) (OP4(I)*CON*rmed4,I=1,9)
	  write(3,59) 
 59   format('Sixth-order parameters') 
      WRITE(3,60) (OP6(I)*CON*rmed6,I=1,13)
 60   FORMAT(4F15.5)  
	  close(3)  
	  parex=1
      end function parex
            
end module apk
