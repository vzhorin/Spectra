module ewald
  use apk

  CHARACTER(4),allocatable:: AT(:)
  real*8 :: H1(3),H2(3),H3(3),T1(3),T2(3),T3(3),B(3,3),YAT(3),XX(3)
  real*8, allocatable :: DECAT(:,:),QF(:,:), Q(:,:,:),RR(:,:),XAT(:,:,:),chrat(:)
  integer, allocatable ::  INDAT(:,:)
  real vol


contains

integer function makelatprim()
 integer ksub,ksum,kcell,ktr, nrel
 real*8 C

! i=Initoflat()
 !if(i.ne.0) then
  open(1,file='input.txt')
  open(2,file='out.txt')
    read(1,'(24x,i3)') ksub !; write(2,'(24x,i3)') ksub

  	allocate (AT(ksub),DECAT(ksub,3),chrat(ksub)) 
	read(1,*); read(1,'(3f9.5)') T1,T2,T3 !;	write(2,*); write(2,'(3f9.5)') T1,T2,T3
    read(1,*); read(1,'(3f9.6)') H1,H2,H3; read(1,'(15X,F6.1)') VOL !;	write(2,*); write(2,'(3f9.6)') H1,H2,H3; write(2,'(15X,F6.1)') VOL
 	read(1,'(A4,x,3F10.6,f5.2)') (AT(I),(DECAT(I,J),J=1,3),chrat(I),I=1,KSUB)
	close(1)
	write(2,'(A4,x,3F10.6,f5.2)') (AT(I),(DECAT(I,J),J=1,3),chrat(I),I=1,KSUB) 
  	write(2,'(i5)')  ksub
  	nrel=1;	ktr=15
	kcell=ktr**3; ksum=kcell*ksub
	write(2,'(i10)') kcell
	allocate(Q(KSUB,3,3),RR(KSUB,3),XAT(KSUB,KCELL,3),INDAT(KSUM,2),stat=ierr)
	write(2,'(i5)') ierr
	 INDAT=0;Q=0.
	 C=DSQRT(PI)/VOL**(0.3333333D00)              
     do  I=1,KSUB; do  L=1,3; RR(I,L)=DECAT(NREL,L)-DECAT(I,L); end do;end do
     do  I=1,3
      RR(NREL,I)=0.D00
      Q(NREL,I,I)=4.*C*C*C/3./DSQRT(PI)
	 end do
     ktt=(ktr+1)/2
	 write(2,'(i5)') ktt
      do 100 I=1,KSUB;    IS=0
       do 100 I1=1,ktr;     II1=I1-ktt
         do 100 I2=1,ktr;   II2=I2-ktt
          do 100 I3=1,ktr;  IS=IS+1;     II3=I3-ktt   
           do 100 L=1,3
             YAT(L)=T1(L)*II1+T2(L)*II2+T3(L)*II3             
             XAT(I,IS,L)=DECAT(NREL,L)-DECAT(I,L)-YAT(L) 
 100  continue
 !     CALL SORT(KSUB,KCELL,XAT,INDAT,IS) 
      WRITE(2,150) (AT(INDAT(I,1))(1:3),(XAT(INDAT(I,1),INDAT(I,2),L),L=1,3),&
                            chrat(INDAT(I,1)),I=2,IS)
 150  FORMAT(A3,2X,3F10.6,f5.2)   

close(2)
! end if
 makelatprim=1
end function makelatprim




	 SUBROUTINE SORT(KSUB,KCELL,X,IND,IS)
      IMPLICIT REAL*8(A-H,O-Z)
	  DIMENSION X(KSUB,KCELL,3)
	  real*8, allocatable :: XN(:,:)
      DIMENSION IND(KSUB*KCELL,2)
      ALLOCATE (XN(KSUB,KCELL))
	  xn=0.
      do I=1,KSUB
       do J=1,KCELL
         do  L=1,3;    XN(I,J)=XN(I,J)+X(I,J,L)*X(I,J,L); end do
		 XN(I,J)=DSQRT(XN(I,J))
	   end do
	  end do
      AM=1.d30                                 
      RMAX=18.d00;rrr=0.d00        
      IS=0
      DO WHILE(RRR.LT.RMAX)
       do I=1,KSUB
	      do J=1,KCELL
		    AM=DMIN1(AM,XN(I,J))
		  end do;
	   end do
       do I=1,KSUB
        do J=1,KCELL
         if(XN(I,J).EQ.AM) then
          IS=IS+1; IND(IS,1)=I;IND(IS,2)=J
          RRR=XN(I,J); XN(I,J)=1.; AM=1.d140   
         else
         endif
 		end do
	   end do
      END DO      
      return
      end  subroutine sort     

      real*8 FUNCTION g(X)
       IMPLICIT REAL*8  (A-H,O-Z)
	    G=0.; IF(X.LE.100.) g=DEXP(-X)/X
	   RETURN
      END  function g
      
      subroutine SAB(P,XMQ,X,A)
       IMPLICIT REAL*8  (A-H,O-Z)
       DIMENSION X(3),A(3,3)
       PQ=P*P;E=G(PQ*XMQ)
       do I=1,3
          do J=1,3
       A(I,J)=PQ*X(I)*X(J)*E; A(J,I)=A(I,J)
		  end do
 	   end do
       RETURN
      end subroutine sab
      
     subroutine HAB(R,XM,X,A)
	  IMPLICIT REAL*8  (A-H,O-Z)
      DIMENSION X(3),A(3,3)
      DATA ZERO,ONE,TWO,THREE/0.0D0,0.1D1,0.2D1,0.3D1/
      SP=TWO/DSQRT(0.3141592654D1)
      RQ=R*R; XR=XM*R; XRQ=XR*XR
      E=G(XRQ);  EDE=ERFC(XR); T=E+EDE/(SP*XR*XRQ)   
      do  I=1,3
        do J=1,3
         DEL=ZERO; IF(J.EQ.I) DEL=ONE; GA=X(I)*X(J)*RQ; A(I,J)=SP*(TWO*GA*E+(THREE*GA/XRQ-DEL)*T); A(J,I)=A(I,J)
  		end do
	  end do
      return
      end subroutine hab

     real*8  FUNCTION ERFC(a)   
!**********does not work right for -1<a<0**********************      
     real*8 :: P(9),Q(9),T(5),U(6)
	 real*8 x,a,v,tp,up,z,ze,pp,qp,erf

        DATA P/2.461969814735D-10,5.641895648310D-1,7.463210564422D0, &
          4.863719709856D1,1.965208329560D2,5.264451949954D2, &
          9.345285271719D2,1.027551886895D3,5.575353353693D2/
        DATA Q/1.000000000000D0,1.322819511547D1,	&
         8.670721408859D1,3.549377788878D2,9.757085017432D2,   &
         1.823909166879D3,2.246337608187D3,1.656663091941D3, &
         5.575353408177D2/ 
        DATA T/9.604973739870D0,9.002601972038D1, &
          2.232005345946D3,7.003325141128D3,5.559230130103D4/
        DATA U/1.000000000D0,3.356171416475D1,5.213579497801D2, &
          4.594323829709D3,2.262900006138D4,4.926739426086D4/
	
!********************************************************
! For small x, erfc(x) = 1 - erf(x); otherwise rational
! approximations  erfc(x) =exp(-z**2)*P/Q are computed.
!  erf(x)=t(x**2)/u(x**2)*x
!********************************************************
      if( a .LT. 0.0 ) then; x = -a; else; x = a; endif
      if( x .LT. 1. ) then
       v=a*a
       tp=T(1)*v**4+T(2)*v**3+T(3)*v**2+T(4)*v+T(5)
       up=U(1)*v**5+U(2)*v**4+U(3)*v**3+U(4)*v**2+U(5)*v+U(6) 
       ERF=tp/up*a;  ERFC=1.0-ERF
      elseif(x.LT.8.0 ) then
        z = -a*a; ze=0.
        IF(DABS(z).LE.150.) ze=DEXP(z)
        pp=P(1)*x**8+P(2)*x**7+P(3)*x**6 +P(4)*x**5+P(5)*x**4+P(6)*x**3+P(7)*x**2+P(8)*x+P(9)
        qp=Q(1)*x**8+Q(2)*x**7+Q(3)*x**6+Q(4)*x**5+Q(5)*x**4+Q(6)*x**3+Q(7)*x**2+Q(8)*x+Q(9)
        ERFC=(ze*pp)/qp
      else
        ERFC=0.
      endif
       IF( a .LT. 0 ) ERFC = 2.0 - ERFC
       RETURN
      END function erfc

end module ewald