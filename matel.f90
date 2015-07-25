!functions for interaction matrix elements calculation
MODULE matel


 contains
 real*8 function V11(S,L,J,MJ,S1,L1,J1,MJ1,R)


    integer s,l,j,mj,s1,l1,j1,mj1
    real r 
    if(J.NE.J1.OR.MJ.NE.MJ1) then
     V11=0.;return
    end if
    V11=SQRT(84.)*sixj(2*s,2*s1,2,2*l1,2*l,2*j)*r*(-1.)**(j+l+s1)
  end function V11

  real*8 function ur(S,L,J,MJ,S1,L1,J1,MJ1,p,k,R)

    integer s,l,j,mj,s1,l1,j1,mj1,p,k
 	real r 
	if(s.NE.s1) then
    ur=0.;return
	end if
	ur=(-1)**(s+l1+p-mj)*threej(2*j,2*p,2*j1,-2*mj,2*k,2*mj1)*&
	&sixj(2*j,2*j1,2*p,2*l1,2*l,2*s)*SQRT(2.*j+1.)*SQRT(2.*j1+1.)*r
  end function ur

  real*8 FUNCTION threej(J1,J2,J3,M1,M2,M3)          

 	integer j1,j2,j3,m1,m2,m3
    real*8 f1,f2,f3,f4,f5,f6,fm,s,u,del
	integer a,l,m,p   
	u=0.
    del= DELTA3(J1,J2,J3)                                         
      IF (M1+M2+M3.NE.0.OR.del.EQ.0)  then
	  threej=0.;return
	  end if
      p=1-MOD(ABS(J1-J2-M3),4)                                        
      f1=ftr(J1+M1);f2=ftr(J1-M1);f3=ftr(J2+M2);f4=ftr(J2-M2);f5=ftr(J3+M3);f6=ftr(J3-M3) 
	  if(f1.LT.0.OR.f2.LT.0.OR.f3.LT.0.OR.f4.LT.0.OR.f5.LT.0.OR.f6.LT.0) then
	  s=0.
	  else                                                   
      S=f1*f2*f3*f4*f5*f6
	  end if                                             
      L=MIN(J1+J2-J3,J1-M1,J2+M2);M=MAX(0,J2-J3-M1,J1-J3+M2)                                      
      IF (L.LT.M) then
	   threej=0.;return
	  end if
      DO K=M,L,2                                                     
      a=1-MOD(K,4)                                                   
      f1=ftr(K);f2=ftr(J1+J2-J3-K);f3=ftr(J3-J2+M1+K);f4=ftr(J1-M1-K);f5=ftr(J3-J1-M2+K);f6=ftr(J2+M2-K)                                                     
	  if(f1.LT.0.OR.f2.LT.0.OR.f3.LT.0.OR.f4.LT.0.OR.f5.LT.0.OR.f6.LT.0) then
	   u=0.;exit
	  end if
      fm=f1*f2*f3*f4*f5*f6;u=u+a/fm                                                         
      end do 
      threej=p*del*DSQRT(s)*U                                                
   END function threej

   real*8 function ClG(j1,j2,j3,m1,m2,m3)

     integer j1,j2,j3,m1,m2,m3
	  ClG=ThreeJ(j1,j2,j3,m1,m2,-m3)*SQRT(j3+1.)*(-1.)**((m3+j1-j2)/2)
   end function ClG

   real*8 FUNCTION SIXJ(J1,J2,J3,L1,L2,L3) 

      integer j1,j2,j3,l1,l2,l3
	  integer p,a,l,m  
	  real*8 d,d1,d2,d3,d4,f0,u, f1,f2,f3,f4,f5,f6,f7
	  u=0.
      d1=DELTA3(J1,J2,J3);d2=DELTA3(J1,L2,L3);d3=DELTA3(L1,J2,L3);d4=DELTA3(L1,L2,J3)                                        
      IF(D1.LE.0..OR.D2.LE.0..OR.D3.LE.0..OR.D4.LE.0.) then
	   sixj=0.;return
	  end if
      D=D1*D2*D3*D4;P=1-MOD(J1+J2+L1+L2,4);U=0.                                                             
      L=MIN(J1+J2-J3,L1+L2-J3,J1+L2-L3,L1+J2-L3)                       
      M=MAX(0,J1+L1-J3-L3,J2+L2-J3-L3)                                  
      IF(L.LT.M) then 
	   sixj=0.;return
	  end if
      DO  K=M,L,2                                                     
       A=1-MOD(K,4)                                                 
       f0=ftr(J1+J2+L1+L2+2-K);f1=ftr(K);f2=ftr(J1+J2-J3-K);f3=ftr(L1+L2-J3-K)                                               
       f4=ftr(J1+L2-L3-K);f5=ftr(L1+J2-L3-K);f6=ftr(-J1-L1+J3+L3+K); f7=ftr(-J2-L2+J3+L3+K)                                          
	  if(f0.LT.0.OR.f1.LT.0.OR.f2.LT.0.OR.f3.LT.0.OR.f4.LT.0.OR.f5.LT.0.OR.f6.LT.0.OR.f7.LT.0) then
	   u=0.;exit
	  end if
	   f0=f0/(f1*f2*f3*f4*f5*f6*f7)
       u=u+a*f0                                                         
      end do
      sixj=p*d*u                                                        
   END function sixj
	                                                              
   real*8 function  DELTA3(J1,J2,J3)                                    
      real*8   f0,f1,f2,f3
	  integer  j0
	  integer j1,j2,j3
	  delta3=0.
	  j0=(j1+j2+j3)/2
      IF (J0.LT.J1.OR.J0.LT.J2.OR.J0.LT.J3) then
	  delta3=0.;RETURN                  
	  end if
	  f1=ftr(J1+J2-J3); f2=ftr(J1-J2+J3); f3=ftr(-J1+J2+J3);f0=ftr(J1+J2+J3+2)  
	  if(f1.LT.0.OR.f2.LT.0.OR.f3.LT.0.OR.f0.LT.0) then
	  delta3=0.;return;  end if;                                           
      delta3=DSQRT((f1/f0)*f2*f3)                                            
   END function delta3

   real*8 FUNCTION ftr(I)
      REAL*8 fc
	  integer I2,i
	  FC=1.; I2 = I/2
	  IF(I2.GT.180.OR.I2.LT.0.OR.I.NE.I2*2)  then 
       ftr=-1.;return
	  else
	   do while(i2.gt.1);fc=fc*i2; i2=i2-1;end do;ftr=fc;return
      end if
   END function ftr
  
end module matel