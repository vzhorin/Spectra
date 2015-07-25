module multi
use msfwina
integer,save :: hDlgPB
integer,parameter, public :: WM_FINISH_EL=WM_USER+1
integer,parameter, public :: WM_FINISH_MA=WM_USER+2
integer,parameter, public :: WM_FINISH_CR=WM_USER+3

contains

integer function threadc(lpar)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_threadc@4' :: threadc
use matrix
use spinc
use  ctrl
 integer lpar,inelec,numstep,i,ifull,iz
 
 lpar=lpar;  inelec=nelec
   select case(ie);
    case(2);if(nelec.gt.5) inelec=10-inelec
    case(3);if(nelec.gt.7) inelec=14-inelec
   end select
 rcr=rcr
 numstep=SIZE(rcrb)
 ifull=Size(rcrbfull)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETRANGE,0,makelparam(1,numstep+1))
 !ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETSTEP,1,0)
 !ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_SETPOS,0,0)
 i=0
 do j=1,ifull
  rcr=rcrbfull(j)
  if(rcr%eln.eq.inelec) then
   rcrb(i+1)=rcr
   if((ie.eq.2.and.nelec.gt.5).or.(ie.eq.3.and.nelec.gt.7)) then
    do iz=1,3;rcrb(i+1)%uk(iz)=-rcrb(i+1)%uk(iz);end do
   end if
   i=i+1
   ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSC,PBM_STEPIT,0,0)
  end if
 end do
 deallocate(rcrbfull)
 ret=SendMessage(hDlgPB,WM_FINISH_CR,0,0)
 ret=SendMessage(hDlgPB,WM_COMMAND,3,3)
 threadc=0;return
end function threadc

integer function threadm(lpar)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_threadm@4' :: threadm
use matrix
use spinc
use ctrl
 integer lpar,numstep,i,ifull
 lpar=lpar												   
 numstep=SIZE(respinb)
 ifull=Size(respinbfull)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSM,PBM_SETRANGE,0,makelparam(1,numstep+1))
 
 i=0
 do j=1,ifull
  rsp=respinbfull(j)
  if(rsp%eln.eq.nelec) then
   respinb(i+1)=rsp
   i=i+1
   ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSM,PBM_STEPIT,0,0)
    end if
 end do
deallocate(respinbfull)	
ret=SendMessage(hDlgPB,WM_FINISH_MA,0,0)
ret=SendMessage(hDlgPB,WM_COMMAND,3,3)
threadm=0;return
end function threadm

integer function threade(lpar)
!MS$ ATTRIBUTES STDCALL, ALIAS : '_threade@4' :: threade
use matrix
use spinc
use ctrl
 integer lpar,numstep,i,ifull
 
 lpar=lpar												   
 numstep=SIZE(relb)
 ifull=Size(relbfull)
 ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSE,PBM_SETRANGE,0,makelparam(1,numstep+1))

 i=0
 do j=1,ifull
  rel=relbfull(j)
  if(rel%eln.eq.nelec) then
   relb(i+1)=rel
   i=i+1;
   ret=SendDlgItemMessage(hDlgPB,ID_PROGRESSE,PBM_STEPIT,0,0)
  end if
 end do
deallocate(relbfull)
ret=SendMessage(hDlgPB,WM_FINISH_EL,0,0)
ret=SendMessage(hDlgPB,WM_COMMAND,3,3)
threade=0;return
end function threade

end module multi
