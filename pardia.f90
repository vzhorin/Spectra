module pardia
type vse
 character*4 ion				             !ion name, for example, Cm3+
 character*32 desc			                !description of compound/job
 integer ie 					             	!2-d,3-f
 integer ne						              	!number of electrons in a shell
 real*4,dimension(12) :: electro        ! Coulomb and configuration
 real*4,dimension(7) :: magnetic         !spin related interactions
 character*3 group							   !symmetry group, example C3h
 complex, dimension(3,7) :: Bpk  !crystal-field parameters
 integer(1) prot
end type vse

type(vse),save :: vsecurrent
integer :: icurrent
real,save, dimension(12) :: electro
real,save, dimension(7) :: magnetic
complex, save, dimension(3,7) :: Bpk

integer, save, dimension(12) :: ielectro
integer,save, dimension(7) :: imagnetic
integer, save, dimension(3,7) :: iBpk
integer*4  maskcoulomb, maskcrystal,maskspin,maskgreek(3),maskelectro(6),maskmagnetic(4)
real*8, save :: ratcu4440=0.597614304667,ratcu6460=-1.87082869339  !ratio for cubic CFP in Wybourne norm
real,save :: fconstraint4=0.808, fconstraint6=0.573
real, save :: pconstraint4=0.5, pconstraint6=0.1

real,save :: rangeelectro=0.1,rangemagnetic=0.2,rangecrystal2=1.9,rangecrystal4=1.8,rangecrystal6=1.5



!*****************<l||Cp||l> elements for f-configuration*******************
real, parameter, dimension(3):: ff=(/-1.366260102,1.12815215,-1.27738077/)

contains

subroutine FreeIon()
 electro=vsecurrent.electro;magnetic=vsecurrent.magnetic
end subroutine FreeIon

subroutine crfpar()
 Bpk=vsecurrent.Bpk
end subroutine crfpar

subroutine Initvse()
use spinc
 taskname=vsecurrent.ion//" in "//vsecurrent.desc 
 ie=vsecurrent.ie; nelec=vsecurrent.ne
end subroutine Initvse

end module pardia