MODULE dataman
use spinc
integer RecLen, RecLencr,RecLenel,RecLensp
TYPE red
  integer(1) eln 					  !number of electrons in configuration 
  Integer(1) idin                      !identificator of interaction type
                                      !idin =: 1 - spinorb(V11); 2 -  u2; 3 - u4; 4- u6
  CHARACTER(3), DIMENSION(2) :: iLS     !identificator of states
  INTEGER(4) fc  					  !factor for reduced elements
  INTEGER(1), DIMENSION(11) :: pr 	  !prime number decomposition of reduced elements
END TYPE red  
!******first 11 prime numbers**********************************************************
  real(8),  PARAMETER::  prime(11)=(/2.d0,3.d0,5.d0,7.d0,11.d0,13.d0,17.d0,19.d0,23.d0,29.d0,31.d0/)
!******F down to up conversion parametersfirst**********************************************************
  real(8),  PARAMETER:: fup(3)=(/225.d0,1089.d0,184041./25.d0/)
!******F to E conversion parametersfirst**********************************************************
  real(8),  PARAMETER:: toe1(3)=(/70/9.d0,231/9.d0,2002/9.d0/)
  real(8),  PARAMETER:: toe2(3)=(/1./9.d0,-1/3.d0,7/9.d0/)
  real(8),  PARAMETER:: toe3(3)=(/5/3.d0,2.d0,-91/3.d0/)
!**************************************************************************************
TYPE recr
  integer(1) eln 					  !number of electrons in configuration 
  CHARACTER(3), DIMENSION(2) :: iLS     !identificator of states
  real,dimension(3) :: uk                        !u2,u4,u6
END TYPE recr  

TYPE reel
  integer(2) eln 					  !number of electrons in configuration 
  integer(2) id										 !1-f2,2-f4,3-f6;4-alpha,5-beta,6-gamma
                                                         ! 7-t2,8-t3,9-t4;10-t6,11-t7,12-t8
  CHARACTER(3), DIMENSION(2) :: iLS     !identificator of states
  real :: fk                        !
END TYPE reel  

 TYPE respin
  integer(2) eln 					  !number of electrons in configuration 
  integer(2) j						   !2*J
  integer(1) id										 !1-sporb,2-m2,3-m4;4-m6,5-p2,6-p4,7-p6
  CHARACTER(3), DIMENSION(2) :: iLS     !identificator of states
  real :: fk                        !
END TYPE respin  

TYPE con
  INTEGER eln                !number of electrons in configuration 
  INTEGER  nst 	            !number of states in configuration  
END TYPE con

type conLS
  type (con) con
  character(400)  :: iLS		!descriptor of LS states - 3 characters all states in one string
end type conLS



!**********description of fn configurations*********************************************
!
type (conLS), dimension(7) :: fcon
data fcon.con/con(1,1),  con(2,7), con(3,17),   &
               con(4,47), con(5,73),con(6,119),con(7,119) &
             /
data fcon(1).iLS / '2F '/				                                                                  
data fcon(2).iLS / '3P 3F 3H 1S 1D 1G 1I '/
data fcon(3).iLS / '4S 4D 4F 4G 4I 2P 2D12D22F12F22G12G22H12H22I 2K 2L '/
data fcon(4).iLS &
 /'5S 5D 5F 5G 5I 3P13P23P33D13D23F13F23F33F43G13G23G33H13H23H33H43I13I23K13K23L 3M 1S11S21D11D21D31D41F 1G11G21G31G41H11H21I11I21I31K 1L11L21N '/ 
data fcon(5).iLS &
/'6P 6F 6H 4S 4P14P24D14D24D34F14F24F34F44G14G24G34G44H14H24H34I14I24I34K14K24L 4M 2P12P22P32P42D12D22D32D42D52F12F22F32F42F52F62F72G12G22G32G42G52G62H12H22H32H42H52H62H72I12I22I32I42I52K12K22K32K42K52L12L22L32M12M22N 2O '/		                  
data fcon(6).iLS &
/'7F 5S 5P 5D15D25D35F15F25G15G25G35H15H25I15I25K 5L 3P13P23P33P43P53P63D13D23D33D43D53F13F23F33F43F53F63F73F83F93G13G23G33G43G53G63G73H13H23H33H43H53H63H73H83H93I13I23I33I43I53I63K13K23K33K43K53K63L13L23L33M13M23M33N 3O 1S11S21S31S41P 1D11D21D31D41D51D61F11F21F31F41G11G21G31G41G51G61G71G81H11H21H31H41I11I21I31I41I51I61I71K11K21K31L11L21L31L41M11M21N11N21Q '/												  
data fcon(7).iLS &
/'8S 6P 6D 6F 6G 6H 6I 4S14S24P14P24D14D24D34D44D54D64F14F24F34F44F54G14G24G34G44G54G64G74H14H24H34H44H54I14I24I34I44I54K14K24K34L14L24L34M 4N 2S12S22P12P22P32P42P52D12D22D32D42D52D62D72F12F22F32F42F52F62F72F82F92F02G12G22G32G42G52G62G72G82G92G02H12H22H32H42H52H62H72H82H92I12I22I32I42I52I62I72I82I92K12K22K32K42K52K62K72L12L22L32L42L52M12M22M32M42N12N22O 2Q '/												  

integer*4, dimension(7) :: jlistfl,jlistfu
data jlistfl/5,0,1,0,1,0,1/
data jlistfu/7,12,17,20,23,24,25/

!
!***************************************************************************************
!**********description of dn configurations*********************************************
 type (conLS), dimension(5) :: dcon
 data dcon.con/con(1,1),  con(2,5), con(3,8),   &
               con(4,16), con(5,16)/
 data dcon(1).iLS / '1D '/
 data dcon(2).iLS / '3P 3F 1S 1D 1G '/
 data dcon(3).iLS / '4P 4F 2P 2D12D22F 2G 2H '/
 data dcon(4).iLS &
  /'5D 3P13P23D 3F13F23G 3H 1S11S21D11D21F 1G11G21I '/ 
 data dcon(5).iLS &
 /'6S 4P 4D 4F 4G 2S 2P 2D12D22D32F12F22G12G2 2H 2I '/		                  
!***************************************************************************************

!integer nelec,ie   !nelec- number of electrons in configuration
                        ! ie - configuration token;2 - d;3-f

integer,parameter ::numinter=8
character*26,dimension(8) :: inter
data  inter(1)/'Coulomb(Fk)'/	         
data  inter(2)/'Configuration(2-body)'/ 
data  inter(3)/'Configuration(3-body,t2-4)'/
data  inter(4)/'Configuration(3-body,t6-8)'/
data  inter(5)/'spin-orbit'/
data  inter(6)/'magnetic(mk)'/
data  inter(7)/'magnetic(Pk)'/
data  inter(8)/'crystal-field(uk)'/


!character*8,dimension(2) :: dbfile
!data dbfile/'datad.rd','dataf.rd'/
!character*9,dimension(2) :: dbfiletxt
!data dbfiletxt/'datad.red','dataf.red'/

character*8,dimension(2) :: dbfilecr
data dbfilecr/'recr.dbd','recr.dbf'/
character*8,dimension(2) :: dbfileels
data dbfileels/'elst.dbd','elst.dbf'/
character*8,dimension(2) :: dbfilespin
data dbfilespin/'spin.dbd','spin.dbf'/

character*7,dimension(2) :: dbfilevse
data dbfilevse/'vse.dbd','vse.dbf'/

contains 

!***********update() - update database of reduced elements*****************************
!logical function update()
!  integer err
!  character*9   dbfilestrtxt
!  character*8   dbfilestr

!  type (red) r
!  update=.TRUE.
!  select case(ie)
!  case(2)
!   dbfilestrtxt=dbfiletxt(1);  dbfilestr=dbfile(1);
!  case(3)
!   dbfilestrtxt=dbfiletxt(2);  dbfilestr=dbfile(2);
!  end select
!  inquire(iolength=RecLen)	r
!!  write(*,*) Reclen
!  open(unit=3,file=dbfilestrtxt,iostat=err)
 ! if(err>0) update=.FALSE.
 ! open(UNIT=2,FILE=dbfilestr,status='replace',ACCESS='direct', RECL=RecLen,iostat=err)
 ! if(err>0) update=.FALSE.
 ! DO WHILE (.NOT. EOF(3))
 !  read(3,*,iostat=err) r
 !  if(err>0) update=.FALSE.
 !  write(2) r
!!   write(*,*) LEN(r.iLS(1)),  r.iLS(1)(:2)
!!   write(*,*) LEN(r.iLS(2)),   r.iLS(2)(:2)
!!   write(*,*) r.fc
 !   end do
 ! close(2)
 ! close(3)
!end function
!***************should be used before matj!!********************************************
!************!!!uses unit 77 for data exchange!!!!***************************************  
  logical function  info()
  character(3), allocatable :: cht(:)
  integer  i,nt,inelec

  i=0;nt=0;info=.False.;inelec=nelec
  if(ie.eq.3) then
   if(nelec.gt.7) inelec=14-nelec
    nt=fcon(inelec)%con%nst
  else
   if(ie.eq.2) then
    if(nelec.gt.5) inelec=10-nelec
     nt=dcon(inelec)%con%nst
	else; return; end if
  end if
  allocate(cht(nt))
  cht=' '
  if(nt.gt.30) nt=nt !30
  do i=1,nt
   if(ie.eq.3) then
    cht(i)=fcon(inelec)%iLS(3*(i-1)+1:3*i)
   else
    cht(i)=dcon(inelec)%iLS(3*(i-1)+1:3*i)
   end if
  end do
  i=infow(nt,cht)
  deallocate(cht)
  info=.True.
end function info

integer  function  infow(nt,cht)
  integer, dimension(nt) :: iS, iL
  character(3), dimension(nt) :: cS, cL,cht
  character(1), dimension(nt) :: cad
  character(7) FileStatusTemp
   logical IsThere                                            

  integer  i,nt
   iS=0;iL=0;cS=' ';cL=' ';cad=' '
  do i=1,nt
   cS(i)=cht(i)(:1)
   cL(i)=cht(i)(2:2)
   cad(i)=cht(i)(3:)
   read(cS(i),*) iS(i)
   iS(i)=iS(i)-1
   iL(i)=idL(cht(i)(2:2))
  end do
  INQUIRE (FILE = 'temp.txt', EXIST = IsThere)
  IF (IsThere) THEN; FileStatusTemp = 'REPLACE' ;ELSE; FileStatusTemp = 'NEW';END IF
  open(3,file="temp.txt",status=FileStatusTemp,iostat=ios)
  if(ios.ne.0) return
  write(3,'(I4)') nt
  do i=1,nt 
  write(3,2) iS(i),iL(i),cad(i),cht(i)	 !spin and orbital momentum values
 2 format(I2,I2,A1,A3)
 !  write(*,2) iS(i),iL(i),cad(i),cht	 !spin and orbital momentum values
  end do
  close(3)
  infow=1
end function infow

integer function  idL(ch)
  character(1) ch
  select case(ch)
   case("S");idL=0;case("P");idL=1;case("D");idL=2;case("F");idL=3;case("G");idL=4;case("H");idL=5;case("I");idL=6; case("K");idL=7;case("L");idL=8;case("M");idL=9;case("N");idL=10;case("O");idL=11;case("Q");idL=12;  case default
    write(*,*)"Error deciphering L information"						  
    idL=0
  end select
end function idL

  character(1) function  toL(i)
  integer i
  select case(i)
   case(0);toL='S';case(1);toL='P';case(2);toL='D';case(3);toL='F';case(4);toL='G';case(5);toL='H';case(6);toL='I'; case(7);toL='K';case(8);toL='L';case(9);toL='M';case(10);toL='N';case(11);toL='O';case(12);toL='Q';
   case default
    write(*,*)"Error deciphering L information"						  
    toL=' '
  end select
end function toL

  real*8 function depr(fc,pr)
    integer(4) fc; integer(1), dimension(11) :: pr
	integer  i;	real*8  t
	t=fc
	do i=1,11;if(pr(i).ne.0) then;t=t*DSQRT(prime(i)**pr(i)); end if;end do
	depr=t
  end function depr

 
END MODULE dataman