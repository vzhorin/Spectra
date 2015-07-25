module spinc 

integer*4 , parameter, public :: ID_EXIT  = 001
integer*4 , parameter, public :: ID_HELPCONTENT  =  101
integer*4 , parameter, public :: ID_ABOUT        =  102
integer*4 , parameter, public :: ID_PASSWORD        = -666

integer*4 , parameter, public :: ID_CONFIGURATION           =  201
integer*4 , parameter, public :: ID_FILETREECOMBO  =  2010, ID_FILETREELIST1=2020,ID_FILETREELIST2=2030,ID_FILETREEDIT=2040, ID_FILETREECHANGE=2041
integer*4 , parameter, public :: ID_FILETREECHECK=2050,ID_FILETREECOPY=2060,ID_FILETREENELEC=2070,ID_FILETREEDELETE=2080
integer*4 , parameter, public :: ID_FILENEW  =  10, ID_FILEOPEN=20,ID_FILECLOSE=30,ID_FILESAVE=40,ID_FILESAVEAS=50,ID_COPY=60
integer*4 , parameter, public :: ID_FILENEDIT1 =1010,ID_FILENEDIT2 =1020,ID_FILENEDIT3 =1030 
integer*4 , parameter, public :: ID_SHOWCMD =665


integer*4 , parameter, public :: ID_D1 =  211;integer*4 , parameter, public :: ID_D2           =  212
integer*4 , parameter, public :: ID_D3  =  213;integer*4 , parameter, public :: ID_D4           =  214
integer*4 , parameter, public :: ID_D5 =  215;integer*4 , parameter, public :: ID_D6           =  216
integer*4 , parameter, public :: ID_D7 =  217;integer*4 , parameter, public :: ID_D8           =  218
integer*4 , parameter, public :: ID_D9 =  219;integer*4 , parameter, public :: ID_F1           =  220
integer*4 , parameter, public :: ID_F2  =  221;integer*4 , parameter, public :: ID_F3           =  222
integer*4 , parameter, public :: ID_F4  =  223;integer*4 , parameter, public :: ID_F5           =  224
integer*4 , parameter, public :: ID_F6  =  225;integer*4 , parameter, public :: ID_F7           =  226
integer*4 , parameter, public :: ID_F8  =  227;integer*4 , parameter, public :: ID_F9           =  228
integer*4 , parameter, public :: ID_F10 =  229;integer*4 , parameter, public :: ID_F11          =  230
integer*4 , parameter, public :: ID_F12 =  231;integer*4 , parameter, public :: ID_F13          =  232

integer*4 , parameter, public :: ID_CALCULATE                     =  301
integer*4 , parameter, public :: ID_CALCULATE_SAVE            =  310
integer*4 , parameter, public :: ID_CALCULATE_RESULTS       =  320
integer*4 , parameter, public :: ID_CALCULATE_RESULTSE      =  321

integer*4 , parameter, public :: ID_CALCULATE_VECTORS       =  330
integer*4 , parameter, public :: ID_CALCULATE_VECTORSN       =  331

integer*4 , parameter, public :: ID_CALCULATE_VECTORS2       =  335, ID_CTERMSALL=  340, ID_CTERMSINCLUDED=  345
integer*4 , parameter, public :: ID_CADD =  350,ID_CADDALL =  351,ID_CREMOVE =  352,ID_CREMOVEALL =  353 
integer*4 , parameter, public :: ID_CSTATICNAME=  360, ID_FIT_ACCURACY=361
integer*4 , parameter, public :: ID_ZEEMAN_G      =  370

integer*4 , parameter, public :: ID_FIT=  302
integer*4 , parameter, public :: ID_FASTFIT=  303

integer*4 , parameter, public :: IDL_DATAFIT=  3021
integer*4 , parameter, public :: ID_FITTING_ACCURACY=  3027

integer*4 , parameter, public :: ID_STOPIT=  3000

integer*4 , parameter, public :: ID_MATRIX =  601, ID_MQUERY =  604
integer*4 , parameter, public :: ID_MLIST1 =  610, ID_MLIST2 =  620
integer*4 , parameter, public :: ID_MLIST3=  630, ID_MATRIXVALUE =  640
integer*4 , parameter, public :: ID_MUPDATE =  650
integer*4 , parameter, public :: ID_MTEXT1= 60101, ID_MTEXT2= 60102,ID_MTEXT3= 60103
integer*4 , parameter, public :: ID_MRESULT1= 60201, ID_MRESULT2= 60202,ID_MRESULT3= 60203
integer*4 , parameter, public :: ID_MJLIST=60300
integer*4 , parameter, public :: ID_ENERGY_LEVELS=680

 integer*4 , parameter, public :: DLG_VERFIRST     =   400
 integer*4 , parameter, public :: DLG_VERLAST      =   403

 integer*4 , parameter, public :: ID_PFREEION_NEW   =  501,ID_PFREEION_OPEN =502
 integer*4, parameter,public:: ID_PSAVE=503,ID_PAD_NEW=504,ID_PAD_OPEN=505   

 integer*4, parameter, public :: ID_PFSTATICNAME=515
 integer*4 , parameter, public :: ID_PF2  =  521,ID_PF4  =  522,ID_PF6  =523,ID_PFDZETA  =524
 integer*4 , parameter, public :: ID_PCR_NEW   =  540,ID_PCR_OPEN =541
 
  integer*4, parameter,public:: ID_FIELD_CALCULATE=542
  integer*4, parameter,public:: IDL_SITE=54210, ID_SELECTTITLE=54220
  integer*4, parameter,public:: ID_SELECTRE=54221, ID_LIGAND=54222, ID_GPAR=54230



 integer*4 , parameter, public :: ID_PCR20 = 550, ID_PCR21= 551, ID_PCR22 = 552,&
                                             ID_PCR23 = 553, ID_PCR24 = 554
 integer*4 , parameter, public :: ID_PCR40=560,  ID_PCR41= 561, ID_PCR42 = 562,&
                                              ID_PCR43=563, ID_PCR44= 564, ID_PCR45 = 565,&
											  ID_PCR46=566, ID_PCR47= 567, ID_PCR48 = 568
 integer*4 , parameter, public :: ID_PCR60=570, ID_PCR61= 571, ID_PCR62 = 572,&
                                             ID_PCR63=573, ID_PCR64= 574, ID_PCR65 = 575,&
											 ID_PCR66=576, ID_PCR67= 577, ID_PCR68 = 578,&
											 ID_PCR69=579, ID_PCR70= 580, ID_PCR71 = 581,&
											 ID_PCR72= 582
 integer*4, parameter, public :: ID_PCR_TASKNAME=590
 integer*4, parameter, public :: ID_PA=5010,ID_BE=5011,ID_GA=5012,ID_PT2=50002,ID_PT3=50003,ID_PT4=50004,ID_PT6=50005,ID_PT7=50006,ID_PT8=50007 
 integer*4, parameter, public :: ID_PM0=5101,ID_PM2=5102,ID_PM4=5103,ID_PP2=5104, ID_PP4=5105, ID_PP6=5106
 
 integer*4 , parameter, public :: ID_OPTIONS                     =  710
 integer*4 , parameter, public :: ID_OTAB                          =  711
 integer*4 , parameter, public :: ID_ONAME                          =  712
 integer*4 , parameter, public :: ID_ONORMW                          =  713
 integer*4 , parameter, public :: ID_ONORMS                          =  714
  integer*4 , parameter, public :: ID_OTERMSSEL                      =  720

 integer*4 , parameter, public :: ID_PROGRESSE=1010,ID_PROGRESSM=1020,ID_PROGRESSC=1030
 integer, parameter, public ::  ID_PROGRESSBEGIN=1040
  
 integer, parameter, public ::  ID_SETTING_SAVE=1110
 integer, parameter, public ::  IDC_PAR1=1201,IDC_PAR2=1202, IDC_PAR3=1203,	IDC_PAR4=1204, &
										  IDC_PAR5=1205,IDC_PAR6=1206, IDC_PAR7=1207, IDC_PAR8=1208, IDC_PAR9=1209
 integer, parameter, public ::  IDC_PARM1=1211,IDC_PARM2=1212, IDC_PARM3=1213,	IDC_PARM4=1214, &
										  IDC_PARM5=1215 !spin-orbit
 integer, parameter, public ::  IDC_RADIO_CF1=1251,IDC_RADIO_CF2=1252, IDC_RADIO_CF3=1253,IDC_RADIO_CF4=1254,IDC_RADIO_CF5=1255 
 integer, parameter, public ::  IDC_PARC=1260 !coulomb fitting


 integer,save :: hInst,hWndTB,hWndSTB, hcmd
 integer,save :: hWndfit
 integer hMWindow

 integer,save  :: ie=3
 integer,save  :: nelec=1
 integer,save  :: mansel=1	! mansel=1 - enable manual selection of terms
 integer,save :: menuenabling=0



 character*40,save :: taskname='Default task'C
 character*12,save   ::  curdirpath='C:\\Spectra'C

 character*8,save :: bave !value for average deviation

 integer*4,public,save :: iedef=ID_F1
 integer,save :: efix=30000
 real,save :: wavefix=0.1	 !minimal norm of eigenvectors for output
 integer,save :: numfix=40 !number of vectors per energy level
 integer*4,parameter,public :: ID_OVECT=730,ID_OVECTNUMB=732, ID_OENERGY=734

 integer*4,parameter,public :: MAXTERM=47
 !ioptima - whether to use optimization for matrix diag.
 integer*4,parameter, public :: ioptima=1 !optimization on - 1 
 integer*4, public, save :: ifastmat=0

integer,public :: nDisplayWidth,nDisplayHeight
integer*4,  save :: genmax=8
real, allocatable :: tf(:)
integer*4 numlevtofit

integer*4,save ::  fastfitflag

integer*4,parameter,public  :: ID_LICENSE=1111,ID_DISCLAIMER=1112
character*800 :: disclaimer=" DISCLAIMER&
NEITHER THE UNITED STATES GOVERNMENT NOR ANY& 
AGENCY THEREOF, NOR THE UNIVERSITY&
OF CHICAGO, OR ANY OF THEIR EMPLOYEES,&
 MAKES ANY WARRANTY, EXPRESS OR IMPLIED,&
OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY&
 FOR THE ACCURACY, COMPLETENESS, &
OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, &
OR PROCESS DISCLOSED, OR&
REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS"C
character*800 ::  license="License\n Portions of this material resulted from work developed under a U.S. Goverment contract &
and are subject to the following license: the Government is granted for itself and &
others acting in its behalf a paid-up, nonexclusive, irrevocable worldwide license in &
this computer software to reproduce, prepare derivative works, and perform publicly and &
display publicly"C


end module		 


