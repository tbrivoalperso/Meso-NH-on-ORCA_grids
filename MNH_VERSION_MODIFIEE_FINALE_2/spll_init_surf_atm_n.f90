!depfile:init_surf_atmn.D
!     ######spl
SUBROUTINE INIT_SURF_ATM_n (YSC, &
                            HPROGRAM,HINIT, OLAND_USE,                   &
                             KI,KSV,KSW,                                &
                             HSV,PCO2,PRHOA,                            &
                             PZENITH,PAZIM,PSW_BANDS,PDIR_ALB,PSCA_ALB, &
                             PEMIS,PTSRAD,PTSURF,                       &
                             KYEAR, KMONTH,KDAY, PTIME,                 &
                             HATMFILE,HATMFILETYPE,                     &
                             HTEST                                      )  
!#############################################################
!
!!****  *INIT_SURF_ATM_n* - routine to initialize GROUND
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    MODD_GRID_n
!!    	XLAT,XLON 		Latitude/longitude
!!   	XDXHAT2D,XDYHAT2D 
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003
!      (P.Tulet )             01/11/03  initialisation of the surface chemistry!
!!     (D.Gazen)    01/12/03  change emissions handling for surf. externalization
!!     (P.LeMoigne) 18/07/05  get 1d mask only if associated tile exists    
!!     (B.Decharme)  03/2009  New keys read for arrange cover by user
!!     (B.Decharme)  04/2009  Read precipitation forcing from the restart file for ARPEGE/ALADIN run
!!     (A. Lemonsu)    2009   New key read for urban green areas
!!     (B.Decharme)  07/2011  Read pgd+prep
!!     (S. Queguiner)  2011   Modif chemistry (2.4)
!!     (B. Decharme)   2013   Read grid only once in AROME case
!!     (G. Tanguy)     2013   Add IF(ALLOCATED(NMASK_FULL))  before deallocate
!!      B. Decharme  04/2013  new coupling variables
!!                            Delete LPROVAR_TO_DIAG check
!!                            Delete NWG_LAYER_TOT
!!     (J.Escobar)      10/06/2013: replace DOUBLE PRECISION by REAL to handle problem for promotion of real on IBM SP
!!     (J.Durand)      2014   add activation of chemical deposition if LCH_EMIS=F
!!      R. S??f??rian 03/2014   Adding decoupling between CO2 seen by photosynthesis and radiative CO2
!!      M.Leriche & V. Masson 05/16 bug in write emis fields for nest
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!EDIT -T BRIVOAL
USE MODD_CST
USE MODE_FM
USE MODE_FMREAD
USE MODI_GATHER_ll
USE MODD_DIM_n
USE MODD_GRID_n
USE MODD_SHADOWS_n 
USE MODD_GRID
USE MODD_LUNIT
USE MODD_LUNIT_n
USE MODN_CONF, ONLY : JPHEXT
USE MODE_SPLITTINGZ_ll , ONLY : GET_DIM_EXTZ_ll
!/
USE MODD_SURFEX_n, ONLY : SURFEX_t
!
USE MODD_SURF_ATM,       ONLY : XCO2UNCPL
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_DST_SURF,       ONLY : NDSTMDE, NDST_MDEBEG, LVARSIG_DST, LRGFIX_DST 
USE MODD_SLT_SURF,       ONLY : NSLTMDE, NSLT_MDEBEG, LVARSIG_SLT, LRGFIX_SLT                                

USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
USE MODD_DATA_COVER,     ONLY : LCLIM_LAI, XDATA_LAI_ALL_YEARS, XDATA_LAI, &
                                NECO2_START_YEAR, NECO2_END_YEAR  
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_CHS_AEROSOL,    ONLY : LVARSIGI, LVARSIGJ
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE  
!
USE MODD_SURFEX_MPI, ONLY : XTIME_INIT_SEA, XTIME_INIT_WATER, XTIME_INIT_NATURE, XTIME_INIT_TOWN, &
                            NRANK, NPIO, NSIZE
USE MODD_SURFEX_OMP, ONLY : NINDX2SFX, NWORK, NWORK2, XWORK, XWORK2, XWORK3, &
                            NWORK_FULL, NWORK2_FULL, XWORK_FULL, XWORK2_FULL, &
                            NBLOCKTOT
!
USE MODD_MASK, ONLY: NMASK_FULL
!
USE MODI_INIT_IO_SURF_n
USE MODI_DEFAULT_SSO
USE MODI_DEFAULT_CH_SURF_ATM
USE MODI_DEFAULT_DIAG_SURF_ATM
USE MODI_READ_DEFAULT_SURF_ATM_n
USE MODI_READ_SURF_ATM_CONF_n
USE MODI_READ_SURF_ATM_DATE
USE MODI_READ_NAM_PREP_SURF_n
USE MODI_READ_SURF
USE MODI_GET_SIZES_PARALLEL
USE MODI_SUNPOS
USE MODI_GET_SIZE_FULL_n
USE MODI_READ_COVER_n
USE MODI_READ_SSO_n
USE MODI_SUBSCALE_Z0EFF
USE MODI_READ_SSO_CANOPY_n
USE MODI_READ_DUMMY_n
USE MODI_READ_GRID
USE MODI_READ_GRIDTYPE
USE MODI_END_IO_SURF_n
USE MODI_PREP_CTRL_SURF_ATM
USE MODI_AVERAGE_RAD
USE MODI_AVERAGE_TSURF
USE MODI_INIT_CHEMICAL_n
USE MODI_CH_INIT_DEPCONST
USE MODI_CH_INIT_EMISSION_n
USE MODI_CH_INIT_SNAP_n
USE MODI_ABOR1_SFX
USE MODI_ALLOC_DIAG_SURF_ATM_n
USE MODI_GET_1D_MASK
USE MODI_INI_DATA_COVER
USE MODI_INIT_INLAND_WATER_n
USE MODI_INIT_NATURE_n
USE MODI_INIT_SEA_n
USE MODI_INIT_TOWN_n
USE MODI_READ_ARRANGE_COVER
USE MODI_READ_COVER_GARDEN
USE MODI_READ_ECO2_IRRIG
USE MODI_READ_LCLIM_LAI
USE MODI_READ_LECOCLIMAP
USE MODI_SURF_VERSION
USE MODI_GET_LUOUT
USE MODI_SET_SURFEX_FILEIN
!
USE MODI_INIT_CPL_GCM_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!
!$ INCLUDE 'omp_lib.h'
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE ! 
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KI),  INTENT(IN)  :: PAZIM     ! solar azimuthal angle (rad from N, clock)
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
INTEGER,                          INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,                          INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,                          INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                             INTENT(IN)  :: PTIME     ! current time since
                                                          !  midnight (UTC, s)
!
 CHARACTER(LEN=28),                INTENT(IN)  :: HATMFILE    ! atmospheric file name
 CHARACTER(LEN=6),                 INTENT(IN)  :: HATMFILETYPE! atmospheric file type
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'


!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=3)  :: YREAD
!
INTEGER           :: ISWB     ! number of shortwave bands
INTEGER           :: JTILE    ! loop counter on tiles
INTEGER           :: IRESP    ! error return code
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IVERSION, IBUGFIX       ! surface version
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISIZE_OMP
!
LOGICAL           :: LZENITH  ! is the PZENITH field initialized ?
!
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZFRAC_TILE     ! fraction of each surface type
REAL, DIMENSION(KI,KSW,NTILESFC)    :: ZDIR_ALB_TILE  ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC)    :: ZSCA_ALB_TILE  ! diffuse albedo
REAL, DIMENSION(KI,NTILESFC)        :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC)        :: ZTSRAD_TILE    ! radiative temperature
REAL, DIMENSION(KI,NTILESFC)        :: ZTSURF_TILE    ! effective temperature
REAL, DIMENSION(KI)                 :: ZZENITH        ! zenith angle
REAL, DIMENSION(KI)                 :: ZAZIM          ! azimuth angle
REAL, DIMENSION(KI)                 :: ZTSUN          ! solar time since midnight
!
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_ZENITH   ! zenithal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_AZIM     ! azimuthal angle
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_CO2      ! air CO2 concentration
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_RHOA     ! air density
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_DIR_ALB  ! direct albedo
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZP_SCA_ALB  ! diffuse albedo
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_EMIS     ! emissivity
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_TSRAD    ! radiative temperature
REAL, DIMENSION(:),     ALLOCATABLE :: ZP_TSURF    ! surface effective temperature
!
REAL :: XTIME0
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!EDIT T- BRIVOAL
!REAL, DIMENSION(:,:), ALLOCATABLE:: XLATV0       ! latitude  (degrees)
!REAL, DIMENSION(:,:), ALLOCATABLE:: XLONV0       ! longitude (degrees)
!REAL, DIMENSION(:,:), ALLOCATABLE:: XLATV0_ll       ! latitude  (degrees) (whole domain)
!REAL, DIMENSION(:,:), ALLOCATABLE:: XLONV0_ll       ! longitude (degrees) (whole domain)
REAL, DIMENSION(:,:), ALLOCATABLE:: PDIR2D       ! longitude (degrees)
REAL, DIMENSION(:,:), ALLOCATABLE:: PDIR2DR       ! longitude (degrees)
REAL, DIMENSION(:,:), ALLOCATABLE:: XLATRED       ! latitude  (degrees)
REAL, DIMENSION(:,:), ALLOCATABLE:: XLONRED       ! longitude (degrees)
!REAL, DIMENSION(:,:), ALLOCATABLE:: PDXHAT2D,PDYHAT2D         ! Latitude and longitude
REAL, DIMENSION(:,:), ALLOCATABLE::  PMESH_SIZE2D        ! Latitude and longitude
REAL, DIMENSION(:,:), ALLOCATABLE::  PMESH_SIZER        ! Latitude and longitude
INTEGER      :: IIU,IJU     ! Uupper bounds of PXHAT,PYHAT
INTEGER      :: IIU_ll,IJU_ll     ! Uupper bounds of PXHAT,PYHAT
INTEGER                :: II, IJ,JI,JJ, IGRID, ILENGTH
INTEGER                            :: JLOOP1
 CHARACTER (LEN=100)    :: HCOMMENT
 CHARACTER (LEN=100)    :: HFILEN
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!
 CPROGNAME=HPROGRAM
!
IF (HTEST/='OK') THEN
   CALL ABOR1_SFX('INIT_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
!-------------------------------------------------------------------------------
!
 CALL SURF_VERSION
!
!-------------------------------------------------------------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (LNAM_READ) THEN
 !
 !*       0.     Defaults
 !               --------
 !
 !        0.1. Hard defaults
 !      
 CALL DEFAULT_SSO(YSC%USS%CROUGH,YSC%USS%XFRACZ0,YSC%USS%XCOEFBE)
 CALL DEFAULT_CH_SURF_ATM(YSC%CHU%CCHEM_SURF_FILE,YSC%CHU%LCH_SURF_EMIS)
 CALL DEFAULT_DIAG_SURF_ATM(YSC%DGU%N2M,YSC%DGU%LT2MMW,YSC%DGU%LSURF_BUDGET,YSC%DGU%L2M_MIN_ZS,&
                            YSC%DGU%LRAD_BUDGET, &
                            YSC%DGU%LCOEF,YSC%DGU%LSURF_VARS,YSC%DGU%LSURF_BUDGETC,          &
                            YSC%DGU%LRESET_BUDGETC,YSC%DGU%LSELECT, YSC%DGU%LPROVAR_TO_DIAG, &
                            YSC%DGU%LDIAG_GRID, YSC%DGU%LFRAC, YSC%DGU%XDIAG_TSTEP )                       
 !
ENDIF
!
!        0.2. Defaults from file header
!    
 CALL READ_DEFAULT_SURF_ATM_n(YSC%CHU, YSC%DGU, YSC%USS, &
                              HPROGRAM)
!
!*       1.     Reading of configuration
!               ------------------------
!
!        1.1. general options (diagnostics, etc...)
!
 CALL READ_SURF_ATM_CONF_n(YSC%CHU, YSC%DGU, YSC%USS, &
                           HPROGRAM)
!
IF(XCO2UNCPL/=XUNDEF)THEN
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!          WARNING    WARNING               !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!! Decoupling between CO2 for photosynthesis !!!' 
  WRITE(ILUOUT,*)'!!! and atmospheric CO2 activated             !!!'
  WRITE(ILUOUT,*)'!!! In NAM_SURF_ATM XCO2UNCPL =',XCO2UNCPL,'  !!!'
  WRITE(ILUOUT,*)'!!!                                           !!!'
  WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'        
ENDIF
!
!        1.2. Date
!
SELECT CASE (HINIT)
  CASE ('PGD')
    YSC%U%TTIME%TDATE%YEAR = NUNDEF
    YSC%U%TTIME%TDATE%MONTH= NUNDEF
    YSC%U%TTIME%TDATE%DAY  = NUNDEF
    YSC%U%TTIME%TIME       = XUNDEF
        
  CASE ('PRE')
    ! check that diagnostics are off if hinit=='pre'
    CALL PREP_CTRL_SURF_ATM(YSC%DGU%N2M,YSC%DGU%LSURF_BUDGET,YSC%DGU%L2M_MIN_ZS,YSC%DGU%LRAD_BUDGET,&
                            YSC%DGU%LCOEF,YSC%DGU%LSURF_VARS,    &
                            YSC%DGU%LSURF_BUDGETC,YSC%DGU%LRESET_BUDGETC,LNOWRITE_TEXFILE,YSC%DGU%LSELECT,ILUOUT,&
                            YSC%DGU%LPROVAR_TO_DIAG)  
    ! preparation of fields  (date not present in PGD file)
    IF (LNAM_READ) CALL READ_NAM_PREP_SURF_n(HPROGRAM)
    CALL READ_SURF_ATM_DATE(&
                            HPROGRAM,HINIT,ILUOUT,HATMFILE,HATMFILETYPE,KYEAR,KMONTH,KDAY,PTIME,YSC%U%TTIME)

  CASE DEFAULT
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%DGU, YSC%U, &
                        HPROGRAM,'FULL  ','SURF  ','READ ')
    CALL READ_SURF(&
                   HPROGRAM,'DTCUR',YSC%U%TTIME,IRESP)
    CALL END_IO_SURF_n(HPROGRAM)

END SELECT
!
!-----------------------------------------------------------------------------------------------------
! READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!        1.3. Schemes used
!
!         Initialisation for IO
!
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%DGU, YSC%U, &
                        HPROGRAM,'FULL  ','SURF  ','READ ')
!
 CALL READ_SURF(&
                   HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'BUG',IBUGFIX,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND.IBUGFIX>=2) THEN
  CALL READ_SURF(&
                   HPROGRAM,'STORAGETYPE',YREAD,IRESP)
ENDIF
!         reading
!
 CALL READ_SURF(&
                   HPROGRAM,'SEA   ',YSC%U%CSEA   ,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'WATER ',YSC%U%CWATER ,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'NATURE',YSC%U%CNATURE,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'TOWN  ',YSC%U%CTOWN  ,IRESP)
!
 CALL READ_SURF(&
                   HPROGRAM,'DIM_FULL  ',YSC%U%NDIM_FULL,  IRESP)
IF (HINIT=='PRE') THEN
  NINDX2SFX = YSC%U%NDIM_FULL
  NSIZE = YSC%U%NDIM_FULL
  CALL END_IO_SURF_n(HPROGRAM)
  !Initialize full mask with good dimension
  IF (ALLOCATED(NMASK_FULL)) DEALLOCATE(NMASK_FULL)
  CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%DGU, YSC%U, &
                        HPROGRAM,'FULL  ','SURF  ','READ ')
  ALLOCATE(NWORK(YSC%U%NDIM_FULL))
  ALLOCATE(XWORK(YSC%U%NDIM_FULL))
  ALLOCATE(NWORK2(YSC%U%NDIM_FULL,10))  
  ALLOCATE(XWORK2(YSC%U%NDIM_FULL,10))
  ALLOCATE(XWORK3(YSC%U%NDIM_FULL,10,10))
  IF (NRANK==NPIO) THEN
    ALLOCATE(NWORK_FULL(YSC%U%NDIM_FULL))
    ALLOCATE(XWORK_FULL(YSC%U%NDIM_FULL))
    ALLOCATE(NWORK2_FULL(YSC%U%NDIM_FULL,10))
    ALLOCATE(XWORK2_FULL(YSC%U%NDIM_FULL,10))
  ELSE
    ALLOCATE(NWORK_FULL(0))
    ALLOCATE(XWORK_FULL(0))
    ALLOCATE(NWORK2_FULL(0,0))
    ALLOCATE(XWORK2_FULL(0,0))
  ENDIF
ENDIF  
 CALL READ_SURF(&
                   HPROGRAM,'DIM_SEA   ',YSC%U%NDIM_SEA,   IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'DIM_NATURE',YSC%U%NDIM_NATURE,IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'DIM_WATER ',YSC%U%NDIM_WATER, IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'DIM_TOWN  ',YSC%U%NDIM_TOWN,  IRESP)
 CALL READ_SURF(&
                   HPROGRAM,'GRID_TYPE ',YSC%UG%CGRID,  IRESP)


 CALL READ_LECOCLIMAP(&
                      HPROGRAM,YSC%U%LECOCLIMAP)
 CALL READ_ARRANGE_COVER(&
                         HPROGRAM,YSC%U%LWATER_TO_NATURE,YSC%U%LTOWN_TO_ROCK)
 CALL READ_COVER_GARDEN(&
                        HPROGRAM,YSC%U%LGARDEN)
!
!* reads if climatological LAI is used or not for ecoclimap2. If not, looks for year to be used.
 CALL READ_LCLIM_LAI(&
                     HPROGRAM,LCLIM_LAI)
IF (.NOT. LCLIM_LAI .AND. YSC%U%TTIME%TDATE%YEAR >= NECO2_START_YEAR &
                     .AND. YSC%U%TTIME%TDATE%YEAR <= NECO2_END_YEAR   ) YSC%DTCO%NYEAR=YSC%U%TTIME%TDATE%YEAR
 CALL INI_DATA_COVER(YSC%DTCO, YSC%U)
 CALL READ_ECO2_IRRIG(&
                      YSC%DTCO, &
                      HPROGRAM)
!
!*       2.     Cover fields and grid:
!               ---------------------
!
!        2.0. Get number of points on this proc
!
 CALL GET_SIZE_FULL_n(YSC%U, &
                      HPROGRAM,YSC%U%NDIM_FULL,YSC%U%NSIZE_FULL)
!
!        2.1. Read cover
!
 CALL READ_COVER_n(YSC%DTCO, YSC%U, &
                   HPROGRAM)
!
!        2.2. Read grid
!
ALLOCATE(YSC%UG%XLAT       (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%XLON       (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%UG%XMESH_SIZE (YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJPDIR(YSC%U%NSIZE_FULL))
!Edit T-BRIVOAL: Here, READ_GRID doesn't read the grid, but the lat/lon is already in the routine from an implicit argument
 CALL READ_GRID(&
                HPROGRAM,YSC%UG%CGRID,YSC%UG%XGRID_PAR,YSC%UG%XLAT,YSC%UG%XLON,YSC%UG%XMESH_SIZE,IRESP,YSC%USS%XZ0EFFJPDIR)
YSC%UG%NGRID_PAR=SIZE(YSC%UG%XGRID_PAR)
!
!EDIT T-BRIVOAL
IF (HINIT=='PRE') THEN
HFILEN=CPGDFILE

ELSE
HFILEN=CINIFILEPGD

ENDIF



  ! 2.2.1 Allocations and reading lat/lon from PGD
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IIU_ll=NIMAX_ll + 2 * JPHEXT
IJU_ll=NJMAX_ll + 2 * JPHEXT


ALLOCATE(xlatt(IIU,IJU))
ALLOCATE(xlatv(IIU,IJU))
ALLOCATE(xlont(IIU,IJU))
ALLOCATE(xlonv(IIU,IJU))

ALLOCATE(PMESH_SIZE2D(IIU,IJU))
ALLOCATE(PDIR2D(IIU,IJU))



CALL FMREAD(HFILEN,'latt',CLUOUT,'XY',xlatt,IGRID,ILENGTH,HCOMMENT,IRESP)  
CALL FMREAD(HFILEN,'latv',CLUOUT,'XY',xlatv,IGRID,ILENGTH,HCOMMENT,IRESP)
CALL FMREAD(HFILEN,'lont',CLUOUT,'XY',xlont,IGRID,ILENGTH,HCOMMENT,IRESP)  
CALL FMREAD(HFILEN,'lonv',CLUOUT,'XY',xlonv,IGRID,ILENGTH,HCOMMENT,IRESP)! Needed to compute the PDIR
!Get array on the whole domain t -brivoal

!CALL GATHERALL_FIELD_ll('XY',xlatt,xlatt_ll,IRESP)
!CALL GATHERALL_FIELD_ll('XY',xlatu,xlatu_ll,IRESP)



! 2.2.2 Computation of 2D mesh size and PDIR


PDIR2D(:,:)=ATAN(((xlont(:,:)-xlonv(:,:))*COS(xlatt(:,:)*(XPI/180)))/(xlatt(:,:)-xlatv(:,:)))
PMESH_SIZE2D(:,:) = XDXHAT2D(:,:)*XDYHAT2D(:,:)

! 2.2.3 GET ONLY VALUES WICH ARE INSIDE THE REAL DOMAIN

ALLOCATE(XLATRED(NIMAX,NJMAX))
ALLOCATE(XLONRED(NIMAX,NJMAX))
ALLOCATE(PDIR2DR(NIMAX,NJMAX))
ALLOCATE(PMESH_SIZER(NIMAX,NJMAX))

XLATRED(:,:)=XLAT(2:(IIU-1),2:(IJU-1))
XLONRED(:,:)=XLON(2:(IIU-1),2:(IJU-1))
PDIR2DR(:,:)=PDIR2D(2:(IIU-1),2:(IJU-1))
PMESH_SIZER(:,:)=PMESH_SIZE2D(2:(IIU-1),2:(IJU-1))

! 2.2.4 Assign lon - lat and mesh size to structure 

JLOOP1 = 0
DO JJ = 1, NJMAX

   YSC%UG%XLON(JLOOP1+1:JLOOP1+NIMAX)=XLONRED(1:NIMAX,JJ)
   YSC%UG%XLAT(JLOOP1+1:JLOOP1+NIMAX)=XLATRED(1:NIMAX,JJ)
   YSC%UG%XMESH_SIZE(JLOOP1+1:JLOOP1+NIMAX)=PMESH_SIZER(1:NIMAX,JJ)
   YSC%USS%XZ0EFFJPDIR(JLOOP1+1:JLOOP1+NIMAX)=PDIR2DR(1:NIMAX,JJ)
   JLOOP1 = JLOOP1 + NIMAX

END DO
DEALLOCATE(PMESH_SIZE2D)


!        2.3. Initialize zenith and azimuth angles if not done yet
!
LZENITH = ALL(PZENITH /= XUNDEF)
IF (.NOT. LZENITH) THEN
!$OMP PARALLEL
!$ NBLOCKTOT = OMP_GET_NUM_THREADS()
!$OMP END PARALLEL
  ALLOCATE(ISIZE_OMP(0:NBLOCKTOT-1))
  CALL GET_SIZES_PARALLEL(YSC%DTCO, YSC%DGU, YSC%UG, YSC%U, &
                          NBLOCKTOT,KI,0,ISIZE_OMP)
  CALL SUNPOS(ISIZE_OMP,KYEAR, KMONTH, KDAY, PTIME, YSC%UG%XLON, YSC%UG%XLAT, ZTSUN, ZZENITH, ZAZIM)
  DEALLOCATE(ISIZE_OMP)
ENDIF

!
IF (HPROGRAM/='AROME '.AND.NRANK==NPIO) THEN
  !
  IF (.NOT.ASSOCIATED(YSC%UG%XGRID_FULL_PAR)) THEN
    CALL READ_GRIDTYPE(&
                       HPROGRAM,YSC%UG%CGRID,YSC%UG%NGRID_PAR,YSC%U%NSIZE_FULL,.FALSE.,HDIR='H')
    ALLOCATE(YSC%UG%XGRID_FULL_PAR(YSC%UG%NGRID_PAR))
    CALL READ_GRIDTYPE(&
                       HPROGRAM,YSC%UG%CGRID,YSC%UG%NGRID_PAR,YSC%U%NSIZE_FULL,.TRUE.,&
                       YSC%UG%XGRID_FULL_PAR,IRESP,HDIR='H')
  ENDIF
  !
ENDIF
!
!*       2.4     Allocation of chemical species name, chemical index of HSV array 
!
 CALL INIT_CHEMICAL_n(ILUOUT, KSV, HSV, YSC%SV,        &
                     YSC%CHU%CCH_NAMES, YSC%CHU%CAER_NAMES     )
!
!        2.4 Initialize Chemical Emissions
!
 CALL READ_SURF(&
                   HPROGRAM,'CH_EMIS',YSC%CHU%LCH_EMIS,IRESP)
!
IF (YSC%CHU%LCH_EMIS) THEN
  !
  IF ( IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<3 ) THEN
    YSC%CHU%CCH_EMIS='AGGR'
  ELSE
    CALL READ_SURF(&
                   HPROGRAM,'CH_EMIS_OPT',YSC%CHU%CCH_EMIS,IRESP)
  END IF
  !
    !
  IF (YSC%CHU%CCH_EMIS=='AGGR') THEN
    CALL CH_INIT_EMISSION_n(&
                            YSC%CHE, YSC%CHU, YSC%SV, &
                            HPROGRAM,YSC%U%NSIZE_FULL,HINIT,PRHOA,YSC%CHU%CCHEM_SURF_FILE) 
  ELSE
    CALL CH_INIT_SNAP_n(&
                        YSC%CHN, YSC%SV, &
                       HPROGRAM,YSC%U%NSIZE_FULL,HINIT,PRHOA,YSC%CHU%CCHEM_SURF_FILE)
  END IF
  !
END IF
    !
    !*       2.5 Initialization of dry deposition scheme (chemistry)
    !    
!
IF (YSC%SV%NBEQ .GT. 0) THEN
!
  IF (HINIT=='ALL') CALL CH_INIT_DEPCONST(HPROGRAM, YSC%CHU%CCHEM_SURF_FILE,ILUOUT,YSC%SV%CSV(YSC%SV%NSV_CHSBEG:YSC%SV%NSV_CHSEND))
!
!
END IF
!
!*       2.5 Subgrid orography
!
 CALL READ_SSO_n(&
                 YSC%U, YSC%USS, &
                 HPROGRAM)
!
!*       2.6 Orographic roughness length
!
ALLOCATE(YSC%USS%XZ0EFFIP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFIM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJP(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0EFFJM(YSC%U%NSIZE_FULL))
ALLOCATE(YSC%USS%XZ0REL  (YSC%U%NSIZE_FULL))
!
 CALL SUBSCALE_Z0EFF(YSC%USS%XAOSIP,YSC%USS%XAOSIM,YSC%USS%XAOSJP,YSC%USS%XAOSJM,         &
                    YSC%USS%XHO2IP,YSC%USS%XHO2IM,YSC%USS%XHO2JP,YSC%USS%XHO2JM,0.,      &
                    YSC%USS%XZ0EFFIP,YSC%USS%XZ0EFFIM,YSC%USS%XZ0EFFJP,YSC%USS%XZ0EFFJM, &
                    YSC%USS%XZ0REL                               )
!
!*       2.7 Dummy fields
!
 CALL READ_DUMMY_n(&
                   YSC%DUU,YSC% U, &
                   HPROGRAM)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP') ! restore input file name
!
!-----------------------------------------------------------------------------------------------------
! END READ PGD FILE
!-----------------------------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
 CALL INIT_IO_SURF_n(YSC%DTCO, YSC%DGU, YSC%U, &
                        HPROGRAM,'FULL  ','SURF  ','READ ')
!
!*       2.8 Allocations and Initialization of diagnostics
!
IF (HINIT=='ALL') CALL ALLOC_DIAG_SURF_ATM_n(&
                                             YSC%DGU, YSC%U, &
                                             HPROGRAM,KSW)
!
!
!*       Canopy fields if Beljaars et al 2004 parameterization is used
!
IF (YSC%USS%CROUGH=='BE04') CALL READ_SSO_CANOPY_n(YSC%DTCO, YSC%SSCP, YSC%U, &
                                               HPROGRAM,HINIT)
!
!*       Physical fields need for ARPEGE/ALADIN climate run
!
 CALL INIT_CPL_GCM_n(&
                     YSC%U, &
                     HPROGRAM,HINIT)
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!-----------------------------------------------------------------------------------------------------
!
!*       4.     Initialization of masks for each surface
!               ----------------------------------------
!
!* number of geographical points
YSC%U%NSIZE_NATURE    = COUNT(YSC%U%XNATURE(:) > 0.0)
YSC%U%NSIZE_TOWN      = COUNT(YSC%U%XTOWN(:)   > 0.0)
YSC%U%NSIZE_WATER     = COUNT(YSC%U%XWATER(:)  > 0.0)
YSC%U%NSIZE_SEA       = COUNT(YSC%U%XSEA(:)    > 0.0)
!
ALLOCATE(YSC%U%NR_NATURE (YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%U%NR_TOWN   (YSC%U%NSIZE_TOWN  ))
ALLOCATE(YSC%U%NR_WATER  (YSC%U%NSIZE_WATER ))
ALLOCATE(YSC%U%NR_SEA    (YSC%U%NSIZE_SEA   ))
!
IF (YSC%U%NSIZE_SEA   >0)CALL GET_1D_MASK( YSC%U%NSIZE_SEA,    YSC%U%NSIZE_FULL, YSC%U%XSEA   , YSC%U%NR_SEA   )
IF (YSC%U%NSIZE_WATER >0)CALL GET_1D_MASK( YSC%U%NSIZE_WATER,  YSC%U%NSIZE_FULL, YSC%U%XWATER , YSC%U%NR_WATER )
IF (YSC%U%NSIZE_TOWN  >0)CALL GET_1D_MASK( YSC%U%NSIZE_TOWN,   YSC%U%NSIZE_FULL, YSC%U%XTOWN  , YSC%U%NR_TOWN  )
IF (YSC%U%NSIZE_NATURE>0)CALL GET_1D_MASK( YSC%U%NSIZE_NATURE, YSC%U%NSIZE_FULL, YSC%U%XNATURE, YSC%U%NR_NATURE)
!


!* number of shortwave spectral bands
ISWB=SIZE(PSW_BANDS)
!
!* tile number
ALLOCATE(ZFRAC_TILE(YSC%U%NSIZE_FULL,NTILESFC))
JTILE = 0
!
!
!*       5.     Default values
!               --------------
!
ZDIR_ALB_TILE = XUNDEF
ZSCA_ALB_TILE = XUNDEF
ZEMIS_TILE    = XUNDEF
ZTSRAD_TILE   = XUNDEF
ZTSURF_TILE   = XUNDEF
!
!
!*       6.     Initialization of sea
!               ---------------------
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XSEA(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_SEA,YSC%U%NR_SEA)
!
! initialization
!/EDIT- T BRIVOAL

! Get the lat-lon values where SEA exists
ALLOCATE(YSC%SM%SG%XLAT(YSC%U%NSIZE_SEA))
ALLOCATE(YSC%SM%SG%XLON(YSC%U%NSIZE_SEA))
ALLOCATE(YSC%SM%SG%XMESH_SIZE(YSC%U%NSIZE_SEA))

JLOOP1=1
DO JJ = 1, YSC%U%NSIZE_FULL
  IF (YSC%U%XSEA(JJ)    > 0.0) THEN
    
	YSC%SM%SG%XLAT(JLOOP1)=YSC%UG%XLAT(JJ)
	YSC%SM%SG%XLON(JLOOP1)=YSC%UG%XLON(JJ)
	YSC%SM%SG%XMESH_SIZE(JLOOP1)=YSC%UG%XMESH_SIZE(JJ)
         JLOOP1=JLOOP1+1
  ENDIF
ENDDO

!/

IF (YSC%U%NDIM_SEA>0) &
  CALL INIT_SEA_n(YSC%DTCO, YSC%DGU, YSC%UG, YSC%U, &
                  YSC%SM, YSC%DGL,YSC%GCP,  &
                  HPROGRAM,HINIT,YSC%U%NSIZE_SEA,KSV,KSW,            &
                  HSV,ZP_CO2,ZP_RHOA,                                &
                  ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                  ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                  KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                  'OK'                                               )  
!

!

 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_SEA,YSC%U%NR_SEA)  
!
!
!*       7.     Initialization of lakes
!               -----------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XWATER(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_WATER,YSC%U%NR_WATER)
!
! initialization
!/EDIT- T BRIVOAL

! Get the lat-lon values where WATER exists
ALLOCATE(YSC%FM%FG%XLAT(YSC%U%NSIZE_WATER))
ALLOCATE(YSC%FM%FG%XLON(YSC%U%NSIZE_WATER))
ALLOCATE(YSC%FM%FG%XMESH_SIZE(YSC%U%NSIZE_WATER))
ALLOCATE(YSC%wM%wG%XLAT(YSC%U%NSIZE_WATER))
ALLOCATE(YSC%wM%wG%XLON(YSC%U%NSIZE_WATER))
ALLOCATE(YSC%wM%wG%XMESH_SIZE(YSC%U%NSIZE_WATER))
JLOOP1=1
DO JJ = 1, YSC%U%NSIZE_FULL
  IF (YSC%U%XWATER(JJ)    > 0.0) THEN
    
	YSC%FM%FG%XLAT(JLOOP1)=YSC%UG%XLAT(JJ)
	YSC%FM%FG%XLON(JLOOP1)=YSC%UG%XLON(JJ)
	YSC%FM%FG%XMESH_SIZE(JLOOP1)=YSC%UG%XMESH_SIZE(JJ)
	YSC%WM%WG%XLAT(JLOOP1)=YSC%UG%XLAT(JJ)
	YSC%WM%WG%XLON(JLOOP1)=YSC%UG%XLON(JJ)
	YSC%WM%WG%XMESH_SIZE(JLOOP1)=YSC%UG%XMESH_SIZE(JJ)
         JLOOP1=JLOOP1+1
  ENDIF
ENDDO
!/

IF (YSC%U%NDIM_WATER>0) &
  CALL INIT_INLAND_WATER_n(YSC%DTCO, YSC%DGU,YSC%UG, &
                           YSC%U, YSC%WM, YSC%FM, YSC%DGL,    &
                           HPROGRAM,HINIT,YSC%U%NSIZE_WATER,KSV,KSW,          &
                           HSV,ZP_CO2,ZP_RHOA,                                &
                           ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                           ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                           KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                           'OK'                                               )
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_WATER,YSC%U%NR_WATER)
!
!
!*       8.     Initialization of vegetation scheme
!               -----------------------------------
!
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XNATURE(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_NATURE,YSC%U%NR_NATURE)
!
! initialization
!/EDIT- T BRIVOAL

! Get the lat-lon values where NATURE exists
ALLOCATE(YSC%IM%IG%XLAT(YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%IM%IG%XLON(YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%IM%IG%XMESH_SIZE(YSC%U%NSIZE_NATURE))
ALLOCATE(YSC%IM%I%XZ0EFFJPDIR(YSC%U%NSIZE_NATURE))

JLOOP1=1
DO JJ = 1, YSC%U%NSIZE_FULL
  IF (YSC%U%XNATURE(JJ)    > 0.0) THEN
    
	YSC%IM%IG%XLAT(JLOOP1)=YSC%UG%XLAT(JJ)
	YSC%IM%IG%XLON(JLOOP1)=YSC%UG%XLON(JJ)
	YSC%IM%IG%XMESH_SIZE(JLOOP1)=YSC%UG%XMESH_SIZE(JJ)
	YSC%IM%I%XZ0EFFJPDIR(JLOOP1)=YSC%USS%XZ0EFFJPDIR(JJ)
         JLOOP1=JLOOP1+1
  ENDIF
ENDDO
!/

IF (YSC%U%NDIM_NATURE>0) &
  CALL INIT_NATURE_n(YSC%DTCO, YSC%DGU, YSC%UG, YSC%U, YSC%IM, &
                     YSC%DTZ, YSC%DGL, YSC%DST, YSC%SLT, YSC%SV,YSC%GCP, &
                     HPROGRAM,HINIT,OLAND_USE,YSC%U%NSIZE_NATURE,KSV,KSW,   &
                     HSV,ZP_CO2,ZP_RHOA,                                &
                     ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                     ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                     KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                     'OK'                                               )
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_NATURE,YSC%U%NR_NATURE)  
!
!
!*       9.     Initialization of urban scheme
!               ------------------------------
!
JTILE               = JTILE + 1
ZFRAC_TILE(:,JTILE) = YSC%U%XTOWN(:)
!
! pack variables which are arguments to this routine
 CALL PACK_SURF_INIT_ARG(YSC%U%NSIZE_TOWN,YSC%U%NR_TOWN)
!
! initialization

IF (YSC%U%NDIM_TOWN>0) &
  CALL INIT_TOWN_n(YSC%DTCO, YSC%DGU, YSC%UG, YSC%U, &
                   YSC%IM%CHI, YSC%IM%DTI, YSC%IM%I, &
                   YSC%TM, YSC%GDM, YSC%GRM, YSC%DGL, YSC%DST, YSC%SLT, &
                   YSC%GCP,HPROGRAM,HINIT,YSC%U%NSIZE_TOWN,KSV,KSW,     &
                   HSV,ZP_CO2,ZP_RHOA,                                &
                   ZP_ZENITH,ZP_AZIM,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB, &
                   ZP_EMIS,ZP_TSRAD,ZP_TSURF,                         &
                   KYEAR,KMONTH,KDAY,PTIME, HATMFILE,HATMFILETYPE,    &
                   'OK'                                               )  
!
!
 CALL UNPACK_SURF_INIT_ARG(JTILE,YSC%U%NSIZE_TOWN,YSC%U%NR_TOWN)  
!
!
!
!*      10.     Output radiative and physical fields
!               ------------------------------------
!


IF (SIZE(PDIR_ALB)>0)                                                   &
  CALL AVERAGE_RAD(ZFRAC_TILE,                                            &
                   ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTSRAD_TILE, &
                   PDIR_ALB,      PSCA_ALB,      PEMIS,      PTSRAD       ) 
!
IF (SIZE(PTSURF)>0) &
  CALL AVERAGE_TSURF(ZFRAC_TILE, ZTSURF_TILE, PTSURF)
!                  
DEALLOCATE(ZFRAC_TILE)
!
!-------------------------------------------------------------------------------
!==============================================================================
IF (LHOOK) CALL DR_HOOK('INIT_SURF_ATM_N',1,ZHOOK_HANDLE)
 CONTAINS
!==============================================================================
SUBROUTINE PACK_SURF_INIT_ARG(KSIZE,KMASK)
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! input arguments:
!
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
ALLOCATE(ZP_CO2          (KSIZE))
ALLOCATE(ZP_RHOA         (KSIZE))
ALLOCATE(ZP_ZENITH       (KSIZE))
ALLOCATE(ZP_AZIM         (KSIZE))
!
!
! output arguments:
!
ALLOCATE(ZP_DIR_ALB(KSIZE,ISWB))
ALLOCATE(ZP_SCA_ALB(KSIZE,ISWB))
ALLOCATE(ZP_EMIS   (KSIZE))
ALLOCATE(ZP_TSRAD  (KSIZE))
ALLOCATE(ZP_TSURF  (KSIZE))
!
IF (KSIZE>0) THEN
  ZP_CO2    = 6.E-4
  ZP_RHOA   = 1.2
  ZP_ZENITH = 0.
  ZP_AZIM   = 0.
  ZP_DIR_ALB = XUNDEF
  ZP_SCA_ALB = XUNDEF
  ZP_EMIS    = XUNDEF
  ZP_TSRAD   = XUNDEF
  ZP_TSURF   = XUNDEF
END IF
!
DO JJ=1,KSIZE
IF (SIZE(PCO2)>0) &
     ZP_CO2   (JJ)     = PCO2        (KMASK(JJ))  
IF (SIZE(PRHOA)>0) &
     ZP_RHOA  (JJ)     = PRHOA       (KMASK(JJ))  
IF (SIZE(PZENITH)>0) THEN
    IF (LZENITH) THEN
       ZP_ZENITH(JJ)     = PZENITH     (KMASK(JJ)) 
    ELSE
       ZP_ZENITH(JJ)     = ZZENITH     (KMASK(JJ)) 
    ENDIF
ENDIF
IF (SIZE(PAZIM  )>0) THEN
    IF (LZENITH) THEN
       ZP_AZIM  (JJ)     = PAZIM       (KMASK(JJ)) 
    ELSE
       ZP_AZIM  (JJ)     = ZAZIM       (KMASK(JJ)) 
    ENDIF
ENDIF
ENDDO
IF (LHOOK) CALL DR_HOOK('PACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!


END SUBROUTINE PACK_SURF_INIT_ARG
!==============================================================================
SUBROUTINE UNPACK_SURF_INIT_ARG(KTILE,KSIZE,KMASK)
!
INTEGER, INTENT(IN) :: KTILE, KSIZE
!
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
!
INTEGER :: JJ   ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',0,ZHOOK_HANDLE)
DO JJ=1,KSIZE
IF (SIZE(ZTSRAD_TILE)>0) &
     ZTSRAD_TILE  (KMASK(JJ),KTILE)  = ZP_TSRAD     (JJ)  
IF (SIZE(ZDIR_ALB_TILE)>0) &
     ZDIR_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_DIR_ALB   (JJ,:)  
IF (SIZE(ZSCA_ALB_TILE)>0) &
     ZSCA_ALB_TILE(KMASK(JJ),:,KTILE)= ZP_SCA_ALB   (JJ,:)  
IF (SIZE(ZEMIS_TILE)>0) &
     ZEMIS_TILE   (KMASK(JJ),KTILE)  = ZP_EMIS      (JJ)
IF (SIZE(ZTSURF_TILE)>0) &
     ZTSURF_TILE  (KMASK(JJ),KTILE)  = ZP_TSURF     (JJ)
ENDDO
!
DEALLOCATE(ZP_CO2    )
DEALLOCATE(ZP_RHOA   )
DEALLOCATE(ZP_ZENITH )
DEALLOCATE(ZP_AZIM   )
DEALLOCATE(ZP_DIR_ALB)
DEALLOCATE(ZP_SCA_ALB)
DEALLOCATE(ZP_EMIS   )
DEALLOCATE(ZP_TSRAD  )
DEALLOCATE(ZP_TSURF  )
IF (LHOOK) CALL DR_HOOK('UNPACK_SURF_INIT_ARG',1,ZHOOK_HANDLE)
!
END SUBROUTINE UNPACK_SURF_INIT_ARG
!==============================================================================
!
END SUBROUTINE INIT_SURF_ATM_n
