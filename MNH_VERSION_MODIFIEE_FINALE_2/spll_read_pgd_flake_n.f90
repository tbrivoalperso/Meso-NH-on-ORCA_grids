!depfile:read_pgd_flaken.D
!     ######spl
      SUBROUTINE READ_PGD_FLAKE_n (DTCO, U, FG, F, &
                                   HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_FLAKE_n* - read FLAKE physiographic fields
!!                        
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_FLAKE_GRID_n, ONLY : FLAKE_GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER


!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_SURF
USE MODI_READ_GRID
USE MODI_READ_LCOVER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(FLAKE_GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
! 
 CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be read
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_FLAKE_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'WATER ',FG%NDIM)
!
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(F%LCOVER(JPCOVER))
 CALL READ_LCOVER(&
                  HPROGRAM,F%LCOVER)
!
ALLOCATE(F%XCOVER(FG%NDIM,JPCOVER))
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',F%XCOVER(:,:),F%LCOVER,IRESP)
!
!* orography
!
ALLOCATE(F%XZS(FG%NDIM))
YRECFM='ZS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XZS(:),IRESP)
!
!* latitude, longitude 
!
!ALLOCATE(FG%XLAT      (FG%NDIM))
!ALLOCATE(FG%XLON      (FG%NDIM))
!ALLOCATE(FG%XMESH_SIZE(FG%NDIM))
 CALL READ_GRID(&
                HPROGRAM,FG%CGRID,FG%XGRID_PAR,FG%XLAT,FG%XLON,FG%XMESH_SIZE,IRESP)
!
!* FLake parameters
!
ALLOCATE(F%XWATER_DEPTH   (FG%NDIM))
YRECFM='WATER_DEPTH'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XWATER_DEPTH(:),IRESP)
!
ALLOCATE(F%XWATER_FETCH   (FG%NDIM))
YRECFM='WATER_FETCH'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XWATER_FETCH(:),IRESP)
!
ALLOCATE(F%XT_BS          (FG%NDIM))
YRECFM='T_BS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XT_BS(:),IRESP)
!
ALLOCATE(F%XDEPTH_BS      (FG%NDIM))
YRECFM='DEPTH_BS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XDEPTH_BS(:),IRESP)
!
ALLOCATE(F%XEXTCOEF_WATER (FG%NDIM))
YRECFM='EXTCOEF_WAT'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,F%XEXTCOEF_WATER(:),IRESP)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_FLAKE_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_FLAKE_n
