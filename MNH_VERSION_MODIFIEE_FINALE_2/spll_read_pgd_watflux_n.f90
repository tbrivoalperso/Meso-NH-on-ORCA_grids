!depfile:read_pgd_watfluxn.D
!     ######spl
      SUBROUTINE READ_PGD_WATFLUX_n (DTCO, U, WG, W, &
                                     HPROGRAM)
!     #########################################
!
!!****  *READ_PGD_WATFLUX_n* - read WATFLUX physiographic fields
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_GRID_n, ONLY : WATFLUX_GRID_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
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
IF (LHOOK) CALL DR_HOOK('READ_PGD_WATFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'WATER ',WG%NDIM)
!
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!* cover classes
!
ALLOCATE(W%LCOVER(JPCOVER))
 CALL READ_LCOVER(&
                  HPROGRAM,W%LCOVER)
!
ALLOCATE(W%XCOVER(WG%NDIM,JPCOVER))
 CALL READ_SURF_COV(&
                    HPROGRAM,'COVER',W%XCOVER(:,:),W%LCOVER,IRESP)
!
!* orography
!
ALLOCATE(W%XZS(WG%NDIM))
YRECFM='ZS'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,W%XZS(:),IRESP)
!
!* latitude, longitude 
!
!ALLOCATE(WG%XLAT      (WG%NDIM))
!ALLOCATE(WG%XLON      (WG%NDIM))
!ALLOCATE(WG%XMESH_SIZE(WG%NDIM))
 CALL READ_GRID(&
                HPROGRAM,WG%CGRID,WG%XGRID_PAR,WG%XLAT,WG%XLON,WG%XMESH_SIZE,IRESP)
IF (LHOOK) CALL DR_HOOK('READ_PGD_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_WATFLUX_n
