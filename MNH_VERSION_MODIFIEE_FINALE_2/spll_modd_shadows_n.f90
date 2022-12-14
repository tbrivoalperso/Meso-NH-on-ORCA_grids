!depfile:modd_shadowsn.D
!     ######spl
       MODULE MODD_SHADOWS_n
!      ########################
!
!!****  *MODD_SHADOWS$n* - declaration of parameters for shadows computations
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to define global orographic
!!      features necessary for the computation of shadows by mountains.
!!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!     V. Masson     *Meteo-France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      04/2012
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE SHADOWS_t
!
!
  REAL    :: XZS_MAX_ll ! maximum orography in the domain
!
  REAL, DIMENSION(:,:),         POINTER :: XZS_XY=>NULL()     !  orography at vort. points
  REAL, DIMENSION(:,:),         POINTER :: XZS_ll=>NULL()     !  orography at mass points (all domain)
  REAL, DIMENSION(:,:),         POINTER :: XZS_XY_ll=>NULL()  !  orography at vort. points (all domain)
  REAL, DIMENSION(:),           POINTER :: XXHAT_ll=>NULL()   !  X coordinate (all domain)
  REAL, DIMENSION(:),           POINTER :: XYHAT_ll=>NULL()   !  Y coordinate (all domain)
  REAL, DIMENSION(:,:),           POINTER :: XXHAT2D_ll=>NULL()   !  X coordinate (all domain)
  REAL, DIMENSION(:,:),           POINTER :: XYHAT2D_ll=>NULL()   !  Y coordinate (all domain)
!
!
END TYPE SHADOWS_t

TYPE(SHADOWS_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: SHADOWS_MODEL

REAL, POINTER :: XZS_MAX_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_XY=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_XY_ll=>NULL()
REAL, DIMENSION(:),     POINTER :: XXHAT_ll=>NULL()
REAL, DIMENSION(:),     POINTER :: XYHAT_ll=>NULL()

REAL, DIMENSION(:,:),     POINTER :: XXHAT2D_ll=>NULL()
REAL, DIMENSION(:,:),     POINTER :: XYHAT2D_ll=>NULL()

CONTAINS

SUBROUTINE SHADOWS_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
SHADOWS_MODEL(KFROM)%XZS_XY=>XZS_XY
SHADOWS_MODEL(KFROM)%XZS_ll=>XZS_ll
SHADOWS_MODEL(KFROM)%XZS_XY_ll=>XZS_XY_ll
SHADOWS_MODEL(KFROM)%XXHAT_ll=>XXHAT_ll
SHADOWS_MODEL(KFROM)%XYHAT_ll=>XYHAT_ll
SHADOWS_MODEL(KFROM)%XXHAT2D_ll=>XXHAT2D_ll  ! T- BRIVOAL
SHADOWS_MODEL(KFROM)%XYHAT2D_ll=>XYHAT2D_ll
!
! Current model is set to model KTO
XZS_MAX_ll=>SHADOWS_MODEL(KTO)%XZS_MAX_ll
XZS_XY=>SHADOWS_MODEL(KTO)%XZS_XY
XZS_ll=>SHADOWS_MODEL(KTO)%XZS_ll
XZS_XY_ll=>SHADOWS_MODEL(KTO)%XZS_XY_ll
XXHAT_ll=>SHADOWS_MODEL(KTO)%XXHAT_ll
XYHAT_ll=>SHADOWS_MODEL(KTO)%XYHAT_ll
XXHAT2D_ll=>SHADOWS_MODEL(KTO)%XXHAT2D_ll
XYHAT2D_ll=>SHADOWS_MODEL(KTO)%XYHAT2D_ll

END SUBROUTINE SHADOWS_GOTO_MODEL

END MODULE MODD_SHADOWS_n
