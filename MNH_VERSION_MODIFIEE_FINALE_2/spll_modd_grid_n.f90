!depfile:modd_gridn.D
!     ######spl
      MODULE MODD_GRID_n
!     ##################
!
!!****  *MODD_GRID$n* - declaration of grid variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the variables
!     describing the grid. 
!    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_GRIDn)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/05/94                      
!!      J. Stein    15/11/95  add the slope angle
!!      V. Ducrocq   13/08/98  // : add XLATOR_ll and XLONOR_ll       
!!      V. Masson   nov 2004  supress XLATOR,XLONOR,XLATOR_ll,XLONOR_ll
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE GRID_t
  REAL, DIMENSION(:,:), POINTER :: XLON=>NULL(),XLAT=>NULL() ! Longitude and latitude  
  REAL, DIMENSION(:,:), POINTER :: XLON_ll=>NULL(),XLAT_ll=>NULL() ! Longitude and latitude 
!(Whole domain)  
!
  REAL, DIMENSION(:),   POINTER :: XXHAT=>NULL()   ! Position x in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:),   POINTER :: XYHAT=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:,:),   POINTER :: XXHAT2D=>NULL()   ! Position x in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:,:),   POINTER :: XYHAT2D=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane


  REAL, DIMENSION(:,:),   POINTER :: xlonv=>NULL()   ! Longitude at v point
  REAL, DIMENSION(:,:),   POINTER :: xlatv=>NULL()   ! Latitude at v point
  REAL, DIMENSION(:,:),   POINTER :: xlonu=>NULL()   ! Longitude at u point
  REAL, DIMENSION(:,:),   POINTER :: xlatu=>NULL()   ! Latitude at u point
  REAL, DIMENSION(:,:),   POINTER :: xlont=>NULL()   ! Longitude at t point
  REAL, DIMENSION(:,:),   POINTER :: xlatt=>NULL()   ! Latitude at t point
  REAL, DIMENSION(:,:),   POINTER :: xlatt_ll=>NULL()   ! Longitude at t point (whole domain)
  REAL, DIMENSION(:,:),   POINTER :: xlatu_ll=>NULL()   ! Longitude at v point (whole domain)



  REAL, DIMENSION(:,:),   POINTER :: XDXHAT2D=>NULL()   ! Position x in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:,:),   POINTER :: XDYHAT2D=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:,:),   POINTER :: XDXHAT2D_ll=>NULL()   ! Position x in the 
                                         ! conformal or cartesian plane (all field) t-brivoal
  REAL, DIMENSION(:,:),   POINTER :: XDYHAT2D_ll=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane (all field) t-brivoal

  REAL, DIMENSION(:),   POINTER :: XDXHAT=>NULL()  ! horizontal stretching in x
  REAL, DIMENSION(:),   POINTER :: XDYHAT=>NULL()  ! horizontal stretching in y
  REAL, DIMENSION(:,:), POINTER :: XMAP=>NULL()    ! Map factor 
!
  REAL, DIMENSION(:,:), POINTER :: XMAP_ll=>NULL()    ! Map factor 
  REAL, DIMENSION(:,:),   POINTER :: XZS=>NULL()   ! orography
  REAL, DIMENSION(:,:,:), POINTER :: XZZ=>NULL()   ! height z 
  REAL, DIMENSION(:),     POINTER :: XZHAT=>NULL() ! height level without orography
!
  REAL, DIMENSION(:,:)  , POINTER :: XDIRCOSXW=>NULL(),XDIRCOSYW=>NULL(),XDIRCOSZW=>NULL() 
                                               ! director cosinus of the normal 
                                               ! to the ground surface 
!  
  REAL, DIMENSION(:,:),  POINTER  ::  XCOSSLOPE=>NULL()  ! cosinus of the angle
                                 ! between i and the slope vector
  REAL, DIMENSION(:,:),  POINTER  ::  XSINSLOPE=>NULL()  ! sinus of the angle
                                 ! between i and the slope vector
! quantities for SLEVE vertical coordinate
  LOGICAL                         :: LSLEVE    ! Logical for SLEVE coordinate
  REAL                            :: XLEN1     ! Decay scale for smooth topography
  REAL                            :: XLEN2     ! Decay scale for small-scale topography deviation
  REAL, DIMENSION(:,:),   POINTER :: XZSMT=>NULL()   ! smooth orography for SLEVE coordinate
END TYPE GRID_t

TYPE(GRID_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: GRID_MODEL

REAL, DIMENSION(:,:), POINTER :: XLON=>NULL(),XLAT=>NULL()
REAL, DIMENSION(:,:), POINTER :: XLON_ll=>NULL(),XLAT_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XXHAT2D=>NULL()   ! Position x in the 
                                        ! conformal or cartesian plane
REAL, DIMENSION(:,:),   POINTER :: XYHAT2D=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane

REAL, DIMENSION(:),   POINTER :: XXHAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XYHAT=>NULL()

REAL, DIMENSION(:,:),   POINTER :: xlonv=>NULL()   ! Longitude at v point
REAL, DIMENSION(:,:),   POINTER :: xlatv=>NULL()   ! Latitude at v point
REAL, DIMENSION(:,:),   POINTER :: xlonu=>NULL()   ! Longitude at u point
REAL, DIMENSION(:,:),   POINTER :: xlatu=>NULL()   ! Latitude at u point
REAL, DIMENSION(:,:),   POINTER :: xlont=>NULL()   ! Longitude at t point
REAL, DIMENSION(:,:),   POINTER :: xlatt=>NULL()   ! Latitude at t point
REAL, DIMENSION(:,:),   POINTER :: xlatt_ll=>NULL()   ! Longitude at t point (whole domain)
REAL, DIMENSION(:,:),   POINTER :: xlatu_ll=>NULL()   ! Longitude at v point (whole domain)

REAL, DIMENSION(:),   POINTER :: XDXHAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XDYHAT=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XDXHAT2D=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XDYHAT2D=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XDXHAT2D_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XDYHAT2D_ll=>NULL()
REAL, DIMENSION(:,:), POINTER :: XMAP=>NULL()
REAL, DIMENSION(:,:), POINTER :: XMAP_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XZZ=>NULL()
REAL, DIMENSION(:),     POINTER :: XZHAT=>NULL()
REAL, DIMENSION(:,:)  , POINTER :: XDIRCOSXW=>NULL(),XDIRCOSYW=>NULL(),XDIRCOSZW=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XCOSSLOPE=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XSINSLOPE=>NULL()
LOGICAL,               POINTER  :: LSLEVE=>NULL()
REAL,                  POINTER  :: XLEN1=>NULL()
REAL,                  POINTER  :: XLEN2=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XZSMT=>NULL()

CONTAINS

SUBROUTINE GRID_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
GRID_MODEL(KFROM)%XLON=>XLON
GRID_MODEL(KFROM)%XLAT=>XLAT
GRID_MODEL(KFROM)%XLON_ll=>XLON_ll
GRID_MODEL(KFROM)%XLAT_ll=>XLAT_ll
GRID_MODEL(KFROM)%XXHAT2D=>XXHAT2D
GRID_MODEL(KFROM)%XYHAT2D=>XYHAT2D


GRID_MODEL(KFROM)%xlonu=>xlonu
GRID_MODEL(KFROM)%xlatu=>xlatu
GRID_MODEL(KFROM)%xlonv=>xlonv
GRID_MODEL(KFROM)%xlatv=>xlatv
GRID_MODEL(KFROM)%xlont=>xlont
GRID_MODEL(KFROM)%xlatt=>xlatt
GRID_MODEL(KFROM)%xlatt_ll=>xlatt_ll
GRID_MODEL(KFROM)%xlatu_ll=>xlatu_ll


GRID_MODEL(KFROM)%XXHAT=>XXHAT
GRID_MODEL(KFROM)%XYHAT=>XYHAT
GRID_MODEL(KFROM)%XDXHAT=>XDXHAT
GRID_MODEL(KFROM)%XDYHAT=>XDYHAT
GRID_MODEL(KFROM)%XDXHAT2D=>XDXHAT2D
GRID_MODEL(KFROM)%XDYHAT2D=>XDYHAT2D
GRID_MODEL(KFROM)%XDXHAT2D_ll=>XDXHAT2D_ll
GRID_MODEL(KFROM)%XDYHAT2D_ll=>XDYHAT2D_ll
GRID_MODEL(KFROM)%XMAP=>XMAP
GRID_MODEL(KFROM)%XMAP_ll=>XMAP_ll
GRID_MODEL(KFROM)%XZS=>XZS
GRID_MODEL(KFROM)%XZZ=>XZZ
GRID_MODEL(KFROM)%XZHAT=>XZHAT
GRID_MODEL(KFROM)%XDIRCOSXW=>XDIRCOSXW
GRID_MODEL(KFROM)%XDIRCOSYW=>XDIRCOSYW
GRID_MODEL(KFROM)%XDIRCOSZW=>XDIRCOSZW
GRID_MODEL(KFROM)%XCOSSLOPE=>XCOSSLOPE
GRID_MODEL(KFROM)%XSINSLOPE=>XSINSLOPE
GRID_MODEL(KFROM)%XZSMT=>XZSMT
!
! Current model is set to model KTO
XLON=>GRID_MODEL(KTO)%XLON
XLAT=>GRID_MODEL(KTO)%XLAT
XLON_ll=>GRID_MODEL(KTO)%XLON_ll
XLAT_ll=>GRID_MODEL(KTO)%XLAT_ll
XXHAT2D=>GRID_MODEL(KTO)%XXHAT2D
XYHAT2D=>GRID_MODEL(KTO)%XYHAT2D


XXHAT=>GRID_MODEL(KTO)%XXHAT
XYHAT=>GRID_MODEL(KTO)%XYHAT
XDXHAT=>GRID_MODEL(KTO)%XDXHAT
XDYHAT=>GRID_MODEL(KTO)%XDYHAT
XDXHAT2D=>GRID_MODEL(KTO)%XDXHAT2D
XDYHAT2D=>GRID_MODEL(KTO)%XDYHAT2D
XDXHAT2D_ll=>GRID_MODEL(KTO)%XDXHAT2D_ll
XDYHAT2D_ll=>GRID_MODEL(KTO)%XDYHAT2D_ll

xlonu=>GRID_MODEL(KTO)%xlonu
xlatu=>GRID_MODEL(KTO)%xlatu
xlonv=>GRID_MODEL(KTO)%xlonv
xlatv=>GRID_MODEL(KTO)%xlatv
xlont=>GRID_MODEL(KTO)%xlont
xlatt=>GRID_MODEL(KTO)%xlatt
xlatt_ll=>GRID_MODEL(KTO)%xlatt_ll
xlatu_ll=>GRID_MODEL(KTO)%xlatu_ll

XMAP=>GRID_MODEL(KTO)%XMAP
XMAP_ll=>GRID_MODEL(KTO)%XMAP_ll
XZS=>GRID_MODEL(KTO)%XZS
XZZ=>GRID_MODEL(KTO)%XZZ
XZHAT=>GRID_MODEL(KTO)%XZHAT
XDIRCOSXW=>GRID_MODEL(KTO)%XDIRCOSXW
XDIRCOSYW=>GRID_MODEL(KTO)%XDIRCOSYW
XDIRCOSZW=>GRID_MODEL(KTO)%XDIRCOSZW
XCOSSLOPE=>GRID_MODEL(KTO)%XCOSSLOPE
XSINSLOPE=>GRID_MODEL(KTO)%XSINSLOPE
LSLEVE=>GRID_MODEL(KTO)%LSLEVE
XLEN1=>GRID_MODEL(KTO)%XLEN1
XLEN2=>GRID_MODEL(KTO)%XLEN2
XZSMT=>GRID_MODEL(KTO)%XZSMT

END SUBROUTINE GRID_GOTO_MODEL

END MODULE MODD_GRID_n
