!depfile:surf_solar_sum.D
!     ######spl
      SUBROUTINE SURF_SOLAR_SUM ( PXHAT2D, PYHAT2D, PMAP, PDIRSWD, PENERGY )
!     ##################################################################
!
!!****  * SURF_SOLAR_SUM * - computes the sum of energy received by
!!                           the surface from direct solar radiation
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson      * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
!JUAN
USE MODE_REPRO_SUM
!JUAN
!
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PXHAT2D    ! X coordinate T-BRIVOAL
REAL, DIMENSION(:,:),   INTENT(IN) :: PYHAT2D    ! Y coordinate
REAL, DIMENSION(:,:), INTENT(IN) :: PMAP     ! map factor
REAL, DIMENSION(:,:), INTENT(IN) :: PDIRSWD  ! direct SW flux on hor. surf.
REAL,                 INTENT(OUT):: PENERGY  ! energy received by the surface
!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: JI, JJ
INTEGER :: INFO_ll
!JUAN16
!REAL                               :: ZENERGY  
REAL, ALLOCATABLE, DIMENSION (:,:) :: ZENERGY_2D
!JUAN16
!
!-------------------------------------------------------------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
!-------------------------------------------------------------------------------
!
!*       1.    SUM OF ENERGY FOR THIS PROCESSOR
!              --------------------------------
!
!JUAN16
ALLOCATE(ZENERGY_2D(IIB:IIE,IJB:IJE))
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    ZENERGY_2D(JI,JJ) = PDIRSWD(JI,JJ)*(PXHAT2D(JI+1,JJ)-PXHAT2D(JI,JJ)) &
                                      *(PYHAT2D(JI,JJ+1)-PYHAT2D(JI,JJ)) &
                                      /PMAP(JI,JJ)**2
  END DO
END DO
!
!-------------------------------------------------------------------------------
!
!*       2.    SUM WITH OTHER PROCESSORS
!              -------------------------
!
!CALL REDUCESUM_ll(ZENERGY,INFO_ll)
PENERGY = SUM_DD_R2_ll(ZENERGY_2D)
!JUAN16
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURF_SOLAR_SUM
