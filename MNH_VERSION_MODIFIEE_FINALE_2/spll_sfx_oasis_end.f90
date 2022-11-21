!depfile:sfx_oasis_end.D
!     ######spl
SUBROUTINE SFX_OASIS_END
!########################
!
!!****  *SFX_OASIS_END* - end coupling SFX - OASIS
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SFX_OASIS, ONLY : LOASIS
!

USE MOD_OASIS

!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: IERR   ! Error info
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
IF(LOASIS)THEN !Same test than in offline.F90 because use for Arpege-Aladin-Arome
  CALL OASIS_TERMINATE(IERR)
  IF (IERR/=OASIS_OK) THEN
     WRITE(*,'(A)'   )'Error OASIS terminate'
     WRITE(*,'(A,I4)')'Return code from oasis_terminate : ',IERR
     CALL ABORT
     STOP
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_END
