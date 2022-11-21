!depfile:tridz.D
!     ######spl
      MODULE MODI_TRIDZ
!     ################
!
INTERFACE
!
      SUBROUTINE TRIDZ(HLUOUT,HLBCX,HLBCY,                              &
                      PMAP,PDXHAT2D,PDYHAT2D,PDXHATM,PDYHATM,PRHOM,         &
                      PAF,PCF,PTRIGSX,PTRIGSY,KIFAXX,KIFAXY,            &
                      PRHODJ,PTHVREF,PZZ,PBFY,PBFB,&
                      PBF_SXP2_YP1_Z)!JUAN Z_SPLITING
!
IMPLICIT NONE
!
CHARACTER (LEN=*),               INTENT(IN) :: HLUOUT   ! output listing name
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX    ! x-direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY    ! y-direction LBC type
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ     ! density of reference * J
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHVREF    ! Virtual Potential
                                        ! Temperature of the reference state
!
REAL, DIMENSION(:,:), INTENT(IN) :: PMAP         ! scale factor
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZZ        ! height z
!
REAL, DIMENSION(:,:), INTENT(IN) :: PDXHAT2D         ! Stretching in x direction
REAL, DIMENSION(:,:), INTENT(IN) :: PDYHAT2D          ! Stretching in y direction
!
REAL, INTENT(OUT) :: PDXHATM                     ! mean grid increment in the x
                                                 ! direction
REAL, INTENT(OUT) :: PDYHATM                     ! mean grid increment in the y
                                                 ! direction
!
REAL, DIMENSION (:), INTENT(OUT) :: PRHOM        !  mean of XRHODJ on the plane
                                                 !  x y localized at a mass 
                                                 !  level
!
REAL, DIMENSION(:), INTENT(OUT)     :: PAF,PCF  ! vectors giving the nonvanishing
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBFY     ! elements (yslice) of the tri-diag.
                                                ! matrix in the pressure eq. 
!JUAN
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBFB     ! elements (bsplit slide) of the tri-diag.
                                                ! matrix in the pressure eq. 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBF_SXP2_YP1_Z ! elements of the tri-diag. SXP2_YP1_Z-slide 
                                                   ! matrix in the pressure eq
!JUAN
!
                                                 ! arrays of sin or cos values
                                                 ! for the FFT :
REAL, DIMENSION(:), INTENT(OUT) :: PTRIGSX       ! - along x
REAL, DIMENSION(:), INTENT(OUT) :: PTRIGSY       ! - along y
!
                                                 ! decomposition in prime
                                                 ! numbers for the FFT:
INTEGER, DIMENSION(19), INTENT(OUT) :: KIFAXX      ! - along x
INTEGER, DIMENSION(19), INTENT(OUT) :: KIFAXY      ! - along y

!
END SUBROUTINE TRIDZ
!
END INTERFACE
!
END MODULE MODI_TRIDZ
