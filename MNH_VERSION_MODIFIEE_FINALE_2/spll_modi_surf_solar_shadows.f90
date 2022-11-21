!depfile:surf_solar_shadows.D
!     ######spl
     MODULE MODI_SURF_SOLAR_SHADOWS 
!    ##############################
!
INTERFACE 
!
      SUBROUTINE SURF_SOLAR_SHADOWS ( PMAP, PXHAT2D, PYHAT2D,                     &
                  PCOSZEN, PSINZEN, PAZIMSOL,                                 &
                  PZS, PZS_XY, PDIRSWDT, PDIRSRFSWD                           )
!
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PMAP         ! map factor
REAL, DIMENSION(:,:),       INTENT(IN) :: PXHAT2D        ! X coordinate
REAL, DIMENSION(:,:),       INTENT(IN) :: PYHAT2D        ! Y coordinate
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN ! COS(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PSINZEN ! SIN(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PAZIMSOL! azimuthal solar angle
                                                ! (radian, from North, clockwise)
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS        ! (resolved) model orography
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS_XY     ! orography at vort. points
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT):: PDIRSWDT   ! SW Flux received by
                                                     ! each subgrid triangle
REAL, DIMENSION(:,:,:),   INTENT(OUT)  :: PDIRSRFSWD ! SuRF. DIRect SW Flux
!
END SUBROUTINE SURF_SOLAR_SHADOWS  
!
END INTERFACE
!
END MODULE MODI_SURF_SOLAR_SHADOWS
