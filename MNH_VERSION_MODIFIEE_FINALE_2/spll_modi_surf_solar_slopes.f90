!depfile:surf_solar_slopes.D
!     ######spl
     MODULE MODI_SURF_SOLAR_SLOPES 
!    #############################
!
INTERFACE 
!
      SUBROUTINE SURF_SOLAR_SLOPES ( PMAP, PXHAT2D, PYHAT2D,                      &
                  PCOSZEN, PSINZEN, PAZIMSOL,                                 &
                  PZS, PZS_XY, PDIRSRFSWD, PDIRSWDT                           )
!
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PMAP         ! map factor
REAL, DIMENSION(:,:),       INTENT(IN) :: PXHAT2D        ! X coordinate
REAL, DIMENSION(:,:),       INTENT(IN) :: PYHAT2D        ! Y coordinate
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN ! COS(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PSINZEN ! SIN(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PAZIMSOL! azimuthal solar angle
!                                               ! (radian, from North, clockwise)
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS    ! (resolved) model orography
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS_XY ! model orography at vort. points
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIRSRFSWD!Downward SuRF. DIRect SW Flux
REAL, DIMENSION(:,:,:,:), INTENT(OUT):: PDIRSWDT ! shortwave flux received by 
!                                                ! each subgrid triangle
!
END SUBROUTINE SURF_SOLAR_SLOPES  
!
END INTERFACE
!
END MODULE MODI_SURF_SOLAR_SLOPES
