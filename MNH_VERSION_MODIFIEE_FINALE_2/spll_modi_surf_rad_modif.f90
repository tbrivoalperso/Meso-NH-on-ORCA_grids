!depfile:surf_rad_modif.D
!     ######spl
     MODULE MODI_SURF_RAD_MODIF 
!    ##########################
!
INTERFACE 
!
      SUBROUTINE SURF_RAD_MODIF ( PMAP, PXHAT2D, PYHAT2D,            &
                  PCOSZEN, PSINZEN, PAZIMSOL,PZS,PZS_XY,         &
                  PDIRFLASWD, PDIRSRFSWD                         )
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PMAP       ! map factor
REAL, DIMENSION(:,:),       INTENT(IN) :: PXHAT2D      ! X coordinate
REAL, DIMENSION(:,:),       INTENT(IN) :: PYHAT2D      ! Y coordinate
REAL, DIMENSION(:,:),     INTENT(IN) :: PCOSZEN    ! COS(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PSINZEN    ! SIN(zenithal solar angle)
REAL, DIMENSION(:,:),     INTENT(IN) :: PAZIMSOL   ! azimuthal solar angle
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS        ! (resolved) model orography
REAL, DIMENSION(:,:),     INTENT(IN) :: PZS_XY     ! orography at vort. points
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PDIRFLASWD ! Downward DIR SW Flux on flat surf
REAL, DIMENSION(:,:,:),   INTENT(OUT):: PDIRSRFSWD ! Downward SuRF. DIRect    SW Flux
!
END SUBROUTINE SURF_RAD_MODIF  
!
END INTERFACE
!
END MODULE MODI_SURF_RAD_MODIF
