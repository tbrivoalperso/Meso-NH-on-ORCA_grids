!depfile:surf_solar_sum.D
!     ######spl
     MODULE MODI_SURF_SOLAR_SUM 
!    ##########################
!
INTERFACE 
!
      SUBROUTINE SURF_SOLAR_SUM ( PXHAT2D, PYHAT2D, PMAP, PDIRSWD, PENERGY )
!
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PXHAT2D    ! X coordinate
REAL, DIMENSION(:,:),   INTENT(IN) :: PYHAT2D    ! Y coordinate
REAL, DIMENSION(:,:), INTENT(IN) :: PMAP     ! map factor
REAL, DIMENSION(:,:), INTENT(IN) :: PDIRSWD  ! direct SW flux on hor. surf.
REAL,                 INTENT(OUT):: PENERGY  ! energy received by the surface
!
!
END SUBROUTINE SURF_SOLAR_SUM  
!
END INTERFACE
!
END MODULE MODI_SURF_SOLAR_SUM
