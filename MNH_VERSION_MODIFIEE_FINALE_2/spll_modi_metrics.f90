!depfile:metrics.D
!     ######spl
      MODULE MODI_METRICS
!     ###################
INTERFACE
!
SUBROUTINE METRICS(PMAP,PDXHAT,PDYHAT,PDXHAT2D,PDYHAT2D,PZZ,                            &
                   PDXX,PDYY,PDZX,PDZY,PDZZ)
REAL, DIMENSION(:,:),   INTENT(IN)  :: PMAP    ! Map factor
REAL, DIMENSION(:),     INTENT(IN)  :: PDXHAT  ! Stretching in x direction 
REAL, DIMENSION(:),     INTENT(IN)  :: PDYHAT  ! Stretching in y direction 
REAL, DIMENSION(:,:),     INTENT(IN)  :: PDXHAT2D  ! Stretching in x direction 2D
REAL, DIMENSION(:,:),     INTENT(IN)  :: PDYHAT2D  ! Stretching in y direction 2D
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PZZ     ! Height in z direction
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDXX  ! metric coefficient dxx
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDYY  ! metric coefficient dyy 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDZX  ! metric coefficient dzx 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDZY  ! metric coefficient dzy 
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDZZ  ! metric coefficient dzz  
!
END SUBROUTINE METRICS
!
END INTERFACE
!
END MODULE MODI_METRICS
