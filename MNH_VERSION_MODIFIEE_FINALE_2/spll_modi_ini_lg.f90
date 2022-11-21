!depfile:ini_lg.D
!     ######spl
      MODULE MODI_INI_LG
!     ##################
INTERFACE
!
      SUBROUTINE INI_LG(PXHAT2D,PYHAT2D,PZZ,PSVT,PLBXSVM,PLBYSVM)
!
REAL,DIMENSION(:,:),      INTENT(IN) :: PXHAT2D,PYHAT2D ! Positions x,y in the cartesian plane
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PZZ         ! True altitude of the w grid-point
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVT        ! scalar var. at t
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PLBXSVM,PLBYSVM  ! LB in x and y-dir.
!
END SUBROUTINE INI_LG
!
END INTERFACE
!
END MODULE MODI_INI_LG
