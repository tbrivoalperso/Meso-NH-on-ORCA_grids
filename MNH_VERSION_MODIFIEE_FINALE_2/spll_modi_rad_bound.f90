!depfile:rad_bound.D
!     ######spl
MODULE MODI_RAD_BOUND
!####################
!
INTERFACE
!
      SUBROUTINE RAD_BOUND (HLBCX,HLBCY,HTURB, PCARPKMAX,             &
                        PTSTEP,PDXHAT2D,PDYHAT2D,PZHAT,                   &
                        PUT,PVT,                                      &
                        PLBXUM,PLBYVM,PLBXUS,PLBYVS,                  &
                        PCPHASE,PCPHASE_PBL,PRHODJ,                   &
                        PTKET,PRUS,PRVS,PRWS                          )
! 
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
CHARACTER(LEN=4),               INTENT(IN) :: HTURB         ! Turbulence scheme
!
!
REAL,                     INTENT(INOUT) :: PCARPKMAX    ! Rayleigh damping amplitude
REAL,                     INTENT(IN) :: PTSTEP      ! time step dt 
REAL,      DIMENSION(:,:),  INTENT(IN) :: PDXHAT2D      ! X-direc. meshlength T- BRIVOAL
REAL,      DIMENSION(:,:),  INTENT(IN) :: PDYHAT2D      ! Y-direc. meshlength T- BRIVOAL
REAL,      DIMENSION(:),  INTENT(IN) :: PZHAT       ! height level without orography
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PUT,PVT     ! at t
!
! Lateral Boundary fields at time t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PLBXUM,PLBYVM    
! temporal derivative of the Lateral Boundary fields 
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PLBXUS,PLBYVS
!
REAL,                     INTENT(IN) :: PCPHASE     ! prescribed phase velocity
REAL,                     INTENT(IN) :: PCPHASE_PBL ! prescribed PBL phase velocity
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODJ      ! Jacobian * dry density 
                                                    ! of the reference state 
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PTKET ! TKE at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT):: PRUS,PRVS   ! Horizontal and Vertical
REAL, DIMENSION(:,:,:),   INTENT(INOUT):: PRWS        ! momentum tendencies
!
END SUBROUTINE RAD_BOUND
!
END INTERFACE
!
END MODULE MODI_RAD_BOUND
