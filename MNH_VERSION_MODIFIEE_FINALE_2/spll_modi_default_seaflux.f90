!depfile:default_seaflux.D
!auto_modi:default_seaflux.D
MODULE MODI_DEFAULT_SEAFLUX
INTERFACE
      SUBROUTINE DEFAULT_SEAFLUX(PTSTEP,POUT_TSTEP,HSEA_ALB,HSEA_FLUX,   &
                                   OPWG, OPRECIP, OPWEBB, KZ0, KGRVWAVES,&
                                   OPROGSST, KTIME_COUPLING,POCEAN_TSTEP,&
                                   PICHCE, HINTERPOL_SST, HINTERPOL_SSS, &
                                   OWAVEWIND     )
REAL,              INTENT(OUT) :: PTSTEP        ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP    ! time step for writing
CHARACTER(LEN=6),  INTENT(OUT) :: HSEA_FLUX     ! type of sea scheme
CHARACTER(LEN=4),  INTENT(OUT) :: HSEA_ALB      ! type of sea albedo
LOGICAL,           INTENT(OUT) :: OPWG          ! gustiness impact
LOGICAL,           INTENT(OUT) :: OPRECIP       ! precipitation correction
LOGICAL,           INTENT(OUT) :: OPWEBB        ! Webb correction
INTEGER,           INTENT(OUT) :: KZ0           ! PZ0SEA formulation
INTEGER,           INTENT(OUT) :: KGRVWAVES     ! Wave gravity in roughness length
LOGICAL,           INTENT(OUT) :: OPROGSST      !two-way 
INTEGER,           INTENT(OUT) :: KTIME_COUPLING!coupling frequency
REAL,              INTENT(OUT) :: PICHCE        !CE coef calculation for ECUME
REAL,              INTENT(OUT) :: POCEAN_TSTEP  !ocean 1D model time-step
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SST ! Quadratic interpolation of monthly SST
CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SSS ! Quadratic interpolation of monthly SSS
LOGICAL,           INTENT(OUT) :: OWAVEWIND     ! wave parameters from wind only
END SUBROUTINE DEFAULT_SEAFLUX
END INTERFACE
END MODULE MODI_DEFAULT_SEAFLUX
