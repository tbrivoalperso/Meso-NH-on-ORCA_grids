!depfile:sfx_oasis_read_nam.D
!auto_modi:sfx_oasis_read_nam.D
MODULE MODI_SFX_OASIS_READ_NAM
INTERFACE
SUBROUTINE SFX_OASIS_READ_NAM(HPROGRAM,PTSTEP_SURF,HINIT)
CHARACTER(LEN=6), INTENT(IN)           :: HPROGRAM    ! program calling surf. schemes
REAL,             INTENT(IN)           :: PTSTEP_SURF ! Surfex time step
CHARACTER(LEN=3), INTENT(IN), OPTIONAL :: HINIT       ! choice of fields to initialize
END SUBROUTINE SFX_OASIS_READ_NAM
END INTERFACE
END MODULE MODI_SFX_OASIS_READ_NAM
