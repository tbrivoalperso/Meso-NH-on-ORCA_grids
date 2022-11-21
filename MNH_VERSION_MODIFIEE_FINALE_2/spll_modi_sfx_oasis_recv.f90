!depfile:sfx_oasis_recv.D
!auto_modi:sfx_oasis_recv.D
MODULE MODI_SFX_OASIS_RECV
INTERFACE
SUBROUTINE SFX_OASIS_RECV(HPROGRAM,KI,KSW,PTIMEC,                &
                          ORECV_LAND, ORECV_SEA, ORECV_WAVE,     &
                          PLAND_WTD,PLAND_FWTD,                  &
                          PLAND_FFLOOD,PLAND_PIFLOOD,            &
                          PSEA_SST,PSEA_UCU,PSEA_VCU,            &
                          PSEA_SST2,PSEA_UCU2,PSEA_VCU2,            &
                          PSEAICE_SIT,PSEAICE_CVR,PSEAICE_ALB,   &
                          PWAVE_CHA,PWAVE_UCU,PWAVE_VCU,         &
                          PWAVE_HS,PWAVE_TP             )
CHARACTER(LEN=*),       INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                INTENT(IN)  :: KI        ! number of points on this proc
INTEGER,                INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,                   INTENT(IN)  :: PTIMEC    ! Cumulated run time step (s)
LOGICAL,                INTENT(IN)  :: ORECV_LAND
LOGICAL,                INTENT(IN)  :: ORECV_SEA
LOGICAL,                INTENT(IN)  :: ORECV_WAVE
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_WTD     ! Land water table depth (m)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_FWTD    ! Land grid-cell fraction of water table rise (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_FFLOOD  ! Land Floodplains fraction (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_PIFLOOD ! Land Potential flood infiltration (kg/m2/s)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_SST ! Sea surface temperature (K)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_UCU ! Sea u-current stress (Pa)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_VCU ! Sea v-current stress (Pa)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_SST2 ! Sea surface temperature 2 (K) !TBRIVOAL
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_UCU2 ! Sea u-current stress 2(Pa) ! TBRIVOAL
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_VCU2 ! Sea v-current stress 2(Pa) ! TBRIVOAL
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_SIT ! Sea-ice Temperature (K)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_CVR ! Sea-ice cover (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_ALB ! Sea-ice albedo (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_CHA ! Charnock coefficient (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_UCU ! u-current velocity (m/s)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_VCU ! v-current velocity (m/s)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_HS  ! Significant wave height (m)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_TP  ! Peak period (s)
END SUBROUTINE SFX_OASIS_RECV
END INTERFACE
END MODULE MODI_SFX_OASIS_RECV
