!depfile:put_sfxcpln.D
!auto_modi:put_sfxcpl_n.D
MODULE MODI_PUT_SFXCPL_n 
INTERFACE
SUBROUTINE PUT_SFXCPL_n (F, I, S, U, W, &
                         HPROGRAM,KI,KSW,PSW_BANDS,PZENITH, &
                         PLAND_WTD,PLAND_FWTD,PLAND_FFLOOD, &
                         PLAND_PIFLOOD,PSEA_SST,PSEA_UCU,   &
                         PSEA_VCU, PSEA_SST2,PSEA_UCU2, PSEA_VCU2, &
                         PSEAICE_SIT,PSEAICE_CVR,  &
                         PSEAICE_ALB,PTSRAD,                &
                         PDIR_ALB,PSCA_ALB,PEMIS,PTSURF,     &
                         PWAVE_CHA,PWAVE_UCU,PWAVE_VCU,      &
                         PWAVE_HS,PWAVE_TP                   )
USE MODD_FLAKE_n, ONLY : FLAKE_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
TYPE(FLAKE_t), INTENT(INOUT) :: F
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
 CHARACTER(LEN=6),        INTENT(IN)  :: HPROGRAM
INTEGER,                 INTENT(IN)  :: KI      ! number of points
INTEGER,                 INTENT(IN)  :: KSW     ! number of bands
REAL, DIMENSION(KI),      INTENT(IN) :: PZENITH
REAL, DIMENSION(KSW),     INTENT(IN) :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_WTD     ! Land water table depth (m)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_FWTD    ! Land grid-cell fraction of water table rise (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_FFLOOD  ! Land Floodplains fraction (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PLAND_PIFLOOD ! Land Potential flood infiltration (kg/m2)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_SST ! Sea surface temperature (K)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_UCU ! Sea u-current stress (Pa)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_VCU ! Sea v-current stress (Pa)

REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_SST2 ! Sea surface temperature (K)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_UCU2 ! Sea u-current stress (Pa)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEA_VCU2 ! Sea v-current stress (Pa)

REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_SIT ! Sea-ice Temperature (K)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_CVR ! Sea-ice cover (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PSEAICE_ALB ! Sea-ice albedo (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PWAVE_CHA ! Charnock coefficient (-)
REAL, DIMENSION(KI),      INTENT(IN) :: PWAVE_UCU ! u-current velocity   (m/s)
REAL, DIMENSION(KI),      INTENT(IN) :: PWAVE_VCU ! v-current velocity   (m/s)
REAL, DIMENSION(KI),      INTENT(IN) :: PWAVE_HS  ! Significant wave height (m)
REAL, DIMENSION(KI),      INTENT(IN) :: PWAVE_TP  ! Peak period (s)
REAL, DIMENSION(KI),     INTENT(OUT) :: PTSRAD   ! Total radiative temperature see by the atmosphere
REAL, DIMENSION(KI),     INTENT(OUT) :: PTSURF   ! Total surface temperature see by the atmosphere
REAL, DIMENSION(KI),     INTENT(OUT) :: PEMIS    ! Total emissivity see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PDIR_ALB ! Total direct albedo see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PSCA_ALB ! Total diffus albedo see by the atmosphere
END SUBROUTINE PUT_SFXCPL_n
END INTERFACE
END MODULE MODI_PUT_SFXCPL_n 
