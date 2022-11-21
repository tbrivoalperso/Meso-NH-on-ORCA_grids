!depfile:sfx_oasis_send.D
!auto_modi:sfx_oasis_send.D
MODULE MODI_SFX_OASIS_SEND
INTERFACE
SUBROUTINE SFX_OASIS_SEND(KLUOUT,KI,KDATE,OSEND_LAND,OSEND_LAKE,OSEND_SEA,OSEND_WAVE,  &
                          PLAND_RUNOFF,PLAND_DRAIN,PLAND_CALVING,PLAND_RECHARGE,&
                          PLAND_SRCFLOOD,                                       &
                          PLAKE_EVAP,PLAKE_RAIN,PLAKE_SNOW,PLAKE_WATF,          &
                          PSEA_FWSU,PSEA_FWSV,PSEA_HEAT,PSEA_SNET,PSEA_WIND,    &
                          PSEA_FWSM,PSEA_EVAP,PSEA_RAIN,PSEA_SNOW,PSEA_EVPR,    &
                          PSEA_WATF,PSEA_PRES,PSEAICE_HEAT,PSEAICE_SNET,        &
                          PSEAICE_EVAP,PWAVE_U10,PWAVE_V10      )
INTEGER,             INTENT(IN) :: KLUOUT
INTEGER,             INTENT(IN) :: KI            ! number of points
INTEGER,             INTENT(IN) :: KDATE  ! current coupling time step (s)
LOGICAL,             INTENT(IN) :: OSEND_LAND
LOGICAL,             INTENT(IN) :: OSEND_LAKE
LOGICAL,             INTENT(IN) :: OSEND_SEA
LOGICAL,             INTENT(IN) :: OSEND_WAVE
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_RUNOFF    ! Cumulated Surface runoff             (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_DRAIN     ! Cumulated Deep drainage              (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_CALVING   ! Cumulated Calving flux               (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_RECHARGE  ! Cumulated Recharge to groundwater    (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_SRCFLOOD  ! Cumulated flood freshwater flux      (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_EVAP  ! Cumulated Evaporation              (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_RAIN  ! Cumulated Rainfall rate            (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_SNOW  ! Cumulated Snowfall rate            (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_WATF  ! Cumulated freshwater flux          (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSU  ! Cumulated zonal wind stress       (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSV  ! Cumulated meridian wind stress    (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_HEAT  ! Cumulated Non solar net heat flux (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_SNET  ! Cumulated Solar net heat flux     (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_WIND  ! Cumulated 10m wind speed          (m)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSM  ! Cumulated wind stress             (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_EVPR  ! Evap. - Precip. rate              (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_WATF  ! Cumulated freshwater flux         (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_PRES  ! Cumulated Surface pressure        (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_HEAT ! Cumulated Sea-ice non solar net heat flux (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_SNET ! Cumulated Sea-ice solar net heat flux     (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_EVAP ! Cumulated Sea-ice sublimation             (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PWAVE_U10  ! 
REAL, DIMENSION(KI), INTENT(IN) :: PWAVE_V10  !
END SUBROUTINE SFX_OASIS_SEND
END INTERFACE
END MODULE MODI_SFX_OASIS_SEND
