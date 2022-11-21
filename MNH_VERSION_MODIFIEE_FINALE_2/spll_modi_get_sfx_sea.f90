!depfile:get_sfx_sea.D
!auto_modi:get_sfx_sea.D
MODULE MODI_GET_SFX_SEA 
INTERFACE
      SUBROUTINE GET_SFX_SEA (S, U, W, &
                              OCPL_SEAICE,OWATER,                      &
                              PSEA_FWSU,PSEA_FWSV,PSEA_HEAT,PSEA_SNET, &
                              PSEA_WIND,PSEA_FWSM,PSEA_EVAP,PSEA_RAIN, &
                              PSEA_SNOW,PSEA_EVPR,PSEA_WATF,PSEA_PRES, &
                              PSEAICE_HEAT,PSEAICE_SNET,PSEAICE_EVAP   )  
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(WATFLUX_t), INTENT(INOUT) :: W
LOGICAL,            INTENT(IN)  :: OCPL_SEAICE ! sea-ice / ocean key
LOGICAL,            INTENT(IN)  :: OWATER      ! water included in sea smask
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_FWSU  ! Cumulated zonal wind stress       (Pa.s)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_FWSV  ! Cumulated meridian wind stress    (Pa.s)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_HEAT  ! Cumulated Non solar net heat flux (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_SNET  ! Cumulated Solar net heat flux     (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_WIND  ! Cumulated 10m wind speed          (m)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_FWSM  ! Cumulated wind stress             (Pa.s)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_EVPR  ! Cumulated Evap-Precip             (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_WATF  ! Cumulated Net water flux (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEA_PRES  ! Cumulated Surface pressure        (Pa.s)
REAL, DIMENSION(:), INTENT(OUT) :: PSEAICE_HEAT ! Cumulated Sea-ice non solar net heat flux (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEAICE_SNET ! Cumulated Sea-ice solar net heat flux     (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSEAICE_EVAP ! Cumulated Sea-ice sublimation             (kg/m2)
END SUBROUTINE GET_SFX_SEA
END INTERFACE
END MODULE MODI_GET_SFX_SEA 
