!depfile:coare30_flux.D
!auto_modi:coare30_flux.D
MODULE MODI_COARE30_FLUX 
INTERFACE
    SUBROUTINE COARE30_FLUX (S, &
                             PZ0SEA,PTA,PEXNA,PRHOA,PSST,PEXNS,PQA,  &
            PVMOD,PZREF,PUREF,PPS,PQSAT,PSFTH,PSFTQ,PUSTAR,PCD,PCDN,PCH,PCE,PRI,&
            PRESA,PRAIN,PZ0HSEA,PHS,PTP)  
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
REAL, DIMENSION(:), INTENT(IN)       :: PTA   ! air temperature at atm. level (K)
REAL, DIMENSION(:), INTENT(IN)       :: PQA   ! air humidity at atm. level (kg/kg)
REAL, DIMENSION(:), INTENT(IN)       :: PEXNA ! Exner function at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA ! air density at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PVMOD ! module of wind at atm. wind level (m/s)
REAL, DIMENSION(:), INTENT(IN)       :: PZREF ! atm. level for temp. and humidity (m)
REAL, DIMENSION(:), INTENT(IN)       :: PUREF ! atm. level for wind (m)
REAL, DIMENSION(:), INTENT(IN)       :: PSST  ! Sea Surface Temperature (K)
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PRAIN !precipitation rate (kg/s/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PHS   ! wave significant height
REAL, DIMENSION(:), INTENT(IN)       :: PTP   ! wave peak period
REAL, DIMENSION(:), INTENT(INOUT)    :: PZ0SEA! roughness length over the ocean
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTH ! heat flux (W/m2)
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTQ ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)      :: PUSTAR! friction velocity (m/s)
REAL, DIMENSION(:), INTENT(OUT)      :: PQSAT ! humidity at saturation
REAL, DIMENSION(:), INTENT(OUT)      :: PCD   ! heat drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCDN  ! momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCH   ! neutral momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCE  !transfer coef. for latent heat flux
REAL, DIMENSION(:), INTENT(OUT)      :: PRI   ! Richardson number
REAL, DIMENSION(:), INTENT(OUT)      :: PRESA ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0HSEA ! heat roughness length
END SUBROUTINE COARE30_FLUX
END INTERFACE
END MODULE MODI_COARE30_FLUX 
