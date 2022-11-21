!depfile:ground_paramn.D
!     ######spl
MODULE MODI_GROUND_PARAM_n
!     ##########
!
INTERFACE 
!
      SUBROUTINE GROUND_PARAM_n( PSFTH, PSFRV, PSFSV, PSFCO2, PSFU, PSFV, &
                                 PSEA_UCU,PSEA_VCU,                       &
                                 PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD        )
!
!* surface fluxes
!  --------------
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSFTH ! surface flux of potential temperature (Km/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSFRV ! surface flux of water vapor           (m/s*kg/kg)
REAL, DIMENSION(:,:,:),INTENT(OUT):: PSFSV ! surface flux of scalar                (m/s*kg/kg)
                                           ! flux of chemical var.                 (ppp.m/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSFCO2! surface flux of CO2                   (m/s*kg/kg)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSFU  ! surface fluxes of horizontal   
REAL, DIMENSION(:,:), INTENT(OUT) :: PSFV  ! momentum in x and y directions        (m2/s2)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSEA_UCU,PSEA_VCU !Sea sfc currents  (m/s)
!
!* Radiative parameters
!  --------------------
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! direct  albedo for each spectral band (-)
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:,:),   INTENT(OUT) :: PEMIS     ! surface emissivity                    (-)
REAL, DIMENSION(:,:),   INTENT(OUT) :: PTSRAD    ! surface radiative temperature         (K)
!
END SUBROUTINE GROUND_PARAM_n
!
END INTERFACE
!
END MODULE MODI_GROUND_PARAM_n
