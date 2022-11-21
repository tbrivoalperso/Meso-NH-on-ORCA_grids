!depfile:diag_cpl_esm_sea.D
!auto_modi:diag_cpl_esm_sea.D
MODULE MODI_DIAG_CPL_ESM_SEA 
INTERFACE
       SUBROUTINE DIAG_CPL_ESM_SEA (S, &
                                     PTSTEP,PZON10M,PMER10M,PSFU,PSFV,     &
                                      PSWD,PSWU,PGFLUX,PSFTQ,PRAIN,PSNOW, &
                                      PLW,PPS,PTICE,PSFTH_ICE,PSFTQ_ICE,  &
                                      PDIR_SW,PSCA_SW,PSWU_ICE,PLWU_ICE,  &
                                      OSIC                                )  
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
REAL,               INTENT(IN) :: PTSTEP    ! atmospheric time-step
REAL, DIMENSION(:), INTENT(IN) :: PZON10M   ! zonal wind
REAL, DIMENSION(:), INTENT(IN) :: PMER10M   ! meridian wind
REAL, DIMENSION(:), INTENT(IN) :: PSFU      ! zonal wind stress
REAL, DIMENSION(:), INTENT(IN) :: PSFV      ! meridian wind stress
REAL, DIMENSION(:), INTENT(IN) :: PSWD      ! total incoming short wave radiation
REAL, DIMENSION(:), INTENT(IN) :: PSWU      ! total upward short wave radiation
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX    ! storage flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:), INTENT(IN) :: PRAIN     ! Rainfall
REAL, DIMENSION(:), INTENT(IN) :: PSNOW     ! Snowfall
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PPS       ! Surface pressure
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PTICE     ! Ice Surface Temperature
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PSWU_ICE  ! upward short wave radiation on seaice
REAL, DIMENSION(:), INTENT(IN) :: PLWU_ICE  ! upward long  wave radiation on seaice
LOGICAL,            INTENT(IN) :: OSIC
END SUBROUTINE DIAG_CPL_ESM_SEA
END INTERFACE
END MODULE MODI_DIAG_CPL_ESM_SEA 
