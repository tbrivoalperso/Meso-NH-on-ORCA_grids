!depfile:read_namelists_seafluxn.D
!     ######spl
SUBROUTINE READ_NAMELISTS_SEAFLUX_n (SM, &
                                     HPROGRAM,HINIT)
!     #######################################################
!
!!      Modified    03/2014 : M.N. Bouin  ! possibility of wave parameters
!!                                        ! from external source
!---------------------------------------------------------------------------   
!
!
!
USE MODD_SURFEX_n, ONLY : SEAFLUX_MODEL_t
!
USE MODN_SEAFLUX_n
!
USE MODI_DEFAULT_SEAFLUX
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_DIAG_SEAFLUX
USE MODI_READ_DEFAULT_SEAFLUX_n
USE MODI_DEFAULT_SEAICE
USE MODI_READ_SEAFLUX_CONF_n
!
USE MODI_READ_NAM_PREP_SEAFLUX_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SEAFLUX_N',0,ZHOOK_HANDLE)
!
 CALL DEFAULT_SEAFLUX(XTSTEP,XOUT_TSTEP,CSEA_ALB,CSEA_FLUX,LPWG,         &
                     LPRECIP,LPWEBB,NZ0,NGRVWAVES,LPROGSST,           &
                     NTIME_COUPLING,XOCEAN_TSTEP,XICHCE,CINTERPOL_SST,&
                     CINTERPOL_SSS,LWAVEWIND)
 CALL DEFAULT_SEAICE(HPROGRAM, CINTERPOL_SIC, CINTERPOL_SIT,             &
                    XFREEZING_SST,XSEAICE_TSTEP, XSIC_EFOLDING_TIME,    &
                    XSIT_EFOLDING_TIME, XCD_ICE_CST, XSI_FLX_DRV        )     
!               
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
!
 CALL DEFAULT_DIAG_SEAFLUX(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                          LDIAG_OCEAN,LDIAG_SEAICE,LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP)  
!
 CALL READ_DEFAULT_SEAFLUX_n(SM%CHS, SM%DGO, SM%DGS, SM%DGSI, SM%O, SM%S, &
                            HPROGRAM)
!
 CALL READ_SEAFLUX_CONF_n(SM%CHS, SM%DGO, SM%DGS, SM%DGSI, SM%O, SM%S, &
                         HPROGRAM)
!
IF (HINIT=='PRE') CALL READ_NAM_PREP_SEAFLUX_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAMELISTS_SEAFLUX_n
