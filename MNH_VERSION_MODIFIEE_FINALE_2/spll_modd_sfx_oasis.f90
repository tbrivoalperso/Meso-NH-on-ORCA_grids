!depfile:modd_sfx_oasis.D
!     ######spl
MODULE MODD_SFX_OASIS
!###############
!
!!****  *MODD_SFX_OASIS - declaration of variable for SFX-OASIS coupling
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/13
!!      Modified       11/2014 : J. Pianezze - add wave coupling and creation of OASIS grids
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! * Surfex - Oasis coupling general key :
!
!-------------------------------------------------------------------------------
!
LOGICAL             :: LOASIS   = .FALSE. ! To use oasis coupler or not
LOGICAL             :: LOASIS_GRID = .FALSE. ! To define oasis grids, areas and masks during simulation
REAL                :: XRUNTIME = 0.0     ! Total simulated time in oasis namcouple (s)
!
!-------------------------------------------------------------------------------
!
! * Land surface variables for Surfex - Oasis coupling
!
!-------------------------------------------------------------------------------
!
LOGICAL             :: LCPL_LAND    = .FALSE. ! Fields to/from surfex land area
LOGICAL             :: LCPL_CALVING = .FALSE. ! Calving flux from surfex land area
LOGICAL             :: LCPL_GW      = .FALSE. ! Fields to/from surfex land area to/from groundwater scheme
LOGICAL             :: LCPL_FLOOD   = .FALSE. ! Fields to/from surfex land area to/from floodplains scheme
!
! Output variables
!
INTEGER             :: NRUNOFF_ID    ! Surface runoff id
INTEGER             :: NDRAIN_ID     ! Drainage id
INTEGER             :: NCALVING_ID   ! Calving flux id
INTEGER             :: NRECHARGE_ID  ! Groundwater recharge id
INTEGER             :: NSRCFLOOD_ID  ! Floodplains freshwater flux id
!
! Input variables
!
INTEGER             :: NWTD_ID       ! water table depth id
INTEGER             :: NFWTD_ID      ! grid-cell fraction of water table rise id
INTEGER             :: NFFLOOD_ID    ! Floodplains fraction id
INTEGER             :: NPIFLOOD_ID   ! Potential flood infiltration id
!
!-------------------------------------------------------------------------------
!
! * Lake variables for Surfex - Oasis coupling
!
!-------------------------------------------------------------------------------
!
LOGICAL             :: LCPL_LAKE    = .FALSE. ! Fields to/from surfex lake area
!
! Output variables
!
INTEGER             :: NLAKE_EVAP_ID ! Evaporation id
INTEGER             :: NLAKE_RAIN_ID ! Rainfall id
INTEGER             :: NLAKE_SNOW_ID ! Snowfall id
INTEGER             :: NLAKE_WATF_ID ! Freshwater id
!
!-------------------------------------------------------------------------------
!
! * Sea variables for Surfex - Oasis coupling 
!
!-------------------------------------------------------------------------------
!
LOGICAL             :: LCPL_SEA     = .FALSE. ! Fields to/from surfex sea/water area
LOGICAL             :: LCPL_SEAICE  = .FALSE. ! Fields to/from surfex sea-ice area (e.g. GELATO 3D, ...)
!
! Sea Output variables
!
INTEGER             :: NSEA_FWSU_ID ! zonal wind stress id
INTEGER             :: NSEA_FWSV_ID ! meridian wind stress id
INTEGER             :: NSEA_HEAT_ID ! Non solar net heat flux id
INTEGER             :: NSEA_SNET_ID ! Solar net heat flux id
INTEGER             :: NSEA_WIND_ID ! 10m wind speed id
INTEGER             :: NSEA_FWSM_ID ! wind stress id
INTEGER             :: NSEA_EVAP_ID ! Evaporation id
INTEGER             :: NSEA_RAIN_ID ! Rainfall id
INTEGER             :: NSEA_SNOW_ID ! Snowfall id
INTEGER             :: NSEA_EVPR_ID ! Evap.-Precip. id
INTEGER             :: NSEA_WATF_ID ! Freshwater id
INTEGER             :: NSEA_PRES_ID ! Surface pressure id
!
! Sea-ice Output variables
!
INTEGER             :: NSEAICE_HEAT_ID ! Sea-ice non solar net heat flux id
INTEGER             :: NSEAICE_SNET_ID ! Sea-ice solar net heat flux id
INTEGER             :: NSEAICE_EVAP_ID ! Sea-ice sublimation id
!
! Sea Input variables
!
INTEGER             :: NSEA_SST_ID ! Sea surface temperature id
INTEGER             :: NSEA_UCU_ID ! Sea u-current stress id
INTEGER             :: NSEA_VCU_ID ! Sea v-current stress id
! TBRIVOAL
INTEGER             :: NSEA_SST_ID2 ! Sea surface temperature id
INTEGER             :: NSEA_UCU_ID2 ! Sea u-current stress id
INTEGER             :: NSEA_VCU_ID2 ! Sea v-current stress id
!

! Sea-ice Input variables
!
INTEGER             :: NSEAICE_SIT_ID  ! Sea-ice Temperature id
INTEGER             :: NSEAICE_CVR_ID  ! Sea-ice cover id
INTEGER             :: NSEAICE_ALB_ID  ! Sea-ice albedo id
!
!-------------------------------------------------------------------------------
!
! * Wave variables for Surfex - Oasis coupling 
!
!-------------------------------------------------------------------------------
!
LOGICAL             :: LCPL_WAVE    = .FALSE. ! Fields to/from surfex wave area
!
! Wave Output variables
!
INTEGER             :: NWAVE_U10_ID ! 10m u-wind speed id
INTEGER             :: NWAVE_V10_ID ! 10m v-wind speed id
!
! Wave Input variables
!
INTEGER             :: NWAVE_CHA_ID ! Charnock coefficient id
INTEGER             :: NWAVE_UCU_ID ! Wave u-current velocity id
INTEGER             :: NWAVE_VCU_ID ! Wave v-current velocity id
INTEGER             :: NWAVE_HS_ID  ! Significant wave height id
INTEGER             :: NWAVE_TP_ID  ! Peak period id
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_SFX_OASIS
