!depfile:default_prep_seaflux.D
!     ######spl
      SUBROUTINE DEFAULT_PREP_SEAFLUX
!     ###########################
!
!!****  *DEFAULT_PREP_SEAFLUX* - routine to set default values for the configuration for SEAFLUX field preparation
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S. Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003 
!!      01/2008     C. Lebeaupin Brossier ! initialization of oceanic var. 
!!                                        ! from MERCATOR analyses types
!!      07/2012     P. Le Moigne          ! CMO1D phasing
!!      01/2014     S. Senesi             ! introduce fractional seaice and sea-ice model 
!!      03/2014     S. Belamari           ! initialize sea surface salinity
!!      03/2014     M.N. Bouin            ! possibility of wave parameters 
!!                                        ! from external source
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_SEAFLUX,   ONLY : CFILE_SEAFLX, CTYPE_SEAFLX, CFILEPGD_SEAFLX, CTYPEPGD, XSST_UNIF,&
                                XSSS_UNIF, XSIC_UNIF, CFILEWAVE_SEAFLX, CTYPEWAVE
!
USE MODN_PREP_SEAFLUX,   ONLY : LSEA_SBL, CSEAICE_SCHEME, LOCEAN_MERCATOR, LOCEAN_CURRENT, &
                                XTIME_REL, LCUR_REL, LTS_REL,    &
                                LZERO_FLUX, LCORR_FLUX, XCORFLX, LDIAPYC

USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_SEAFLUX',0,ZHOOK_HANDLE)
CFILE_SEAFLX = '                          '
CTYPE_SEAFLX = 'GRIB  '
!
CFILEWAVE_SEAFLX = '                          '
CTYPEWAVE       = '      '
!
CFILEPGD_SEAFLX = '                          '
CTYPEPGD        = '      '
!
XSST_UNIF = XUNDEF
XSSS_UNIF = XUNDEF
XSIC_UNIF = XUNDEF
!
LSEA_SBL = .FALSE.
CSEAICE_SCHEME='NONE  '
LOCEAN_MERCATOR = .FALSE.
LOCEAN_CURRENT = .FALSE.
!
XTIME_REL  = 25920000.
XCORFLX    = 0.
LCUR_REL   = .FALSE.
LTS_REL    = .FALSE.
LZERO_FLUX = .FALSE.
LCORR_FLUX = .FALSE.
LDIAPYC    = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_SEAFLUX
