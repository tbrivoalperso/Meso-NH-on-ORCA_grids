!depfile:modn_prep_seaflux.D
!     ######spl
      MODULE MODN_PREP_SEAFLUX
!     ##################
!
!!****  *MODN_PREP_SEAFLUX* - declaration of namelist NAM_PREP_SEAFLUX
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_SEAFLUX
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!      S.Malardel    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003                    
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!                  07/2013, S. Senesi    : handle seaice scheme
!!                              and uniform sea surface salinity and ice cover
!!      Modified    03/2014 : M.N. Bouin  ! possibility of wave parameters
!!                                        ! from external source
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_SEAFLUX, ONLY : CFILE_SEAFLX, CTYPE_SEAFLX, CFILEPGD_SEAFLX, CTYPEPGD, &
                              XSST_UNIF, XSSS_UNIF, XSIC_UNIF, &
			CFILEWAVE_SEAFLX, CTYPEWAVE
!
IMPLICIT NONE
!
INTEGER           :: NYEAR            ! YEAR for surface
INTEGER           :: NMONTH           ! MONTH for surface
INTEGER           :: NDAY             ! DAY for surface
REAL              :: XTIME            ! TIME for surface
LOGICAL           :: LSEA_SBL         ! flag to use air layers inside the SBL
CHARACTER(LEN=6)  :: CSEAICE_SCHEME    ! name of the seaice scheme
LOGICAL           :: LOCEAN_MERCATOR  ! oceanic variables initialized from 
                                      !   MERCATOR if true
LOGICAL           :: LOCEAN_CURRENT   ! initial ocean state with current 
                                      !   (if false ucur=0, vcur=0)
REAL              :: XTIME_REL        ! relaxation time (s)
LOGICAL           :: LCUR_REL         ! If T, relax on current
LOGICAL           :: LTS_REL          ! If T, relax on T, S
LOGICAL           :: LZERO_FLUX       ! If T, relax on T, S
LOGICAL           :: LCORR_FLUX       ! If T, fluxes correction is made
REAL              :: XCORFLX          ! correction coefficient ( W.m-2.K-1)
LOGICAL           :: LDIAPYC          ! If T, fluxes correction is made                                      
!
NAMELIST/NAM_PREP_SEAFLUX/CFILE_SEAFLX, CTYPE_SEAFLX, CFILEPGD_SEAFLX, CTYPEPGD, XSST_UNIF,  &
                          CFILEWAVE_SEAFLX, CTYPEWAVE, &
                          XSSS_UNIF, XSIC_UNIF, NYEAR, NMONTH, NDAY, XTIME, LSEA_SBL, &
                          CSEAICE_SCHEME, LOCEAN_MERCATOR, LOCEAN_CURRENT,              & 
                          XTIME_REL,LCUR_REL,LTS_REL,                                  &
                          LZERO_FLUX,XCORFLX,LCORR_FLUX, LDIAPYC  
!
END MODULE MODN_PREP_SEAFLUX
