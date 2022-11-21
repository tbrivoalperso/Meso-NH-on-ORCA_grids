!depfile:ini_dynamics.D
!     ######spl
      MODULE MODI_INI_DYNAMICS
!     ########################
INTERFACE
SUBROUTINE INI_DYNAMICS(HLUOUT,PLON,PLAT,PRHODJ,PTHVREF,PMAP,PZZ,            &
               PDXHAT2D,PDYHAT2D,PZHAT,HLBCX,HLBCY,PTSTEP,                       &
               OVE_RELAX,OVE_RELAX_GRD,OHORELAX_UVWTH,OHORELAX_RV,           &
               OHORELAX_RC,OHORELAX_RR,OHORELAX_RI,OHORELAX_RS,OHORELAX_RG,  &
               OHORELAX_RH,OHORELAX_TKE,OHORELAX_SV,                         &
               OHORELAX_SVC2R2,OHORELAX_SVC1R3,OHORELAX_SVELEC,OHORELAX_SVLG,&
               OHORELAX_SVCHEM,OHORELAX_SVAER,OHORELAX_SVDST,OHORELAX_SVSLT, &
               OHORELAX_SVPP,OHORELAX_SVCS,  OHORELAX_SVCHIC,                &
               PRIMKMAX,KRIMX,KRIMY,PALKTOP,PALKGRD,PALZBOT,PALZBAS,         &
               PT4DIFU,PT4DIFTH,PT4DIFSV,                                    &
               PCORIOX,PCORIOY,PCORIOZ,PCURVX,PCURVY,                        &
               PDXHATM,PDYHATM,PRHOM,PAF,PBFY,PCF,                           &
               PTRIGSX,PTRIGSY,KIFAXX,KIFAXY,                                &
               PALK,PALKW,KALBOT,PALKBAS,PALKWBAS,KALBAS,                    &
               OMASK_RELAX,PKURELAX, PKVRELAX, PKWRELAX,                     &
               PDK2U,PDK4U,PDK2TH,PDK4TH,PDK2SV,PDK4SV,OZDIFFU,PZDIFFU_HALO2,& 
               PBFB,&
               PBF_SXP2_YP1_Z) !JUAN Z_SPLITING
!  intent in arguments
!
USE MODE_TYPE_ZDIFFU
IMPLICIT NONE
!
CHARACTER(LEN=*),       INTENT(IN)        :: HLUOUT    ! name for output-listing
REAL, DIMENSION(:,:),   INTENT(IN)        :: PLON,PLAT !Longitude and latitude
REAL, DIMENSION(:,:,:), INTENT(IN)        :: PRHODJ    ! rho J
REAL, DIMENSION(:,:,:), INTENT(IN)        :: PTHVREF   ! virtual potential 
                                      ! temperature of the reference state
REAL, DIMENSION(:,:),   INTENT(IN)        :: PMAP      ! Map factor
REAL, DIMENSION(:,:,:), INTENT(IN)        :: PZZ       ! height 
REAL, DIMENSION(:,:),     INTENT(IN)        :: PDXHAT2D     ! Stretching in x direction
REAL, DIMENSION(:,:),     INTENT(IN)        :: PDYHAT2D     ! Stretching in y direction
REAL, DIMENSION(:),     INTENT(IN)        :: PZHAT     ! Gal-Chen Height   
CHARACTER(LEN=4), DIMENSION(:), INTENT(IN) :: HLBCX    ! x-direction LBC type
CHARACTER(LEN=4), DIMENSION(:), INTENT(IN) :: HLBCY    ! y-direction LBC type
LOGICAL,                INTENT(IN)        :: OVE_RELAX ! logical
             ! switch to activate the VErtical  RELAXation
LOGICAL,                INTENT(IN)        :: OVE_RELAX_GRD ! logical
             ! switch to activate the VErtical  RELAXation (ground layer)
LOGICAL,            INTENT(IN) :: OHORELAX_UVWTH  ! switch for the 
                       ! horizontal relaxation for U,V,W,TH
LOGICAL,            INTENT(IN) :: OHORELAX_RV     ! switch for the 
                       ! horizontal relaxation for Rv
LOGICAL,            INTENT(IN) :: OHORELAX_RC     ! switch for the 
                       ! horizontal relaxation for Rc
LOGICAL,            INTENT(IN) :: OHORELAX_RR     ! switch for the 
                       ! horizontal relaxation for Rr
LOGICAL,            INTENT(IN) :: OHORELAX_RI     ! switch for the 
                       ! horizontal relaxation for Ri
LOGICAL,            INTENT(IN) :: OHORELAX_RS     ! switch for the 
                       ! horizontal relaxation for Rs
LOGICAL,            INTENT(IN) :: OHORELAX_RG     ! switch for the 
                       ! horizontal relaxation for Rg
LOGICAL,            INTENT(IN) :: OHORELAX_RH     ! switch for the 
                       ! horizontal relaxation for Rh
LOGICAL,            INTENT(IN) :: OHORELAX_TKE    ! switch for the 
                       ! horizontal relaxation for tke
LOGICAL,DIMENSION(:),INTENT(IN):: OHORELAX_SV     ! switch for the 
                       ! horizontal relaxation for sv variables
LOGICAL,             INTENT(IN):: OHORELAX_SVC2R2 ! switch for the 
                       ! horizontal relaxation for c2r2 variables
LOGICAL,             INTENT(IN):: OHORELAX_SVC1R3 ! switch for the 
                       ! horizontal relaxation for c1r3 variables
LOGICAL,             INTENT(IN):: OHORELAX_SVELEC ! switch for the 
                       ! horizontal relaxation for elec variables
LOGICAL,             INTENT(IN):: OHORELAX_SVLG   ! switch for the 
                       ! horizontal relaxation for lg variables
LOGICAL,             INTENT(IN):: OHORELAX_SVCHEM ! switch for the 
                       ! horizontal relaxation for chem variables
LOGICAL,             INTENT(IN):: OHORELAX_SVCHIC ! switch for the 
                       ! horizontal relaxation for ice chem variables
LOGICAL,             INTENT(IN):: OHORELAX_SVAER  ! switch for the 
                       ! horizontal relaxation for aer variables
LOGICAL,             INTENT(IN):: OHORELAX_SVDST  ! switch for the 
                       ! horizontal relaxation for dst variables
LOGICAL,             INTENT(IN):: OHORELAX_SVSLT  ! switch for the 
                       ! horizontal relaxation for slt variables
LOGICAL,             INTENT(IN):: OHORELAX_SVPP   ! switch for the 
                       ! horizontal relaxation for passive pollutants
LOGICAL,             INTENT(IN):: OHORELAX_SVCS   ! switch for the 
                       ! horizontal relaxation for conditional sampling
REAL,                    INTENT(IN)    :: PRIMKMAX !Max. value of the horiz.
                                     ! relaxation coefficients
INTEGER,                INTENT(IN)     :: KRIMX,KRIMY ! Number of points in 
                                     ! the rim zone in the x and y directions 
REAL,     INTENT(IN)   :: PALKTOP    ! Damping coef. at the top of the absorbing
                                     ! layer
REAL,     INTENT(IN)   :: PALKGRD    ! Damping coef. at the top of the absorbing
                                     ! layer                                    
REAL,     INTENT(IN)   :: PALZBOT    ! Height of the absorbing layer base
REAL,     INTENT(IN)   :: PALZBAS    ! Height of the absorbing layer base
REAL,     INTENT(IN)   :: PT4DIFU    ! Damping time scale for 2*dx wavelength
                                     ! specified for the 4nd order num. diffusion
                                     ! for momentum
REAL,     INTENT(IN)   :: PT4DIFTH   ! for meteorological scalar variables
REAL,     INTENT(IN)   :: PT4DIFSV   ! for tracer scalar variables

REAL,     INTENT(IN)   :: PTSTEP     ! Time step    
!
!  intent out arguments
!
REAL, INTENT(OUT) :: PDXHATM                     ! mean grid increment in the x
                                                 ! direction
REAL, INTENT(OUT) :: PDYHATM                     ! mean grid increment in the y
                                                 ! direction
!
REAL, DIMENSION (:), INTENT(OUT) :: PRHOM        !  mean of XRHODJ on the plane x y 
                                                 !  localized at a mass level
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PCORIOX,PCORIOY ! Hor. Coriolis parameters
REAL, DIMENSION(:,:), INTENT(OUT) :: PCORIOZ         ! Vert. Coriolis parameter 
REAL, DIMENSION(:,:), INTENT(OUT) :: PCURVX,PCURVY   ! Curvature coefficients
! 
REAL, DIMENSION(:),     INTENT(OUT) :: PAF ! vectors giving the non-vanishing        
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBFY ! elements of the tri-diag matrix        
                                            ! on an y-slice of global physical domain
REAL, DIMENSION(:),     INTENT(OUT) :: PCF ! in the pressure equation
REAL, DIMENSION(:),     INTENT(OUT) :: PTRIGSX ! Arrays for sinus or cosinus 
REAL, DIMENSION(:),     INTENT(OUT) :: PTRIGSY ! values for the FFT in x and
                                               ! y directions
INTEGER, DIMENSION(:),  INTENT(OUT) :: KIFAXX  ! Decomposition in prime numbers
INTEGER, DIMENSION(:),  INTENT(OUT) :: KIFAXY  ! for the FFT in x and y
                                               ! direction      
INTEGER          ,  INTENT(OUT)  :: KALBOT     ! Vertical index corresponding
                                               ! to the absorbing layer base
!  
REAL, DIMENSION(:),   INTENT(OUT) :: PALK  ! Function of the absorbing
                                           ! layer damping coefficient
                                           ! defined for  u,v,and theta
REAL, DIMENSION(:),   INTENT(OUT) :: PALKW ! Idem but defined for w    
INTEGER          ,  INTENT(OUT)  :: KALBAS     ! Vertical index corresponding
                                               ! to the absorbing layer base
!  
REAL, DIMENSION(:),   INTENT(OUT) :: PALKBAS  ! Function of the absorbing
                                           ! layer damping coefficient
                                           ! defined for  u,v,and theta
REAL, DIMENSION(:),   INTENT(OUT) :: PALKWBAS ! Idem but defined for w
LOGICAL, DIMENSION(:,:),  INTENT(OUT)   :: OMASK_RELAX  ! True where the 
                                           ! lateral relax. has to be performed
REAL, DIMENSION(:,:),     INTENT(OUT)   :: PKURELAX  !  Horizontal relaxation
REAL, DIMENSION(:,:),     INTENT(OUT)   :: PKVRELAX  !  coefficients for the
REAL, DIMENSION(:,:),     INTENT(OUT)   :: PKWRELAX  ! u, v and mass locations
REAL,                 INTENT(OUT) :: PDK2U ! 2nd order num. diffusion coef. /dx2
REAL,                 INTENT(OUT) :: PDK4U ! 4nd order num. diffusion coef. /dx4
                                           ! for momentum
REAL,                 INTENT(OUT) :: PDK2TH! for meteorological scalar variables
REAL,                 INTENT(OUT) :: PDK4TH! 
REAL,                 INTENT(OUT) :: PDK2SV! for tracer scalar variables
REAL,                 INTENT(OUT) :: PDK4SV! 
!
LOGICAL, INTENT(IN) :: OZDIFFU
TYPE(TYPE_ZDIFFU_HALO2)                       :: PZDIFFU_HALO2
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBFB ! elements of the tri-diag matrix
                                            ! on an b-slice of global physical domain
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PBF_SXP2_YP1_Z ! elements of the tri-diag. SXP2_YP1_Z-slide 
                                                   ! matrix in the pressure eq.
END SUBROUTINE INI_DYNAMICS
!
END INTERFACE
!
END MODULE MODI_INI_DYNAMICS
