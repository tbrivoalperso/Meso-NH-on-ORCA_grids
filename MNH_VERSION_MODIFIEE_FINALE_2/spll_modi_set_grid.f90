!depfile:set_grid.D
!     ######spl
      MODULE MODI_SET_GRID
!     ####################
!
INTERFACE
!
      SUBROUTINE SET_GRID(KMI,HINIFILE,HLUOUT,                                &
                          KIU,KJU,KKU,KIMAX_ll,KJMAX_ll,                      &
                          PBMX1,PBMX2,PBMX3,PBMX4,PBMY1,PBMY2,PBMY3,PBMY4,    &
                          PBFX1,PBFX2,PBFX3,PBFX4,PBFY1,PBFY2,PBFY3,PBFY4,    &
                          KXOR,KYOR,KXEND,KYEND,                              &
			  KDXRATIO,KDYRATIO,                                  &
                          HLBCX,HLBCY,                                        &
                          PTSTEP,PSEGLEN,                                     &
                          PLONORI,PLATORI,PLON,PLAT,                          &
                          PXHAT,PYHAT,PXHAT2D,PYHAT2D,                        &
                          PDXHAT,PDYHAT,PDXHAT2D,PDYHAT2D, PMAP,              &
                          PZS,PZZ,PZHAT,OSLEVE,PLEN1,PLEN2,PZSMT,             &
                          PJ,                                                 &
                          TPDTMOD,TPDTCUR,KSTOP,KOUT_TIMES,KOUT_NUMB          )
!     #########################################################################
USE MODD_TYPE_DATE
!
INTEGER,                INTENT(IN)  :: KMI       ! Model index
CHARACTER (LEN=*),      INTENT(IN)  :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN)  :: HLUOUT    ! name for output-listing
                                                 !  of nested models
INTEGER,                INTENT(IN)  :: KIU       ! Upper dimension in x direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KJU       ! Upper dimension in y direction
                                                 ! for sub-domain arrays
INTEGER,                INTENT(IN)  :: KKU       ! Upper dimension in z direction
                                                 ! for domain arrays
INTEGER,               INTENT(IN)   :: KIMAX_ll  !  Dimensions  in x direction
                                                 ! of the physical domain,
INTEGER,               INTENT(IN)   :: KJMAX_ll  !  Dimensions  in y direction
                                                 ! of the physical domain,
REAL, DIMENSION(:), INTENT(IN) :: PBMX1,PBMX2,PBMX3,PBMX4 ! Mass points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBMY1,PBMY2,PBMY3,PBMY4 ! Mass points in Y-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFX1,PBFX2,PBFX3,PBFX4 ! Flux points in X-direc.
REAL, DIMENSION(:), INTENT(IN) :: PBFY1,PBFY2,PBFY3,PBFY4 ! Flux points in Y-direc.
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the inner model domain, relative to outer model
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction resolution RATIO
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between inner model and outer model
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCX   ! type of lateral
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCY   ! boundary conditions
!
REAL,                   INTENT(IN)  :: PTSTEP    ! time step of model KMI
REAL,                   INTENT(INOUT) :: PSEGLEN ! segment duration (in seconds)
!
REAL,                   INTENT(OUT) :: PLONORI   ! Longitude  of the
                                                 ! Origine point of
                                                 ! conformal projection
REAL,                   INTENT(OUT) :: PLATORI   ! Latitude of the
                                                 ! Origine point of
                                                 ! conformal projection
REAL, DIMENSION(:,:),   INTENT(OUT) :: PLON,PLAT ! Longitude and latitude
REAL, DIMENSION(:),     INTENT(OUT) :: PXHAT     ! Position x in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:),     INTENT(OUT) :: PYHAT     ! Position y in the conformal
                                                 ! plane or on the cartesian plane
REAL, DIMENSION(:,:),     INTENT(OUT) :: PXHAT2D    ! horizontal stretching in x
REAL, DIMENSION(:,:),     INTENT(OUT) :: PYHAT2D    ! horizontal stretching in y
REAL, DIMENSION(:),     INTENT(OUT) :: PDXHAT    ! horizontal stretching in x
REAL, DIMENSION(:),     INTENT(OUT) :: PDYHAT    ! horizontal stretching in y
REAL, DIMENSION(:,:),     INTENT(OUT) :: PDXHAT2D    ! horizontal stretching in x
REAL, DIMENSION(:,:),     INTENT(OUT) :: PDYHAT2D    ! horizontal stretching in y

REAL, DIMENSION(:,:),   INTENT(OUT) :: PMAP      ! Map factor
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZS       ! orography
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PZZ       ! Height z
REAL, DIMENSION(:),     INTENT(OUT) :: PZHAT     ! Height  level
LOGICAL,                INTENT(OUT) :: OSLEVE    ! flag for SLEVE coordinate
REAL,                   INTENT(OUT) :: PLEN1     ! Decay scale for smooth topography
REAL,                   INTENT(OUT) :: PLEN2     ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:),   INTENT(OUT) :: PZSMT     ! smooth-orography
!
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTMOD   ! date and time of the model
                                                 ! beginning
TYPE (DATE_TIME),       INTENT(OUT) :: TPDTCUR   ! Current date and time
INTEGER,                INTENT(OUT) :: KSTOP     ! number of time steps for
                                                 ! current segment
INTEGER, DIMENSION(:), INTENT(OUT)  :: KOUT_TIMES ! list of the values
               ! of the temporal index in the temporal model loop where fields
               !  outputs on FM-files are realized
INTEGER,                INTENT(OUT) :: KOUT_NUMB ! number of outputs
!
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PJ        ! Jacobian
!
END SUBROUTINE SET_GRID
!
END INTERFACE
!
END MODULE MODI_SET_GRID
