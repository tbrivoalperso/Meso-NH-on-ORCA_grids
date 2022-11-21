!depfile:read_all_data_grib_case.D
!     ######spl
      MODULE MODI_READ_ALL_DATA_GRIB_CASE
!     #################################
INTERFACE
SUBROUTINE READ_ALL_DATA_GRIB_CASE(HFILE,HPRE_REAL1,HGRIB,HPGDFILE,      &
                    PTIME_HORI,KVERB,ODUMMY_REAL,ZLATM,ZLONM,PXHAT2D,PYHAT2D, &
		    platu,plonu,platv,plonv,platt,plont  ) 
!
CHARACTER(LEN=4),   INTENT(IN) :: HFILE    !which file ('ATM0','ATM1' or 'CHEM')
CHARACTER(LEN=28),  INTENT(IN) :: HPRE_REAL1 ! name of the PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN) :: HGRIB    ! name of the GRIB file
CHARACTER(LEN=28),  INTENT(IN) :: HPGDFILE ! name of the physiographic data file
INTEGER,           INTENT(IN)  :: KVERB    ! verbosity level
LOGICAL,           INTENT(IN)  :: ODUMMY_REAL! flag to interpolate dummy fields
REAL,           INTENT(INOUT)  :: PTIME_HORI ! time spent in hor. interpolations
REAL, DIMENSION(:,:), INTENT(IN) :: ZLATM         ! Lat of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: ZLONM         ! Lon of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: PXHAT2D         ! Lat of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: PYHAT2D         ! Lon of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: platu         ! Lat of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: plonu         ! Lon of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: platv         ! Lat of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: plonv         ! Lon of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: platt         ! Lat of PGD mass points
REAL, DIMENSION(:,:), INTENT(IN) :: plont         ! Lon of PGD mass points
END SUBROUTINE READ_ALL_DATA_GRIB_CASE
!
END INTERFACE
END MODULE MODI_READ_ALL_DATA_GRIB_CASE
