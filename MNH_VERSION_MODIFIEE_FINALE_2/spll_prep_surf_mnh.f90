!depfile:prep_surf_mnh.D
!     ######spl
      SUBROUTINE PREP_SURF_MNH(HATMFILE,HATMFILETYPE)
!     #######################################################
!
!!****  *PREP_SURF_MNH* - calls surface field preparation
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!     V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!  06/2016     (G.Delautier) phasage surfex 8
!------------------------------------------------------------------
!

!
USE MODD_LUNIT_n,     ONLY : CINIFILE
USE MODD_TIME_n,      ONLY : TDTCUR
USE MODD_LUNIT,       ONLY : CLUOUT0, COUTFMFILE, CPGDFILE
USE MODD_IO_SURF_MNH, ONLY : COUTFILE, CFILE
USE MODE_FM
!
USE MODI_INIT_PGD_SURF_ATM
USE MODI_PREP_SURF_ATM
USE MODI_WRITE_SURF_ATM_N
USE MODI_WRITE_DIAG_SURF_ATM_N
USE MODD_MNH_SURFEX_n
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=28), INTENT(IN)   :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),  INTENT(IN)   :: HATMFILETYPE! type of the Atmospheric file
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=28)  :: YPGDFILE  ='                            '  ! name of the pgd file
CHARACTER(LEN=6)   :: YPGDFILETYPE ='      '                     ! type of the pgd file
INTEGER  :: ILUOUT0  ! logical unit for listing file
INTEGER  :: IRESP    ! return code in FM routines
CHARACTER(LEN=6) :: YATMFILETYPE    ! type of the Atmospheric file
!------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
WRITE(ILUOUT0,*) '***************************************************'
WRITE(ILUOUT0,*) '***************** EXTERNALIZED SURFACE ************'
WRITE(ILUOUT0,*) '***************************************************'
!
!
COUTFILE   = CINIFILE
COUTFMFILE = CINIFILE
!
YATMFILETYPE = HATMFILETYPE
IF(YATMFILETYPE=='GRIBEX') YATMFILETYPE='GRIB  '
IF (LEN_TRIM(HATMFILE)==0) YATMFILETYPE='      '
!
CALL INIT_PGD_SURF_ATM(YSURF_CUR,'MESONH','PRE',HATMFILE,YATMFILETYPE,  &
                       TDTCUR%TDATE%YEAR, TDTCUR%TDATE%MONTH, &
                       TDTCUR%TDATE%DAY, TDTCUR%TIME          )
PRINT*,'INIT_PGD_SURF_ATM OK'
CALL PREP_SURF_ATM(YSURF_CUR,'MESONH',HATMFILE,YATMFILETYPE,HATMFILE,YATMFILETYPE)
PRINT*,'PREP_SURF_ATM OK'
CALL WRITE_SURF_ATM_n(YSURF_CUR,'MESONH','PRE',.FALSE.)
PRINT*,'WRITE_SURF_ATM_n OK'
CALL WRITE_DIAG_SURF_ATM_n(YSURF_CUR,'MESONH','PRE')

!
!----------------------------------------------------------
!
END SUBROUTINE PREP_SURF_MNH
