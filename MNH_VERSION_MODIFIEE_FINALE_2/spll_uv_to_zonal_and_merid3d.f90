!depfile:uv_to_zonal_and_merid.D
!     ######spl
      SUBROUTINE UV_TO_ZONAL_AND_MERID3D(PU,PV,KGRID,PZC,PMC,HFMFILE,HRECU,HRECV,HCOMMENT)
!     ##########################################
!
!!****  *UV_TO_ZONAL_AND_MERID* - compute the zonal and meridien components
!!                                of input wind, and return or write them
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
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
!!      I. Mallet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/11/00
!!      N.Asencio   10/09/03 no pointer for PZC,PMC (no pointer in SHUMAN)
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CONF
USE MODD_CST
USE MODD_GRID
USE MODD_PARAMETERS
USE MODD_DIM_n
USE MODD_GRID_n
USE MODD_LUNIT_n
!
USE MODE_FM
USE MODE_FMWRIT
!
USE MODI_SHUMAN
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PU    ! input U component
REAL, DIMENSION(:,:,:), INTENT(IN) :: PV    ! input V component
INTEGER,                INTENT(IN) :: KGRID ! grid positions of components
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PZC   ! output U component
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PMC   ! output V component
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HFMFILE   ! Name of FM-file to write
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HRECU     ! Name of the U article
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HRECV     ! Name of the V article
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: HCOMMENT  ! Comment string
!
!*      0.2    declarations of local variables
!
INTEGER                            :: IKU
REAL                               :: ZRAD_O_DG
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZWORK2
REAL, DIMENSION(:,:,:), ALLOCATABLE:: ZWORK3
REAL, DIMENSION(:,:,:), ALLOCATABLE:: ZZC,ZMC
!
INTEGER           :: IGRID          ! grid indicator
INTEGER           :: IRESP          ! return-code for the file routines
INTEGER           :: ILENCH         ! length of comment string
INTEGER           :: ILUOUT         ! logical unit for output listing
!-----------------------------------------------------------------
!

CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
IKU=SIZE(PU,3)
ALLOCATE(ZWORK2(SIZE(XLON,1),SIZE(XLON,2)))
ALLOCATE(ZWORK3(SIZE(XLON,1),SIZE(XLON,2),IKU))
!
ALLOCATE(ZZC(SIZE(XLON,1),SIZE(XLON,2),IKU))
ALLOCATE(ZMC(SIZE(XLON,1),SIZE(XLON,2),IKU))
!
ZRAD_O_DG = XPI/180.
IF (LCARTESIAN) THEN                 ! cartesian geometry
  ZWORK2(:,:) = -XBETA *ZRAD_O_DG
ELSE                                 ! conformal projection
!  ZWORK2(:,:) = XRPK * (XLON(:,:) -XLON0) * ZRAD_O_DG -(XBETA *ZRAD_O_DG)
  ZWORK2(:,:)=ATAN(((xlont(:,:)-xlonv(:,:))*COS(xlatt(:,:)*(XPI/180)))/(xlatt(:,:)-xlatv(:,:)))

END IF
ZWORK3(:,:,:) = SPREAD( ZWORK2(:,:),DIM=3,NCOPIES=IKU )
DEALLOCATE(ZWORK2)
!
! Zonal and Meridien components of wind
!
IF (KGRID==23) THEN
  WRITE(ILUOUT,*) '- zonal and meridien components of winds are computed'
  WHERE ( (PU(:,:,:) /= XUNDEF) .AND. (PV(:,:,:) /= XUNDEF) ) 
    ZZC(:,:,:) =  PU(:,:,:) *MXM(COS(ZWORK3(:,:,:))) &
              + MYF(MXM(PV(:,:,:))) *MXM(SIN(ZWORK3(:,:,:)))
    ZMC(:,:,:) = - MXF(MYM(PU(:,:,:))) *MYM(SIN(ZWORK3(:,:,:))) &
               + PV(:,:,:) *MYM(COS(ZWORK3(:,:,:)))
  ELSEWHERE
    ZZC(:,:,:) = XUNDEF
    ZMC(:,:,:) = XUNDEF
  END WHERE
ELSE IF (KGRID==11) THEN
  WRITE(ILUOUT,*) '- zonal and meridien components of winds are computed'
  WHERE ( (PU(:,:,:) /= XUNDEF) .AND. (PV(:,:,:) /= XUNDEF) ) 
    ZZC(:,:,:) = PU(:,:,:) *COS(ZWORK3(:,:,:)) +PV(:,:,:) *SIN(ZWORK3(:,:,:))
    ZMC(:,:,:) = - PU(:,:,:) *SIN(ZWORK3(:,:,:)) +PV(:,:,:) *COS(ZWORK3(:,:,:))
  ELSEWHERE
    ZZC(:,:,:) = XUNDEF
    ZMC(:,:,:) = XUNDEF
  END WHERE
ELSE IF (KGRID==0) THEN
!
! in this case, input winds are ZONal and MERidien
!          and, output ones are in MesoNH grid (mass points)
  WRITE(ILUOUT,*) '- components of winds are replaced in MesoNH grid'
  WHERE ( (PU(:,:,:) /= XUNDEF) .AND. (PV(:,:,:) /= XUNDEF) ) 
    ZZC(:,:,:) = COS(ZWORK3(:,:,:))* PU(:,:,:) - SIN(ZWORK3(:,:,:))* PV(:,:,:)
    ZMC(:,:,:) = SIN(ZWORK3(:,:,:))* PU(:,:,:) + COS(ZWORK3(:,:,:))* PV(:,:,:)
  ELSEWHERE
    ZZC(:,:,:) = XUNDEF
    ZMC(:,:,:) = XUNDEF
  END WHERE
ELSE
  WRITE(ILUOUT,*) '- warning in uv_to_zonal_and_merid: no computation for GRID= ',KGRID
  RETURN
END IF
!
IF (PRESENT(HFMFILE)) THEN
  ILENCH=LEN(HCOMMENT)
  SELECT CASE (KGRID)
    CASE(23)
      IGRID=2
      CALL FMWRIT(HFMFILE,HRECU,CLUOUT,'XY',ZZC,IGRID,ILENCH,HCOMMENT,IRESP)
      IGRID=3
      CALL FMWRIT(HFMFILE,HRECV,CLUOUT,'XY',ZMC,IGRID,ILENCH,HCOMMENT,IRESP)
    CASE(11,0)
      IGRID=1
      CALL FMWRIT(HFMFILE,HRECU,CLUOUT,'XY',ZZC,IGRID,ILENCH,HCOMMENT,IRESP)
      IGRID=1
      CALL FMWRIT(HFMFILE,HRECV,CLUOUT,'XY',ZMC,IGRID,ILENCH,HCOMMENT,IRESP)
  END SELECT
ELSE IF (PRESENT(PZC) .AND. PRESENT(PMC)) THEN
  PZC(:,:,:) = ZZC(:,:,:)
  PMC(:,:,:) = ZMC(:,:,:)
ELSE
  WRITE(ILUOUT,*) '- warning in uv_to_zonal_and_merid3d: bad optional arguments'
  RETURN
END IF
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZWORK3)
DEALLOCATE(ZZC,ZMC)
!
END SUBROUTINE UV_TO_ZONAL_AND_MERID3D
