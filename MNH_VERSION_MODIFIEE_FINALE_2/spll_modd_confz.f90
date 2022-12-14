!depfile:modd_confz.D
!     ######spl
      MODULE MODD_CONFZ
!     #################
!
!!****  *MODD_CONFZ* - declaration of configuration variables
!!
!!    PURPOSE
!!    -------
!    configuration of ZSPLITTING 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!	J. Escobar L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/02/2009    
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER,PARAMETER :: JZ_FLAT_INV=1 , JZ_FLAT_INVZ=2 , JZ_P1P2_SPLITTING=8
!JUAN
INTEGER,SAVE      :: NZ_VERB        = 0  ! level of message for NZ solver/IO
INTEGER,SAVE      :: NZ_PROC        = 0  ! Number of proc to use in the Z splitting 

INTEGER,SAVE      :: NB_PROCIO_R    = 1  ! Number of proc to use for parallel IO in read file
INTEGER,SAVE      :: NB_PROCIO_W    = 1  ! Number of proc to use for parallel IO in read write 
INTEGER,SAVE      :: MPI_BUFFER_SIZE = 400  ! default size for MPI_BSEND buffer in 10^6 Bytes
LOGICAL,SAVE      :: LMNH_MPI_BSEND = .FALSE.  ! default send mode MPI_BSEND else MPI_ISEND
LOGICAL,SAVE      :: LMNH_MPI_ALLTOALLV_REMAP = .FALSE. ! default remap with send/recv     <=> NZ_SPLITTING=10
                                                        ! else    remap with mpi_alltoallv <=> NZ_SPLITTING=14 ( BG/MPICH optimization )
INTEGER,SAVE      :: NZ_SPLITTING   = 10 ! /!\ setting of NZ_SPLITTING by namelist for 'EXPERT' use only for DEBUG 
                                         ! 'STANDARD' user use LMNH_MPI_ALLTOALLV_REMAP=T/F only !!!
                                         !  IZ=1=flat_inv;  IZ=2=flat_invz ;  IZ=1+2=the two ; IZ=4 alltoall ; +8=P1/P2 splitting
!JUAN
!
END MODULE MODD_CONFZ
