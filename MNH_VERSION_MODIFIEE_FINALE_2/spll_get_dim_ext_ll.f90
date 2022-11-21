!depfile:extern_usersurc_ll.D
!     ######spl
       SUBROUTINE  GET_DIM_EXT_ll( HSPLIT, KXDIM, KYDIM )
!!     ##################################################
!
  USE MODE_TOOLS_ll, ONLY : E_GET_DIM_EXT_ll => GET_DIM_EXT_ll
!
  CHARACTER*1, INTENT(IN) :: HSPLIT
  INTEGER, INTENT(OUT)    :: KXDIM, KYDIM
!
  CALL E_GET_DIM_EXT_ll( HSPLIT, KXDIM, KYDIM )
!

     END SUBROUTINE GET_DIM_EXT_ll
