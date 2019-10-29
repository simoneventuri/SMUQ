SubModule(Coarray_Collective_Module) Coarray_Collective_SubModule

  implicit none
!
!     co_all(mask[,team])         : True if all values are true
!     co_any(mask[,team])         : True if any value is true
!     co_count(mask[,team,kind])  : Numbers of true elements
!     co_maxloc(array[,team])     : Image indices of maximum values
!     co_maxval(array[,team])     : Maximum values
!     co_minloc(array[,team])     : Image indices of minimum values
!     co_minval(array[,team])     : Minimum values
!     co_product(array[,team])    : Products of elements
!     co_sum(array[,team])        : Sums of elements

  contains

! ==============================================================================================================
!    CoMinVal
! ==============================================================================================================

Module Procedure CoMinVal_REAL64_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMin        =       CoVar                                                                                   ! Copying the local value of the variable coarray
  do iImg = This_image()+1,Num_images()
    VarMin      =       min( VarMin, CoVar[iImg] )
  end do
  do iImg = 1,This_image()-1
    VarMin      =       min( VarMin, CoVar[iImg] )
  end do
#else
  VarMin        =       CoVar                                                                                   ! Copying the local value of the variable coarray
#endif
End Procedure

Module Procedure CoMinVal_REAL64_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMin        =       minval(CoVar)
  do iImg = This_image()+1,Num_images()
    VarMin      =       min( VarMin, minval(CoVar(:)[iImg]) )
  end do
  do iImg = 1,This_image()-1
    VarMin      =       min( VarMin, minval(CoVar(:)[iImg]) )
  end do
#else
  VarMin        =       minval(CoVar)
#endif
End Procedure

Module Procedure CoMinVal_Integer_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMin        =       CoVar                                                                                   ! Copying the local value of the variable coarray
  do iImg = This_image()+1,Num_images()
    VarMin      =       min( VarMin, CoVar[iImg] )
  end do
  do iImg = 1,This_image()-1
    VarMin      =       min( VarMin, CoVar[iImg] )
  end do
#else
  VarMin        =       CoVar                                                                                   ! Copying the local value of the variable coarray
#endif
End Procedure

Module Procedure CoMinVal_Integer_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMin        =       minval(CoVar)
  do iImg = This_image()+1,Num_images()
    VarMin      =       min( VarMin, minval(CoVar(:)[iImg]) )
  end do
  do iImg = 1,This_image()-1
    VarMin      =       min( VarMin, minval(CoVar(:)[iImg]) )
  end do
#else
  VarMin        =       minval(CoVar)
#endif
End Procedure

! ==============================================================================================================
!    CoMaxVal
! ==============================================================================================================

Module Procedure CoMaxVal_REAL64_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMax        =       CoVar                                                                                   ! Copying the local value of the variable coarray
  do iImg = This_image()+1,Num_images()
    VarMax      =       max( VarMax, CoVar[iImg] )
  end do
  do iImg = 1,This_image()-1
    VarMax      =       max( VarMax, CoVar[iImg] )
  end do
#else
  VarMax        =       CoVar                                                                                   ! Copying the local value of the variable coarray
#endif
End Procedure

Module Procedure CoMaxVal_REAL64_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMax        =       maxval(CoVar)
  do iImg = This_image()+1,Num_images()
    VarMax      =       max( VarMax, maxval(CoVar(:)[iImg]) )
  end do
  do iImg = 1,This_image()-1
    VarMax      =       max( VarMax, maxval(CoVar(:)[iImg]) )
  end do
#else
  VarMax        =       maxval(CoVar)
#endif
End Procedure

Module Procedure CoMaxVal_Integer_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMax        =       CoVar                                                                                   ! Copying the local value of the variable coarray
  do iImg = This_image()+1,Num_images()
    VarMax      =       max( VarMax, CoVar[iImg] )
  end do
  do iImg = 1,This_image()-1
    VarMax      =       max( VarMax, CoVar[iImg] )
  end do
#else
  VarMax        =       CoVar                                                                                   ! Copying the local value of the variable coarray
#endif
End Procedure

Module Procedure CoMaxVal_Integer_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  VarMax        =       maxval(CoVar)
  do iImg = This_image()+1,Num_images()
    VarMax      =       max( VarMax, maxval(CoVar(:)[iImg]) )
  end do
  do iImg = 1,This_image()-1
    VarMax      =       max( VarMax, maxval(CoVar(:)[iImg]) )
  end do
#else
  VarMax        =       maxval(CoVar)
#endif
End Procedure

! ==============================================================================================================
!    CoMinLoc
! ==============================================================================================================

! This procedure gets the index of the image where the minimum value of the input coarray variable is located
Module Procedure CoMinLoc_Rank0_CoRank1
#ifdef COARRAY
  real(8)                                                               ::  VarMin                          ! Minimum value of the input coarray variable
  VarMin        =       CoMinVal( CoVar )                                                                       ! Getting the minimum value
  do iImg = 1,Num_images()
    if ( VarMin == CoVar[iImg]  ) exit
  end do
#else
  iImg  =       1
#endif
End Procedure

! ==============================================================================================================
!    CoMaxLoc
! ==============================================================================================================

! This procedure gets the index of the image where the maximum value of the input coarray variable is located
Module Procedure CoMaxLoc_Rank0_CoRank1
#ifdef COARRAY
  real(8)                                                               ::  VarMax                          ! Maximum value of the input coarray variable
  VarMax        =       CoMaxVal( CoVar )                                                                       ! Getting the maximum value
  do iImg = 1,Num_images()
    if ( VarMax == CoVar[iImg] ) exit
  end do
#else
  iImg  =       1
#endif
End Procedure


! ==============================================================================================================
!    CoSum
! ==============================================================================================================
Module Procedure CoSum_Integer_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  SumVar        =       0
  do iImg = 1,Num_images()
    SumVar      =       SumVar + CoVar[iImg]
  end do
#else
  SumVar        =       CoVar
#endif
End Procedure

Module Procedure CoSum_Integer_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  integer                                                               ::  iElt                            ! Index of elements
  SumVar        =       0
  do iImg = 1,Num_images()
    do iElt = 1,size(CoVar)
      SumVar    =       SumVar + CoVar(iElt)[iImg]
    end do
  end do
#else
  SumVar        =       sum( CoVar )
#endif
End Procedure

Module Procedure CoSum_REAL64_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  SumVar        =       0.0_8
  do iImg = 1,Num_images()
    SumVar      =       SumVar + CoVar[iImg]
  end do
#else
  SumVar        =       CoVar
#endif
End Procedure

Module Procedure CoSum_REAL64_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  integer                                                               ::  iElt                            ! Index of elements
  SumVar        =       0.0_8
  do iImg = 1,Num_images()
    do iElt = 1,size(CoVar)
      SumVar    =       SumVar + CoVar(iElt)[iImg]
    end do
  end do
#else
  SumVar        =       sum( CoVar )
#endif
End Procedure

! ==============================================================================================================
!    CoNorm2
! ==============================================================================================================
! Norm2(X) = sqrt( sum( X(:)**2 ) )
Module Procedure CoNorm2_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  real(8)                                                               ::  SumV2
  SumV2         =       0.0_8
  do iImg = 1,Num_images()
    SumV2       =       SumV2 + CoVar[iImg]**2
  end do
  VarL2         =       sqrt( SumV2 )
#else
  VarL2         =       CoVar
#endif
End Procedure

Module Procedure CoNorm2_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  integer                                                               ::  iElt                            ! Index of elements
  real(8)                                                               ::  SumV2
  SumV2         =       0.0_8
  do iImg = 1,Num_images()
    do iElt = 1,size(CoVar)
      SumV2     =       SumV2 + CoVar(iElt)[iImg]**2
    end do
  end do
  VarL2         =       sqrt( SumV2 )
#else
  VarL2         =       norm2(  CoVar(:) )
#endif
End Procedure


! ==============================================================================================================
!    CoAny
! ==============================================================================================================

Module Procedure coAny_Logical_Rank0_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  Indicator     =       .false.
  do iImg = 1,Num_images()
    if ( .Not. CoVar[iImg] ) cycle
    Indicator =       .true.
    return
  end do
#else
  Indicator        =       CoVar        ! If not a coarray
#endif
End Procedure

Module Procedure coAny_Logical_Rank1_CoRank1
#ifdef COARRAY
  integer                                                               ::  iImg                            ! Index of images
  integer                                                               ::  iElt                            ! Index of elements
  Indicator     =       .false.
  do iImg = 1,Num_images()
    do iElt = 1,size(CoVar)
      if ( .Not. CoVar(iElt)[iImg] ) cycle
      Indicator =       .true.
      return
    end do
  end do
#else
  Indicator        =       any( CoVar )   ! If not a coarray
#endif
End Procedure

End SubModule