Module Coarray_Collective_Module

! Include file needed for: _ASSUMED_CODIM_
# include "forpack-include.inc"

  use iso_fortran_env ,only:  INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

  implicit none

  private
  public  ::  CoMinVal
  public  ::  CoMaxVal
  public  ::  CoMinLoc
  public  ::  CoMaxLoc
  public  ::  CoNorm2
  public  ::  CoSum
  public  ::  coAny

  Interface             CoMinVal
    Module Procedure    CoMinVal_REAL64_Rank0_CoRank1,      CoMinVal_REAL64_Rank1_CoRank1
    Module Procedure    CoMinVal_Integer_Rank0_CoRank1,   CoMinVal_Integer_Rank1_CoRank1
  End Interface

  Interface             CoMaxVal
    Module Procedure    CoMaxVal_REAL64_Rank0_CoRank1,      CoMaxVal_REAL64_Rank1_CoRank1
    Module Procedure    CoMaxVal_Integer_Rank0_CoRank1,   CoMaxVal_Integer_Rank1_CoRank1
  End Interface

  Interface             CoMinLoc
    Module Procedure    CoMinLoc_Rank0_CoRank1
  End Interface

  Interface             CoMaxLoc
    Module Procedure    CoMaxLoc_Rank0_CoRank1
  End Interface

  Interface             CoNorm2
    Module Procedure    CoNorm2_Rank0_CoRank1,    CoNorm2_Rank1_CoRank1
  End Interface

  Interface             CoSum
    Module Procedure    CoSum_Integer_Rank0_CoRank1,    CoSum_Integer_Rank1_CoRank1
    Module Procedure    CoSum_REAL64_Rank0_CoRank1,       CoSum_REAL64_Rank1_CoRank1
  End Interface

  Interface             coAny
    Module Procedure    coAny_Logical_Rank0_CoRank1, coAny_Logical_Rank1_CoRank1
  End Interface

  Interface
    ! ==============================================================================================================
    !    CoMinVal
    ! ==============================================================================================================
    Pure Module Function CoMinVal_REAL64_Rank0_CoRank1( CoVar ) result(VarMin)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose minimum value has to be found
      real(8)                                                               ::  VarMin                          ! Minimum value of the input coarray variable
    End Function
    Pure Module Function CoMinVal_REAL64_Rank1_CoRank1( CoVar ) result(VarMin)
      real(8)       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable whose minimum value has to be found
      real(8)                                                               ::  VarMin                          ! Minimum value of the input coarray variable
    End Function
    Pure Module Function CoMinVal_Integer_Rank0_CoRank1( CoVar ) result(VarMin)
      integer       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose minimum value has to be found
      integer                                                               ::  VarMin                          ! Minimum value of the input coarray variable
    End Function
    Pure Module Function CoMinVal_Integer_Rank1_CoRank1( CoVar ) result(VarMin)
      integer       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable whose minimum value has to be found
      integer                                                               ::  VarMin                          ! Minimum value of the input coarray variable
    End Function
    ! ==============================================================================================================
    !    CoMaxVal
    ! ==============================================================================================================
    Pure Module Function CoMaxVal_REAL64_Rank0_CoRank1( CoVar ) result(VarMax)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose maximum value has to be found
      real(8)                                                               ::  VarMax                          ! Maximum value of the input coarray variable
    End Function
    Pure Module Function CoMaxVal_REAL64_Rank1_CoRank1( CoVar ) result(VarMax)
      real(8)       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable whose maximum value has to be found
      real(8)                                                               ::  VarMax                          ! Maximum value of the input coarray variable
    End Function
    Pure Module Function CoMaxVal_Integer_Rank0_CoRank1( CoVar ) result(VarMax)
      integer       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose maximum value has to be found
      integer                                                               ::  VarMax                          ! Maximum value of the input coarray variable
    End Function
    Pure Module Function CoMaxVal_Integer_Rank1_CoRank1( CoVar ) result(VarMax)
      integer       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable whose maximum value has to be found
      integer                                                               ::  VarMax                          ! Maximum value of the input coarray variable
    End Function
    ! ==============================================================================================================
    !    CoMinLoc
    ! ==============================================================================================================
    Pure Module Function CoMinLoc_Rank0_CoRank1( CoVar ) result(iImg)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose image index of the minimum value has to be found
      integer                                                               ::  iImg                            ! Index of the image where the minimum value of the input coarray variable is found
    End Function
    ! ==============================================================================================================
    !    CoMaxLoc
    ! ==============================================================================================================
    Pure Module Function CoMaxLoc_Rank0_CoRank1( CoVar ) result(iImg)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose image index of the maximum value has to be found
      integer                                                               ::  iImg                            ! Index of the image where the maximum value of the input coarray variable is found
    End Function
    ! ==============================================================================================================
    !    CoSum
    ! ==============================================================================================================
    Pure Module Function CoSum_Integer_Rank0_CoRank1( CoVar ) result(SumVar)
      integer       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable to be summed
      integer                                                               ::  SumVar                          ! Sum of the input variable
    End Function
    Pure Module Function CoSum_Integer_Rank1_CoRank1( CoVar ) result(SumVar)
      integer       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable to be summed
      integer                                                               ::  SumVar                          ! Sum of the input variable
    End Function
    Pure Module Function CoSum_REAL64_Rank0_CoRank1( CoVar ) result(SumVar)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable to be summed
      real(8)                                                               ::  SumVar                          ! Sum of the input variable
    End Function
    Pure Module Function CoSum_REAL64_Rank1_CoRank1( CoVar ) result(SumVar)
      real(8)       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable to be summed
      real(8)                                                               ::  SumVar                          ! Sum of the input variable
    End Function
    ! ==============================================================================================================
    !    CoNorm2
    ! ==============================================================================================================
    Pure Module Function CoNorm2_Rank0_CoRank1( CoVar ) result(VarL2)
      real(8)       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray variable whose L2 norm has to be computed
      real(8)                                                               ::  VarL2                           ! L2 norm
    End Function
    Pure Module Function CoNorm2_Rank1_CoRank1( CoVar ) result(VarL2)
      real(8)       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray variable whose L2 norm has to be computed
      real(8)                                                               ::  VarL2                           ! L2 norm
    End Function
    ! ==============================================================================================================
    !    CoAny
    ! ==============================================================================================================
    Pure Module Function coAny_Logical_Rank0_CoRank1( CoVar ) result(Indicator)
      logical       _ASSUMED_CODIM_                         ,intent(in)     ::  CoVar                           ! Coarray logical variable
      logical                                                               ::  Indicator                       !
    End Function
    Pure Module Function coAny_Logical_Rank1_CoRank1( CoVar ) result(Indicator)
      logical       _ASSUMED_CODIM_ ,dimension(:)           ,intent(in)     ::  CoVar                           ! Coarray logical variable
      logical                                                               ::  Indicator                       !
    End Function

  End Interface

End Module