SubModule(FitModel_Class) FitModel_SubClass

  implicit none

  contains

! This procedure initializes a FitModel object. The name of the model and its number of parameters are given
! by the extended object through the deferred type-bound procedures 'Get_Name' and 'Get_NParam'.
! Once the number of parameters is known, the component 'Param' which stores the value of these parameters
! is allocated to the correct dimension.
! In addition, an initial value can by assigned to these parameters using the optional input argument 'Param'.
! The assignment is only done if the number of element of the optional input array 'Param' is equal to the
! number of parameters.
Module Procedure InitializeFitModel
  call This%Free()
  This%Name     =     This%Get_Name()                                                                           ! Setting the name of the FitModel object
  This%NParam   =     This%Get_NParam()                                                                         ! Setting the number of parameters of the FitModel object
  allocate( This%Param(This%NParam) )                                                                           ! Allocating the component storing the parameters of the FitModel
  This%Param    =     0.0_rkp
  if ( present(Param) ) then
    if ( size(Param) >= This%NParam ) then
      This%Param            =   Param(1:This%NParam)
      This%Param_Defined    =   .True.
    end if
  end if
  This%Initialized          =   .True.
End Procedure

Module Procedure FreeFitModel
  This%Initialized    =   .False.
  This%Param_Defined  =   .False.
  This%NParam         =   0
  if ( allocated(This%Name) )   deallocate(This%Name)
  if ( allocated(This%Param) )  deallocate(This%Param)
End Procedure

! This procedure returns the LHS matrix of the linear system 'A x = b'.
! This matrix is a M-by-N matrix where M is the number of fitting points (x positions) and N is the number
! of model parameters. The output matrix is allocated to the correct dimension inside the current procedure.
! Once allocated, the row-vectors are computed one-by-one by looping over the matrix rows.
! The computation of the row-vectors is done by calling the 'Get_LHS_RowVector' deferred type-bound procedure,
! which is called using the generic binding 'Get_LHS_Vector'.
Module Procedure Compute_LHS_Matrix
  integer                                                               ::  M                                 ! Number of fitting positions
  integer                                                               ::  N                                 ! Number of model parameters
  integer                                                               ::  i                                 ! Index of the row of the matrix A
  M       =     size(x)                                                                                         ! Setting the number of fitting positions
  N       =     This%Get_NParam()                                                                               ! Setting the number of model parameters
  allocate( A(M,N) )                                                                                            ! Allocating the LHS matrix
  do i = 1,M                                                                                                    ! Loop on all the matrix rows (Each one corresponds to a fitting position)
    A(i,:)      =     This%Get_LHS_RowVector( x(i) )                                                                ! Computing the vector corresponding to the current row
  end do                                                                                                        ! End loop on the matrix rows
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! The only difference with the 'Get_LHS_RowVector' deferred type-bound procedure is that the output variable
! storing the row-vector 'A_Row' has the 'allocatable' attribute.
! This way the calling procedure does not has to worry about the size of the row-vector, this variable being
! automatically allocated to the correct dimension inside the current procedure.
! Once the variable is allocated, the 'Get_LHS_RowVector' deferred type-bound procedure is called through the
! generic binding 'Get_LHS_Vector' to actually compute the elements of the row-vector.
Module Procedure Compute_LHS_RowVector
!   allocate( A_Row( This%Get_NParam() ) )                                                                        ! Allocating the LHS row-vector
!   A_Row   =   This%Get_LHS_RowVector( x )                                                                       ! Computing the LHS row-vector
  allocate( A_Row , source = This%Get_LHS_RowVector( x ) )                                                      ! Computing the LHS row-vector
End Procedure


! This procedure sets the coefficients of the RHS vector b of the linear system A x = b
Module Procedure Compute_RHS_Vector
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  allocate( b(size(y)) )
  b(:)   =   This%Get_RHS_Vector(y)
!   allocate( b(size(y)), source = This%Get_RHS_Vector(y) )
#else
  allocate( b, source = This%Get_RHS_Vector(y) )                                                                ! Setting the RHS vector
#endif
End Procedure

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Module Procedure Compute_LHS_Jacobian_Vector
  allocate( dAdx_Row , source = This%Get_LHS_RowVector_Jacobian( x ) )                                          ! Computing the jacobian of the LHS row-vector
End Procedure



! By default, no modification is done on the fitting coefficients
Module Procedure DeLinearize
End Procedure

! This procedure sets the parameters of the FitModel object.
! The size of the input array of parameters is check in order to ensure that it is compatible with the number
! of parameters of the concret 'FitModel' calling this procedure. If an inconsistency is found, then the
! Status varaible is set to 1.
Module Procedure Set_Param
  integer                                                               ::  Status_Local
  if ( size(Param) /= This%Get_NParam() ) then
    Status_Local  =   1
  else
    Status_Local  =   0
  end if
  if ( .Not. allocated(This%Param) ) then
    allocate( This%Param , source = Param )
  else
    This%Param  =   Param
  end if
  This%Param_Defined  = .True.
  if ( present(Status) ) Status = Status_Local
End Procedure

Module Procedure Evaluate_NoParam_0d
  y       =     This%Eval( x, This%Param )
End Procedure

Module Procedure Consistent_Param
!   integer                                                               ::  NParam
  Consistent    =       ( size(Param) >= This%Get_NParam() )
!   NParam    =   This%Get_NParam()
!   Consistent    =       ( size(Param) >= NParam )
End Procedure

! @COMPILER_BUG gcc-6.1.3/intel-17.0.1: If this procedure is truned to a 'Module Procedure/End Procedure', then there is an ICE. I guess this in connected to the use of 'size(X)' in the declaration of an argument
Pure Module Function Evaluate_NoParam_1d( This, x ) result(y)
  class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 ! X coordinates (DIM=NPtsFit)
  real(rkp)     ,dimension(size(X))                                     ::  y                                 ! Y coordinates to be computed (DIM=NPtsFit)
  integer                                                               ::  i
  do i = 1,size(x)
    y(i)  =     This%Eval( x(i), This%Param )
  end do
End Function

! @COMPILER_BUG gcc-6.1.3/intel-17.0.1: If this procedure is truned to a 'Module Procedure/End Procedure', then there is an ICE. I guess this in connected to the use of 'size(X)' in the declaration of an argument
Pure Module Function Evaluate_1d( This, x, Param ) result(y)
  class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 ! X coordinates (DIM=NPtsFit)
  real(rkp)     ,dimension(:)                           ,intent(in)     ::  Param
  real(rkp)     ,dimension(size(X))                                     ::  y                                 ! Y coordinates to be computed (DIM=NPtsFit)
  integer                                                               ::  i
  integer                                                               ::  NParam
!   if ( size(Param) < This%Get_NParam() ) then ! @COMPILER_BUG: gcc-6.3.1: "Error: ‘get_nparam’ at (1) should be a FUNCTION"... but it i
  NParam    =   This%Get_NParam()
  if ( size(Param) < NParam ) then
    y     =     0.0_rkp
  else
    do i = 1,size(x)
      y(i)  =     This%Eval( x(i), Param )
    end do
  end if
End Function

! @COMPILER_BUG gcc-6.1.3/intel-17.0.1: If this procedure is truned to a 'Module Procedure/End Procedure', then there is an ICE. I guess this in connected to the use of 'size(X)' in the declaration of an argument
Pure Module Function Get_Param( This ) result(Param)
  class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)   ,dimension( This%NParam )                                 ::  Param                             !< Array of parameters associated to current FitModel
  Param   =   This%Param
End Function

End SubModule