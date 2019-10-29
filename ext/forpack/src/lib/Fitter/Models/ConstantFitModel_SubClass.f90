SubModule(ConstantFitModel_Class) ConstantFitModel_SubClass

  implicit none

  character(*)    ,parameter    ::  Model_Name    =   "Constant"
  integer         ,parameter    ::  Model_NParam  =   1

  contains

! This procedure sets the name of the current FitModel object.
Module Procedure Get_Name
  Name      =     Model_Name                                                                                    ! Setting the name of the model
End Procedure

! This procedure sets the number of parameters of the current FitModel object.
Module Procedure Get_NParam
  NParam    =     Model_NParam                                                                                  ! Setting the number of parameters of the model
End Procedure

! This procedure evaluates the values of the current FitModel object.
Module Procedure Evaluate_0d
  if ( size(Param) < Model_NParam ) then                                                                        !< If the input Param array is too small, then ...
    y   =   0.0_rkp                                                                                             !< ... Set the fonction value to 0 ...
  else                                                                                                          !< ... otherwise
    y   =     Param(1)                                                                                          !< ... evaluate the fonction using the input x value and parameters
  end if
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! This matrix corresponds to a MxN matrix where:
! * M: Number of fitting positions (size of the input 'x')
! * N: Number of model parameters (number of unknows).
! The Constant fonction is:
!     f(x) = y0
! This Module Function is linear and has a single parameter x(1) = y0.
Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
  class(ConstantFitModel_Type)                          ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
  real(rkp)                                                             ::  rdum
  A_Row(1)  =     1.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  return
  rdum = x  ! Just to avoid un-used argument
End Function

! This procedure computes the column vector associated to the RHS vector b of the linear system A x = b.
Module Procedure Get_RHS_Vector
  b         =     y                                                                                             ! Setting the RHS vector
End Procedure

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
  class(ConstantFitModel_Type)                          ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
  real(rkp)                                                             ::  rdum
  dAdx_Row(1) =   0.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  return
  rdum = x  ! Just to avoid un-used argument
End Function

End SubModule