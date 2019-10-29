SubModule(QuadraticFitModel_Class) QuadraticFitModel_SubClass

  implicit none

  character(*)    ,parameter    ::  Model_Name    =   "Quadratic"
  integer         ,parameter    ::  Model_NParam  =   3

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
    y   =     Param(1) + Param(2) * x + Param(2) * x**2                                                         !< ... evaluate the fonction using the input x value and parameters
  end if
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! This matrix corresponds to a MxN matrix where:
! * M: Number of fitting positions (size of the input 'x')
! * N: Number of model parameters (number of unknows).
Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
  class(QuadraticFitModel_Type)                         ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
  A_Row(1)  =     1.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  A_Row(2)  =     x                                                                                             ! Setting column 2 of the LHS matrix
  A_Row(3)  =     x**2                                                                                          ! Setting column 3 of the LHS matrix
End Function

! This procedure computes the column vector associated to the RHS vector b of the linear system A x = b.
Module Procedure Get_RHS_Vector
  b         =     y                                                                                             ! Setting the RHS vector: The log is taken in order to obtain a linear system
End Procedure

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
  class(QuadraticFitModel_Type)                         ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
  dAdx_Row(1) =   0.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  dAdx_Row(2) =   1.0_rkp                                                                                       ! Setting column 2 of the LHS matrix
  dAdx_Row(3) =   2.0_rkp * X                                                                                   ! Setting column 3 of the LHS matrix
End Function

End SubModule