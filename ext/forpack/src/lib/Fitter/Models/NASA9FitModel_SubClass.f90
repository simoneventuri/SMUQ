SubModule(NASA9FitModel_Class) NASA9FitModel_SubClass

  implicit none

  character(*)    ,parameter    ::  Model_Name    =   "NASA9"
  integer         ,parameter    ::  Model_NParam  =   7

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
  real(rkp)                                                             ::  x2, x3, x4
  if ( size(Param) < Model_NParam ) then                                                                        !< If the input Param array is too small, then ...
    y   =   0.0_rkp                                                                                             !< ... Set the fonction value to 0 ...
  else                                                                                                          !< ... otherwise
    x2      =     x * x
    x3      =     x * x2
    x4      =     x * x3
    y       =     Param(1) / x2                 &                                                       ! Computing the specific heat at constant pressure [J/kg/K]
            +     Param(2) / x                  &
            +     Param(3)                      &
            +     Param(4) * x                  &
            +     Param(5) * x2                 &
            +     Param(6) * x3                 &
            +     Param(7) * x4
  end if
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! This matrix corresponds to a MxN matrix where:
! * M: Number of fitting positions (size of the input 'x')
! * N: Number of model parameters (number of unknows).
Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
  class(NASA9FitModel_Type)                             ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
  real(rkp)                                                             ::  x2, x3, x4
  x2        =     x * x
  x3        =     x * x2
  x4        =     x * x3
  A_Row(1)  =     1.0_rkp / x2                                                                                  ! Setting column 1 of the LHS matrix
  A_Row(2)  =     1.0_rkp / x                                                                                   ! Setting column 2 of the LHS matrix
  A_Row(3)  =     1.0_rkp                                                                                       ! Setting column 3 of the LHS matrix
  A_Row(4)  =     x                                                                                             ! Setting column 4 of the LHS matrix
  A_Row(5)  =     x2                                                                                            ! Setting column 5 of the LHS matrix
  A_Row(6)  =     x3                                                                                            ! Setting column 6 of the LHS matrix
  A_Row(7)  =     x4                                                                                            ! Setting column 7 of the LHS matrix
End Function

! This procedure computes the column vector associated to the RHS vector b of the linear system A x = b.
Module Procedure Get_RHS_Vector
  b         =     y                                                                                             ! Setting the RHS vector: The log is taken in order to obtain a linear system
End Procedure

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
  class(NASA9FitModel_Type)                             ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
  real(rkp)                                                             ::  x2, x3
  x2        =     x * x
  x3        =     x * x2
  dAdx_Row(1) =   - 2.0_rkp / x3                                                                                ! Setting column 1 of the LHS matrix
  dAdx_Row(2) =   - 1.0_rkp / x2                                                                                ! Setting column 2 of the LHS matrix
  dAdx_Row(3) =   0.0_rkp                                                                                       ! Setting column 3 of the LHS matrix
  dAdx_Row(4) =   1.0_rkp                                                                                       ! Setting column 4 of the LHS matrix
  dAdx_Row(5) =   2.0_rkp * x                                                                                   ! Setting column 5 of the LHS matrix
  dAdx_Row(6) =   3.0_rkp * x2                                                                                  ! Setting column 6 of the LHS matrix
  dAdx_Row(7) =   4.0_rkp * x3                                                                                  ! Setting column 7 of the LHS matrix
End Function

End SubModule