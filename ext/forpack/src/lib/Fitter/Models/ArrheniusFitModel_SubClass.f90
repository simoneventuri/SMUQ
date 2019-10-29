SubModule(ArrheniusFitModel_Class) ArrheniusFitModel_SubClass

  implicit none

  character(*)    ,parameter    ::  Model_Name    =   "Arrhenius"
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
    y   =     Param(1) * x**Param(2) * exp( - Param(3) / x )                                                    !< ... evaluate the fonction using the input x value and parameters
  end if
End Procedure

! This procedure computes the column vector associated to the RHS vector b of the linear system A x = b.
Module Procedure Get_RHS_Vector
  b         =     log(y)                                                                                        ! Setting the RHS vector: The log is taken in order to obtain a linear system
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! This matrix corresponds to a MxN matrix where:
! * M: Number of fitting positions (size of the input 'x')
! * N: Number of model parameters (number of unknows).
! The Arrhenius fonction is:
!     K(T) = x(1) * T**(X(2)) * exp( - x(3) / T )
! This Module Function is non-linear but can be easily linearized by taking its log:
!     log[ K(T) ] = log[ x(1) ] + log [ T**(x(2)) ] + log[ exp( - x(3) / T ) ]
!     log[ K(T) ] = z(1) * [    1   ]
!                 + z(2) * [ log(T) ]
!                 + z(3) * [  - 1/T ]

! This Module Function is non-linear but can be easily linearized by taking its log:
!     log[ K(T) ]  =  /   log[ x(1) ]          \  =  /    z(1) * [    1   ] \
!                     | + log[ T**(x(2)) ]     |     |  + z(2) * [ log(T) ] |
!                     \ + log[ exp(-x(3)/T ) ] /     \  + z(3) * [  - 1/T ] /
!
! This new expression is linear and corresponds to the function:
!     yi = f(xi)    with      yi = log( K(Ti) )     and   xi = Ti
! the parameters being : [ 1 , log(T) , -1/T ]
! The parameters z(i) to be computed are related to the Arrhenius parameters by:
!     x(1) = exp( z(1) )
!     x(2) = z(2)
!     x(3) = z(3)
! Once the linear least square system has been solved for the z(i), the parameters
! x(i) are then computed using the 'DeLinearize' procedure.
Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
  class(ArrheniusFitModel_Type)                         ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
  A_Row(1)  =     1.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  A_Row(2)  =     log(x)                                                                                        ! Setting column 2 of the LHS matrix
  A_Row(3)  =     - 1.0_rkp / x                                                                                 ! Setting column 3 of the LHS matrix
End Function

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
  class(ArrheniusFitModel_Type)                         ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
  dAdx_Row(1) =   0.0_rkp                                                                                       ! Setting column 1 of the LHS matrix
  dAdx_Row(2) =   1.0_rkp / X                                                                                   ! Setting column 2 of the LHS matrix
  dAdx_Row(3) = - 1.0_rkp / X**2                                                                                ! Setting column 3 of the LHS matrix
End Function

Module Procedure DeLinearize
  This%Param(1)     =     exp( This%Param(1) )                                                                  ! Modifying the 1st parameter to go back to the Arrhenius expression
End Procedure

End SubModule