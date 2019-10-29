SubModule(Poly9thOrderFitModel_Class) Poly9thOrderFitModel_SubClass

  implicit none

  character(*)    ,parameter    ::  Model_Name    =   "Poly9thOrder"
  integer         ,parameter    ::  Model_NParam  =   9
  real(rkp)       ,parameter    ::  T0 = 1.0E-03_rkp

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
  real(rkp) ,dimension(Model_NParam)                                    ::  Vec
  if ( size(Param) < Model_NParam ) then                                                                        !< If the input Param array is too small, then ...
    y   =   0.0_rkp
    return
  end if
  call GetVariableVector( x, Vec )
  y       =  exp( dot_product( Param , Vec ) )
End Procedure

! This procedure computes the column vector associated to the RHS vector b of the linear system A x = b.
Module Procedure Get_RHS_Vector
  b         =     log(y)                                                                                        ! Setting the RHS vector: The log is taken in order to obtain a linear system
End Procedure

! This procedure compute the row-vector of a given row of the LHS matrix A of the linear system 'A x = b'.
! This matrix corresponds to a MxN matrix where:
! * M: Number of fitting positions (size of the input 'x')
! * N: Number of model parameters (number of unknows).

! The 9th order polynomial fonction is:
!     K(T) = exp[ x(1) * Tp^-(3) + x(2) * Tp^-(2) + x(3) * Tp^-(1) + x(4) * log(Tp) + x(5) + x(6) * Tp + x(7) * Tp^2 + x(8) * Tp^3 + x(9) * Tp^4 ]
! with Tp = T/T0.
! This function is non-linear but can be easily linearized by taking its log:
!     log[ K(T) ]  =  /   x(1) * Tp^-(3) \  =  /   z(1) * [ Tp^-(3) ] \
!                     | + x(2) * Tp^-(2) |     | + z(2) * [ Tp^-(2) ] |
!                     | + x(3) * Tp^-(1) |     | + z(3) * [ Tp^-(1) ] |
!                     | + x(4) * log(Tp) |     | + z(4) * [ log(Tp) ] |
!                     | + x(5)           |     | + z(5) * [    1    ] |
!                     | + x(6) * Tp      |     | + z(6) * [   Tp    ] |
!                     | + x(7) * Tp^2    |     | + z(7) * [   Tp^2  ] |
!                     | + x(8) * Tp^3    |     | + z(8) * [   Tp^3  ] |
!                     \ + x(9) * Tp^4    /     \ + z(9) * [   Tp^4  ] /
!
! This new expression is linear in z and corresponds to the function:
!     yi = f(xi)    with      yi = log( K(Ti) )     and   xi = Ti/T0
! the parameters being : [ Tp^-(3) , Tp^-(2) , Tp^-(1) , log(Tp) , 1 , Tp , Tp^2 , Tp^3 , Tp^4 ]
! Once the linear least square system has been solved for the z(i), the parameters
! x(i) are then computed using the 'DeLinearize' procedure.
Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
  class(Poly9thOrderFitModel_Type)                      ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
  call GetVariableVector( x, A_Row )
End Function

! This procedure is used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
  class(Poly9thOrderFitModel_Type)                      ,intent(in)     ::  This                              !< Passed-object dummy argument
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
  real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
  real(rkp)                                                             ::  x_, x_p2, x_p3, x_p4, x_m1, x_m2, x_m3, x_m4

  x_      =   x    * T0
  x_p2    =   x_   * x_
  x_p3    =   x_p2 * x_
  x_p4    =   x_p3 * x_
  x_m1    =   1.0_rkp / x_
  x_m2    =   x_m1 * x_m1
  x_m3    =   x_m2 * x_m1
  x_m4    =   x_m3 * x_m1

  dAdx_Row(1)  =  - 3.0_rkp * x_m4 !   Tp^-(3)
  dAdx_Row(2)  =  - 2.0_rkp * x_m3 !   Tp^-(2)
  dAdx_Row(3)  =  - 1.0_rkp * x_m2 !   Tp^-(1)
  dAdx_Row(4)  =    1.0_rkp * x_m1 !   log(Tp)
  dAdx_Row(5)  =    0.0_rkp        !   1
  dAdx_Row(6)  =    1.0_rkp        !   Tp
  dAdx_Row(7)  =    2.0_rkp * x_   !   Tp^2
  dAdx_Row(8)  =    3.0_rkp * x_p2 !   Tp^3
  dAdx_Row(9)  =    4.0_rkp * x_p3 !   Tp^4

  dAdx_Row    =   dAdx_Row * T0
End Function


! This procedure evaluates the values of the current FitModel object.
Pure Subroutine GetVariableVector( x, Vec )
  real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates (Temperature)
  real(rkp) ,dimension(Model_NParam)                    ,intent(out)    ::  Vec
  real(rkp)                                                             ::  x_, x_p2, x_p3, x_p4, x_m1, x_m2, x_m3

  x_      =   x    * T0
  x_p2    =   x_   * x_
  x_p3    =   x_p2 * x_
  x_p4    =   x_p3 * x_
  x_m1    =   1.0_rkp / x_
  x_m2    =   x_m1 * x_m1
  x_m3    =   x_m2 * x_m1

  Vec(1)  =   x_m3      ! Tp^-(3)
  Vec(2)  =   x_m2      ! Tp^-(2)
  Vec(3)  =   x_m1      ! Tp^-(1)
  Vec(4)  =   log(x_)   ! log(Tp)
  Vec(5)  =   1.0_rkp   ! 1
  Vec(6)  =   x_        ! Tp
  Vec(7)  =   x_p2      ! Tp^2
  Vec(8)  =   x_p3      ! Tp^3
  Vec(9)  =   x_p4      ! Tp^4

End Subroutine

End SubModule