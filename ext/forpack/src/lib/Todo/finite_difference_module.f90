Module Finite_Difference_Module

  use Parameters_Module         ,only:  rkp

  implicit none

  private

  public  ::  FiniteDifference_2ndOrderCentered

  Interface             FiniteDifference_2ndOrderCentered
    Module Procedure    FiniteDifference_2ndOrderCentered_1D, FiniteDifference_2ndOrderCentered_2D
  End Interface

  contains

Function FiniteDifference_2ndOrderCentered_1D( x, y ) result(dydx)
  real(rkp)     ,dimension( : )                         ,intent(in)     ::  x
  real(rkp)     ,dimension( : )                         ,intent(in)     ::  y
  real(rkp)     ,dimension( size(y,1) )                                 ::  dydx
  integer                                                               ::  i
! A forward finite differece 1st order scheme is used for the first point
  i             =       1
  dydx(i)       =       ( y(i+1) - y(i) ) / ( x(i+1) - x(i) )
! A backward finite differece 1st order scheme is used for the last point
  i             =       size(x,1)
  dydx(i)       =       ( y(i) - y(i-1) ) / ( x(i) - x(i-1) )
! A centered 2nd order scheme is used for internal points
  do i = 2,size(x,1)-1
    dydx(i)     =       0.5_rkp * ( (y(i)-y(i-1))/(x(i)-x(i-1)) + (y(i+1)-y(i))/(x(i+1)-x(i)) )
  end do
End Function

Function FiniteDifference_2ndOrderCentered_2D( x, y ) result(dydx)
  real(rkp)     ,dimension( : )                         ,intent(in)     ::  x
  real(rkp)     ,dimension( :, : )                      ,intent(in)     ::  y
  real(rkp)     ,dimension( size(y,1), size(y,2) )                      ::  dydx
  integer                                                               ::  i
! A forward finite differece 1st order scheme is used for the first point
  i             =       1
  dydx(:,i)     =       ( y(:,i+1) - y(:,i) ) / ( x(i+1) - x(i) )
! A backward finite differece 1st order scheme is used for the last point
  i             =       size(x,1)
  dydx(:,i)     =       ( y(:,i) - y(:,i-1) ) / ( x(i) - x(i-1) )
! A centered 2nd order scheme is used for internal points
  do i = 2,size(x,1)-1
    dydx(:,i)   =       0.5_rkp * ( (y(:,i)-y(:,i-1))/(x(i)-x(i-1)) + (y(:,i+1)-y(:,i))/(x(i+1)-x(i)) )
  end do
End Function

End Module