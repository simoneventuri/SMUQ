function r8_factorial_log ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL_LOG returns the logarithm of N factorial.
!
!  Discussion:
!
!    N! = Product ( 1 <= I <= N ) I
!
!    N! = Gamma(N+1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the function.
!    0 <= N.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL_LOG, the logarithm of N!.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial_log

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_FACTORIAL_LOG - Fatal error!'
    write ( *, '(a)' ) '  N < 0.'
    stop 1
  end if

  r8_factorial_log = 0.0D+00

  do i = 2, n
    r8_factorial_log = r8_factorial_log + log ( real ( i, kind = 8 ) )
  end do

  return
end
