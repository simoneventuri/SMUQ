function sin_power_int ( a, b, n )

!*****************************************************************************80
!
!! SIN_POWER_INT evaluates the sine power integral.
!
!  Discussion:
!
!    The function is defined by
!
!      SIN_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( sin ( t ))^n dt
!
!    The algorithm uses the following fact:
!
!      Integral sin^n ( t ) = (1/n) * (
!        sin^(n-1)(t) * cos(t) + ( n-1 ) * Integral sin^(n-2) ( t ) dt )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, integer ( kind = 4 ) N, the power of the sine function.
!
!    Output, real ( kind = 8 ) SIN_POWER_INT, the value of the integral.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) ca
  real ( kind = 8 ) cb
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mlo
  integer ( kind = 4 ) n
  real ( kind = 8 ) sa
  real ( kind = 8 ) sb
  real ( kind = 8 ) sin_power_int
  real ( kind = 8 ) value

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SIN_POWER_INT - Fatal error!'
    write ( *, '(a)' ) '  Power N < 0.'
    value = 0.0D+00
    stop 1
  end if

  sa = sin ( a )
  sb = sin ( b )
  ca = cos ( a )
  cb = cos ( b )

  if ( mod ( n, 2 ) == 0 ) then

    value = b - a
    mlo = 2
  else
    value = ca - cb
    mlo = 3
  end if

  do m = mlo, n, 2
    value = ( real ( m - 1, kind = 8 ) * value &
              + sa ** ( m - 1 ) * ca - sb ** ( m - 1 ) * cb ) &
      / real ( m, kind = 8 )
  end do

  sin_power_int = value

  return
end
