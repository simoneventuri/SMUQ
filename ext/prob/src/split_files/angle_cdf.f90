subroutine angle_cdf ( x, n, cdf )

!*****************************************************************************80
!
!! ANGLE_CDF evaluates the Angle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation and Sensitivity of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!    N must be at least 2.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) n
  real ( kind = 8 ) n_real
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) sin_power_int
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: zero = 0.0D+00

  if ( n < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ANGLE_CDF - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 2.'
    write ( *, '(a,i8)' ) '  The input value of N = ', n
    stop 1
  end if

  if ( x <= 0.0D+00 ) then
    cdf = 0.0D+00
  else if ( r8_pi <= x ) then
    cdf = 1.0D+00
  else if ( n == 2 ) then
    cdf = x / r8_pi
  else
    n_real = real ( n, kind = 8 )
    cdf = sin_power_int ( zero, x, n - 2 ) * gamma ( n_real / 2.0D+00 ) &
      / ( sqrt ( r8_pi ) * gamma ( ( n_real - 1.0D+00 ) / 2.0D+00 ) )
  end if

  return
end
