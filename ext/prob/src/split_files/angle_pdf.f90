subroutine angle_pdf ( x, n, pdf )

!*****************************************************************************80
!
!! ANGLE_PDF evaluates the Angle PDF.
!
!  Discussion:
!
!    X is an angle between 0 and PI, corresponding to the angle
!    made in an N-dimensional space, between a fixed line passing
!    through the origin, and an arbitrary line that also passes
!    through the origin, which is specified by a choosing any point
!    on the N-dimensional sphere with uniform probability.
!
!    The formula is
!
!      PDF(X) = ( sin ( X ) )^(N-2) * Gamma ( N / 2 )
!               / ( sqrt ( PI ) * Gamma ( ( N - 1 ) / 2 ) )
!
!      PDF(X) = 1 / PI if N = 2.
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
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( n < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ANGLE_PDF - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 2.'
    write ( *, '(a,i8)' ) '  The input value of N = ', n
    stop 1
  end if

  if ( x < 0.0D+00 .or. r8_pi < x ) then
    pdf = 0.0D+00
  else if ( n == 2 ) then
    pdf = 1.0D+00 / r8_pi
  else
    pdf = &
      ( sin ( x ) ) ** ( n - 2 ) * gamma ( real ( n, kind = 8 ) / 2.0D+00 ) &
      / ( sqrt ( r8_pi ) * gamma ( real ( n - 1, kind = 8 ) / 2.0D+00 ) )
  end if

  return
end
