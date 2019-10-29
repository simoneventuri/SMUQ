subroutine von_mises_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! VON_MISES_SAMPLE samples the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) f
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) rho
  integer ( kind = 4 ) seed
  real ( kind = 8 ) tau
  real ( kind = 8 ) u1
  real ( kind = 8 ) u2
  real ( kind = 8 ) u3
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  tau = 1.0D+00 + sqrt ( 1.0D+00 + 4.0D+00 * b * b )
  rho = ( tau - sqrt ( 2.0D+00 * tau ) ) / ( 2.0D+00 * b )
  r = ( 1.0D+00 + rho ** 2 ) / ( 2.0D+00 * rho )

  do

    u1 = r8_uniform_01 ( seed )
    z = cos ( r8_pi * u1 )
    f = ( 1.0D+00 + r * z ) / ( r + z )
    c = b * ( r - f )

    u2 = r8_uniform_01 ( seed )

    if ( u2 < c * ( 2.0D+00 - c ) ) then
      exit
    end if

    if ( c <= log ( c / u2 ) + 1.0D+00 ) then
      exit
    end if

  end do

  u3 = r8_uniform_01 ( seed )

  x = a + sign ( 1.0D+00, u3 - 0.5D+00 ) * acos ( f )

  return
end
