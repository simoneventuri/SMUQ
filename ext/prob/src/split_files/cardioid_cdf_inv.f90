subroutine cardioid_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! CARDIOID_CDF_INV inverts the Cardioid CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0 <= CDF <= 1.
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) X, the argument with the given CDF.
!    A - PI <= X <= A + PI.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) fp
  real ( kind = 8 ) fx
  integer ( kind = 4 ) it
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ), parameter :: tol = 0.000001D+00
  real ( kind = 8 ) x

  if ( cdf <= 0.0D+00 ) then

    x = a - r8_pi

  else if ( cdf < 1.0D+00 ) then

    x = a

    it = 0

    do

      fx = cdf - ( r8_pi + x - a + 2.0D+00 * b * sin ( x - a ) ) &
        / ( 2.0D+00 * r8_pi )

      if ( abs ( fx ) < tol ) then
        exit
      end if

      if ( 10 < it ) then
        stop 1
      end if

      fp = - ( 1.0D+00 + 2.0D+00 * b * cos ( x - a ) ) / ( 2.0D+00 * r8_pi )

      x = x - fx / fp
      x = max ( x, a - r8_pi )
      x = min ( x, a + r8_pi )

      it = it + 1

    end do

  else

    x = a + r8_pi

  end if

  return
end
