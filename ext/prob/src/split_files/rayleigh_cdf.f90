subroutine rayleigh_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! RAYLEIGH_CDF evaluates the Rayleigh CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 <= X.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    cdf = 0.0D+00
  else
    cdf = 1.0D+00 - exp ( - x ** 2 / ( 2.0D+00 * a ** 2 ) )
  end if

  return
end
