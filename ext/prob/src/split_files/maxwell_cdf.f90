subroutine maxwell_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! MAXWELL_CDF evaluates the Maxwell CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) p2
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( x <= 0.0D+00 ) then

    cdf = 0.0D+00

  else

    x2 = x / a
    p2 = 1.5D+00

    cdf = r8_gamma_inc ( p2, x2 )

  end if

  return
end
