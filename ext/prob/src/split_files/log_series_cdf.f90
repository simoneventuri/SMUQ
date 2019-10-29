subroutine log_series_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! LOG_SERIES_CDF evaluates the Logarithmic Series CDF.
!
!  Discussion:
!
!    Simple summation is used, with a recursion to generate successive
!    values of the PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Thanks:
!
!    Oscar van Vlijmen
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  cdf = 0.0D+00

  do x2 = 1, x

    if ( x2 == 1 ) then
      pdf = - a / log ( 1.0D+00 - a )
    else
      pdf = real ( x2 - 1, kind = 8 ) * a * pdf / real ( x2, kind = 8 )
    end if

    cdf = cdf + pdf

  end do

  return
end
