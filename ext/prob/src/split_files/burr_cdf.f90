subroutine burr_cdf ( x, a, b, c, d, cdf )

!*****************************************************************************80
!
!! BURR_CDF evaluates the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) d
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= a ) then

    cdf = 0.0D+00

  else

    y = ( x - a ) / b

    cdf = 1.0D+00 - 1.0D+00 / ( 1.0D+00 + y ** c  ) ** d

  end if

  return
end
