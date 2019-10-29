subroutine half_normal_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! HALF_NORMAL_CDF evaluates the Half Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) x

  if ( x <= a ) then
    cdf = 0.0D+00
  else
    call normal_cdf ( x, a, b, cdf2 )
    cdf = 2.0D+00 * cdf2 - 1.0D+00
  end if

  return
end
