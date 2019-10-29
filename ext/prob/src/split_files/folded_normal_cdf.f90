subroutine folded_normal_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! FOLDED_NORMAL_CDF evaluates the Folded Normal CDF.
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
!    0.0 <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  if ( x < 0.0D+00 ) then
    cdf = 0.0D+00
  else
    x1 = ( x - a ) / b
    call normal_01_cdf ( x1, cdf1 )
    x2 = ( - x - a ) / b
    call normal_01_cdf ( x2, cdf2 )
    cdf = cdf1 - cdf2
  end if

  return
end
