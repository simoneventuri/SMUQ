subroutine beta_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! BETA_CDF evaluates the Beta CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
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
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    cdf = 0.0D+00
  else if ( x <= 1.0D+00 ) then
    cdf = beta_inc ( a, b, x )
  else
    cdf = 1.0D+00
  end if

  return
end
