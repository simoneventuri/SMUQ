subroutine uniform_01_cdf ( x, cdf )

!*****************************************************************************80
!
!! UNIFORM_01_CDF evaluates the Uniform 01 CDF.
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
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    cdf = 0.0D+00
  else if ( 1.0D+00 < x ) then
    cdf = 1.0D+00
  else
    cdf = x
  end if

  return
end
