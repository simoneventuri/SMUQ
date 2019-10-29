subroutine gumbel_cdf ( x, cdf )

!*****************************************************************************80
!
!! GUMBEL_CDF evaluates the Gumbel CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
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

  cdf = exp ( - exp ( - x ) )

  return
end
