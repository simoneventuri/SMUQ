subroutine frechet_cdf ( x, alpha, cdf )

!*****************************************************************************80
!
!! FRECHET_CDF evaluates the Frechet CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_CDF - Fatal error!'
    write ( *, '(a)' ) '  ALPHA <= 0.0.'
    stop 1
  end if

  if ( x <= 0.0D+00 ) then
    cdf = 0.0D+00
  else
    cdf = exp ( - 1.0D+00 / x ** alpha )
  end if

  return
end
