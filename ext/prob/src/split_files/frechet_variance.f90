subroutine frechet_variance ( alpha, variance )

!*****************************************************************************80
!
!! FRECHET_VARIANCE returns the variance of the Frechet PDF.
!
!  Discussion:
!
!    The PDF does not have a variance unless 2 < ALPHA.
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
!    It is required that 2.0 < ALPHA.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  if ( alpha <= 2.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  Variance does not exist if ALPHA <= 2.'
    stop 1
  end if

  mean = gamma ( ( alpha - 1.0D+00 ) / alpha )

  variance = gamma ( ( alpha - 2.0D+00 ) / alpha ) - mean * mean

  return
end
