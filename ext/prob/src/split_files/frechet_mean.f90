subroutine frechet_mean ( alpha, mean )

!*****************************************************************************80
!
!! FRECHET_MEAN returns the mean of the Frechet PDF.
!
!  Discussion:
!
!    The distribution does not have a mean value unless 1 < ALPHA.
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
!    It is required that 1.0 < ALPHA.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) mean

  if ( alpha <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_MEAN - Fatal error!'
    write ( *, '(a)' ) '  Mean does not exist if ALPHA <= 1.'
    stop 1
  end if

  mean = gamma ( ( alpha - 1.0D+00 ) / alpha )

  return
end
