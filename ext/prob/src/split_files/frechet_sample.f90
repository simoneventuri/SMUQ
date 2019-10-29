subroutine frechet_sample ( alpha, seed, x )

!*****************************************************************************80
!
!! FRECHET_SAMPLE samples the Frechet PDF.
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
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_SAMPLE - Fatal error!'
    write ( *, '(a)' ) '  ALPHA <= 0.0.'
    stop 1
  end if

  cdf = r8_uniform_01 ( seed )

  call frechet_cdf_inv ( cdf, alpha, x )

  return
end
