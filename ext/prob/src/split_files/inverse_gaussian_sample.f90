subroutine inverse_gaussian_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_SAMPLE samples the Inverse Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) phi
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  phi = b / a
  call normal_01_sample ( seed, z )
  y = z * z

  t = 1.0D+00 + 0.5D+00 * ( y - sqrt ( 4.0D+00 * phi * y + y * y ) ) / phi
  u = r8_uniform_01 ( seed )

  if ( u * ( 1.0D+00 + t ) <= 1.0D+00 ) then
    x = a * t
  else
    x = a / t
  end if

  return
end
