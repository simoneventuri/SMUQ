subroutine planck_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! PLANCK_SAMPLE samples the Planck PDF.
!
!  Discussion:
!
!    The Planck sampling seems to be giving incorrect results.
!    I suspect this has to do with a possible problem in the
!    ZIPF_SAMPLE routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer Verlag, 1986, pages 552.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) c2
  real ( kind = 8 ) g
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  integer ( kind = 4 ) z

  a2 = 0.0D+00
  b2 = 1.0D+00
  c2 = b + 1.0D+00

  call gamma_sample ( a2, b2, c2, seed, g )

  call zipf_sample ( c2, seed, z )

  x = g / ( a * real ( z, kind = 8 ) )

  return
end
