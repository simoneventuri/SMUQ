subroutine log_series_sample ( a, seed, x )

!*****************************************************************************80
!
!! LOG_SERIES_SAMPLE samples the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer-Verlag, 1986, page 547.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  integer ( kind = 4 ) x

  u = r8_uniform_01 ( seed )
  v = r8_uniform_01 ( seed )

  x = int ( 1.0D+00 + log ( v ) / ( log ( 1.0D+00 - ( 1.0D+00 - a ) ** u ) ) )

  return
end
