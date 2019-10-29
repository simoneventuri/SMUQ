subroutine beta_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! BETA_SAMPLE samples the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Algorithm BN,
!    Statistical Computing,
!    Dekker, 1980.
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
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) mu
  integer ( kind = 4 ) seed
  real ( kind = 8 ) stdev
  real ( kind = 8 ) test
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  mu = ( a - 1.0D+00 ) / ( a + b - 2.0D+00 )
  stdev = 0.5D+00 / sqrt ( a + b - 2.0D+00 )

  do

    call normal_01_sample ( seed, y )

    x = mu + stdev * y

    if ( x < 0.0D+00 .or. 1.0D+00 < x ) then
      cycle
    end if

    u = r8_uniform_01 ( seed )

    test =     ( a - 1.0D+00 )     * log (         x   / ( a - 1.0D+00 ) ) &
             + ( b - 1.0D+00 )     * log ( ( 1.0D+00 - x ) / ( b - 1.0D+00 ) ) &
             + ( a + b - 2.0D+00 ) * log ( a + b - 2.0D+00 ) + 0.5D+00 * y * y

    if ( log ( u ) <= test ) then
      exit
    end if

  end do

  return
end
