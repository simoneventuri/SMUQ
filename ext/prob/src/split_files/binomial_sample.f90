subroutine binomial_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! BINOMIAL_SAMPLE samples the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Algorithm BU,
!    Statistical Computing,
!    Dekker, 1980.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  integer ( kind = 4 ) x

  x = 0

  do i = 1, a

    u = r8_uniform_01 ( seed )

    if ( u <= b ) then
      x = x + 1
    end if

  end do

  return
end
