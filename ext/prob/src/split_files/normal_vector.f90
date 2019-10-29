subroutine normal_vector ( n, mean, dev, seed, x )

!*****************************************************************************80
!
!! NORMAL_VECTOR samples the normal probability distribution.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) has
!    a user-specified mean and standard deviation.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    Before calling this routine, the user may call RANDOM_SEED
!    in order to set the seed of the random number generator.
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values desired.
!
!    Input, real ( kind = 8 ) MEAN, the desired mean value.
!
!    Input, real ( kind = 8 ) DEV, the desired standard deviation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) dev
  real ( kind = 8 ) mean

  call normal_01_vector ( n, seed, x )

  x(1:n) = mean + dev * x(1:n)

  return
end
