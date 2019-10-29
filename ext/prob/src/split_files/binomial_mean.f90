subroutine binomial_mean ( a, b, mean )

!*****************************************************************************80
!
!! BINOMIAL_MEAN returns the mean of the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the expected value of the number of
!    successes in A trials.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean

  mean = real ( a, kind = 8 ) * b

  return
end
