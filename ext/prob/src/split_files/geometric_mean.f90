subroutine geometric_mean ( a, mean )

!*****************************************************************************80
!
!! GEOMETRIC_MEAN returns the mean of the Geometric PDF.
!
!  Discussion:
!
!    MEAN is the expected value of the number of trials required
!    to obtain a single success.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean

  mean = 1.0D+00 / a

  return
end
