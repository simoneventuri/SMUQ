subroutine binomial_variance ( a, b, variance )

!*****************************************************************************80
!
!! BINOMIAL_VARIANCE returns the variance of the Binomial PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = real ( a, kind = 8 ) * b * ( 1.0D+00 - b )

  return
end
