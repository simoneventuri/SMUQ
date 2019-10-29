subroutine bernoulli_variance ( a, variance )

!*****************************************************************************80
!
!! BERNOULLI_VARIANCE returns the variance of the Bernoulli PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) variance

  variance = a * ( 1.0D+00 - a )

  return
end
