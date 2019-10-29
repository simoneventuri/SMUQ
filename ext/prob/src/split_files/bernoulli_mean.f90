subroutine bernoulli_mean ( a, mean )

!*****************************************************************************80
!
!! BERNOULLI_MEAN returns the mean of the Bernoulli PDF.
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
!    Input, real ( kind = 8 ) A, the probability of success.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean

  mean = a

  return
end
