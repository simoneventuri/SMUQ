subroutine multinomial_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! MULTINOMIAL_MEAN returns the means of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    SUM ( 1 <= I <= B) C(I) = 1.0.
!
!    Output, real ( kind = 8 ) MEAN(B), MEAN(I) is the expected value of the
!    number of outcome I in N trials.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  real ( kind = 8 ) mean(b)

  mean(1:b) = real ( a, kind = 8 ) * c(1:b)

  return
end
