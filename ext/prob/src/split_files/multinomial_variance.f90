subroutine multinomial_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! MULTINOMIAL_VARIANCE returns the variances of the Multinomial PDF.
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
!    sum ( 1 <= I <= B ) C(I) = 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE(B), VARIANCE(I) is the variance of the
!    total number of events of type I.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  integer ( kind = 4 ) i
  real ( kind = 8 ) variance(b)

  do i = 1, b
    variance(i) = real ( a, kind = 8 ) * c(i) * ( 1.0D+00 - c(i) )
  end do

  return
end
