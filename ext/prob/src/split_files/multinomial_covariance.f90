subroutine multinomial_covariance ( a, b, c, covariance )

!*****************************************************************************80
!
!! MULTINOMIAL_COVARIANCE returns the covariances of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 1999
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
!    Output, real ( kind = 8 ) COVARIANCE(B,B), the covariance matrix.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  real ( kind = 8 ) covariance(b,b)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, b
    do j = 1, b

      if ( i == j ) then
        covariance(i,j) = real ( a, kind = 8 ) * c(i) * ( 1.0D+00 - c(i) )
      else
        covariance(i,j) = - real ( a, kind = 8 ) * c(i) * c(j)
      end if

    end do
  end do

  return
end
