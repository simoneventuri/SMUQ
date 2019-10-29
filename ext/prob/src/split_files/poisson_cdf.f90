subroutine poisson_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! POISSON_CDF evaluates the Poisson CDF.
!
!  Discussion:
!
!    CDF(X,A) is the probability that the number of events observed
!    in a unit time period will be no greater than X, given that the
!    expected number of events in a unit time period is A.
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
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!    0 <= X.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) last
  real ( kind = 8 ) new
  real ( kind = 8 ) sum2
  integer ( kind = 4 ) x

  if ( x < 0 ) then

    cdf = 0.0D+00

  else

    new = exp ( - a )
    sum2 = new

    do i = 1, x
      last = new
      new = last * a / real ( i, kind = 8 )
      sum2 = sum2 + new
    end do

    cdf = sum2

  end if

  return
end
