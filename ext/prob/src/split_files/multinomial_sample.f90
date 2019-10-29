subroutine multinomial_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! MULTINOMIAL_SAMPLE samples the Multinomial PDF.
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
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer-Verlag, New York, 1986, page 559.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!    0 <= A.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on
!    one trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    sum ( 1 <= I <= B) C(I) = 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X(B); X(I) is the number of
!    occurrences of event I during the N trials.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  integer ( kind = 4 ) ifactor
  integer ( kind = 4 ) ntot
  real ( kind = 8 ) prob
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sum2
  integer ( kind = 4 ) x(b)

  ntot = a

  sum2 = 1.0D+00

  x(1:b) = 0

  do ifactor = 1, b - 1

    prob = c(ifactor) / sum2
!
!  Generate a binomial random deviate for NTOT trials with
!  single trial success probability PROB.
!
    call binomial_sample ( ntot, prob, seed, x(ifactor) )

    ntot = ntot - x(ifactor)
    if ( ntot <= 0 ) then
      return
    end if

    sum2 = sum2 - c(ifactor)

  end do
!
!  The last factor gets what's left.
!
  x(b) = ntot

  return
end
