subroutine negative_binomial_cdf_values ( n_data, f, s, p, cdf )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_VALUES returns values of the negative binomial CDF.
!
!  Discussion:
!
!    Assume that a coin has a probability P of coming up heads on
!    any one trial.  Suppose that we plan to flip the coin until we
!    achieve a total of S heads.  If we let F represent the number of
!    tails that occur in this process, then the value of F satisfies
!    a negative binomial PDF:
!
!      PDF(F,S,P) = Choose ( F from F+S-1 ) * P**S * (1-P)**F
!
!    The negative binomial CDF is the probability that there are F or
!    fewer failures upon the attainment of the S-th success.  Thus,
!
!      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    FC Powell,
!    Statistical Tables for Sociology, Biology and Physical Sciences,
!    Cambridge University Press, 1982.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) F, the maximum number of failures.
!
!    Output, integer ( kind = 4 ) S, the number of successes.
!
!    Output, real ( kind = 8 ) P, the probability of a success on one trial.
!
!    Output, real ( kind = 8 ) CDF, the probability of at most F failures
!    before the S-th success.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 27

  real ( kind = 8 ) cdf
  real ( kind = 8 ), save, dimension ( n_max ) :: cdf_vec = (/ &
    0.6367D+00, 0.3633D+00, 0.1445D+00, &
    0.5000D+00, 0.2266D+00, 0.0625D+00, &
    0.3438D+00, 0.1094D+00, 0.0156D+00, &
    0.1792D+00, 0.0410D+00, 0.0041D+00, &
    0.0705D+00, 0.0109D+00, 0.0007D+00, &
    0.9862D+00, 0.9150D+00, 0.7472D+00, &
    0.8499D+00, 0.5497D+00, 0.2662D+00, &
    0.6513D+00, 0.2639D+00, 0.0702D+00, &
    1.0000D+00, 0.0199D+00, 0.0001D+00 /)
  integer ( kind = 4 ) f
  integer ( kind = 4 ), save, dimension ( n_max ) :: f_vec = (/ &
     4,  3,  2, &
     3,  2,  1, &
     2,  1,  0, &
     2,  1,  0, &
     2,  1,  0, &
    11, 10,  9, &
    17, 16, 15, &
     9,  8,  7, &
     2,  1,  0 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) p
  real ( kind = 8 ), save, dimension ( n_max ) :: p_vec = (/ &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 0.50D+00, &
    0.40D+00, 0.40D+00, 0.40D+00, &
    0.30D+00, 0.30D+00, 0.30D+00, &
    0.30D+00, 0.30D+00, 0.30D+00, &
    0.10D+00, 0.10D+00, 0.10D+00, &
    0.10D+00, 0.10D+00, 0.10D+00, &
    0.01D+00, 0.01D+00, 0.01D+00 /)
  integer ( kind = 4 ) s
  integer ( kind = 4 ), save, dimension ( n_max ) :: s_vec = (/ &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    4, 5, 6, &
    1, 2, 3, &
    1, 2, 3, &
    1, 2, 3, &
    0, 1, 2 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    f = 0
    s = 0
    p = 0.0D+00
    cdf = 0.0D+00
  else
    f = f_vec(n_data)
    s = s_vec(n_data)
    p = p_vec(n_data)
    cdf = cdf_vec(n_data)
  end if

  return
end
