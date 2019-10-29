subroutine normal_cdf_values ( n_data, mu, sigma, x, fx )

!*****************************************************************************80
!
!! NORMAL_CDF_VALUES returns some values of the Normal CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the variance of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 12

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.5000000000000000D+00, &
    0.9772498680518208D+00, &
    0.9999683287581669D+00, &
    0.9999999990134124D+00, &
    0.6914624612740131D+00, &
    0.6305586598182364D+00, &
    0.5987063256829237D+00, &
    0.5792597094391030D+00, &
    0.6914624612740131D+00, &
    0.5000000000000000D+00, &
    0.3085375387259869D+00, &
    0.1586552539314571D+00 /)
  real ( kind = 8 ) mu
  real ( kind = 8 ), save, dimension ( n_max ) :: mu_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01, &
    0.3000000000000000D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    mu = 0.0D+00
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    mu = mu_vec(n_data)
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
