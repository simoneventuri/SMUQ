subroutine rayleigh_cdf_values ( n_data, sigma, x, fx )

!*****************************************************************************80
!
!! RAYLEIGH_CDF_VALUES returns some values of the Rayleigh CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = RayleighDistribution [ sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
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
!    Output, real ( kind = 8 ) SIGMA, the shape parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 9

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.8646647167633873D+00, &
    0.9996645373720975D+00, &
    0.9999999847700203D+00, &
    0.999999999999987D+00, &
    0.8646647167633873D+00, &
    0.3934693402873666D+00, &
    0.1992625970831920D+00, &
    0.1175030974154046D+00, &
    0.7688365361336422D-01 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) sigma
  real ( kind = 8 ), save, dimension ( n_max ) :: sigma_vec = (/ &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.4000000000000000D+01, &
    0.5000000000000000D+01 /)
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
    0.2000000000000000D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    sigma = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    sigma = sigma_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
