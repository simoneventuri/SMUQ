subroutine chi_square_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = ChiSquareDistribution [ df ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2004
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
!    Output, integer ( kind = 4 ) A, the parameter of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     1,  2,  1,  2, &
     1,  2,  3,  4, &
     1,  2,  3,  4, &
     5,  3,  3,  3, &
     3,  3, 10, 10, &
    10 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.7965567455405796D-01, &
    0.4987520807317687D-02, &
    0.1124629160182849D+00, &
    0.9950166250831946D-02, &
    0.4729107431344619D+00, &
    0.1812692469220181D+00, &
    0.5975750516063926D-01, &
    0.1752309630642177D-01, &
    0.6826894921370859D+00, &
    0.3934693402873666D+00, &
    0.1987480430987992D+00, &
    0.9020401043104986D-01, &
    0.3743422675270363D-01, &
    0.4275932955291202D+00, &
    0.6083748237289110D+00, &
    0.7385358700508894D+00, &
    0.8282028557032669D+00, &
    0.8883897749052874D+00, &
    0.1721156299558408D-03, &
    0.3659846827343712D-02, &
    0.1857593622214067D-01 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, &
    0.01D+00, &
    0.02D+00, &
    0.02D+00, &
    0.40D+00, &
    0.40D+00, &
    0.40D+00, &
    0.40D+00, &
    1.00D+00, &
    1.00D+00, &
    1.00D+00, &
    1.00D+00, &
    1.00D+00, &
    2.00D+00, &
    3.00D+00, &
    4.00D+00, &
    5.00D+00, &
    6.00D+00, &
    1.00D+00, &
    2.00D+00, &
    3.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
