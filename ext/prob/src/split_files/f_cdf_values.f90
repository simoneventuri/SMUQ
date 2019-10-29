subroutine f_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! F_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = FRatioDistribution [ dfn, dfd ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
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
!    Output, integer ( kind = 4 ) A, integer B, the parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
    1, &
    1, &
    5, &
    1, &
    2, &
    4, &
    1, &
    6, &
    8, &
    1, &
    3, &
    6, &
    1, &
    1, &
    1, &
    1, &
    2, &
    3, &
    4, &
    5 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), save, dimension ( n_max ) :: b_vec = (/ &
     1, &
     5, &
     1, &
     5, &
    10, &
    20, &
     5, &
     6, &
    16, &
     5, &
    10, &
    12, &
     5, &
     5, &
     5, &
     5, &
     5, &
     5, &
     5, &
     5 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.5000000000000000D+00, &
    0.4999714850534485D+00, &
    0.4996034370170990D+00, &
    0.7496993658293228D+00, &
    0.7504656462757382D+00, &
    0.7514156325324275D+00, &
    0.8999867031372156D+00, &
    0.8997127554259699D+00, &
    0.9002845660853669D+00, &
    0.9500248817817622D+00, &
    0.9500574946122442D+00, &
    0.9501926400000000D+00, &
    0.9750133887312993D+00, &
    0.9900022327445249D+00, &
    0.9949977837872073D+00, &
    0.9989999621122122D+00, &
    0.5687988496283079D+00, &
    0.5351452100063650D+00, &
    0.5143428032407864D+00, &
    0.5000000000000000D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     1.00D+00, &
     0.528D+00, &
     1.89D+00, &
     1.69D+00, &
     1.60D+00, &
     1.47D+00, &
     4.06D+00, &
     3.05D+00, &
     2.09D+00, &
     6.61D+00, &
     3.71D+00, &
     3.00D+00, &
    10.01D+00, &
    16.26D+00, &
    22.78D+00, &
    47.18D+00, &
     1.00D+00, &
     1.00D+00, &
     1.00D+00, &
     1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
