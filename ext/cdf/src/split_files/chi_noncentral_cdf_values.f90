subroutine chi_noncentral_cdf_values ( n_data, x, lambda, df, cdf )

!*****************************************************************************80
!
!! CHI_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
!
!  Discussion:
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
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
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, integer ( kind = 4 ) DF, the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the noncentral chi CDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 27

  real ( kind = 8 ) cdf
  real, save, dimension ( n_max ) :: cdf_vec = (/ &
    0.839944D+00, 0.695906D+00, 0.535088D+00, &
    0.764784D+00, 0.620644D+00, 0.469167D+00, &
    0.307088D+00, 0.220382D+00, 0.150025D+00, &
    0.307116D-02, 0.176398D-02, 0.981679D-03, &
    0.165175D-01, 0.202342D-03, 0.498448D-06, &
    0.151325D-01, 0.209041D-02, 0.246502D-03, &
    0.263684D-01, 0.185798D-01, 0.130574D-01, &
    0.583804D-01, 0.424978D-01, 0.308214D-01, &
    0.105788D+00, 0.794084D-01, 0.593201D-01 /)
  integer ( kind = 4 ) df
  integer ( kind = 4 ), save, dimension ( n_max ) :: df_vec = (/ &
      1,   2,   3, &
      1,   2,   3, &
      1,   2,   3, &
      1,   2,   3, &
     60,  80, 100, &
      1,   2,   3, &
     10,  10,  10, &
     10,  10,  10, &
     10,  10,  10 /)
  real ( kind = 8 ) lambda
  real, save, dimension ( n_max ) :: lambda_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00, &
     1.0D+00,  1.0D+00,  1.0D+00, &
     5.0D+00,  5.0D+00,  5.0D+00, &
    20.0D+00, 20.0D+00, 20.0D+00, &
    30.0D+00, 30.0D+00, 30.0D+00, &
     5.0D+00,  5.0D+00,  5.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00, &
     2.0D+00,  3.0D+00,  4.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real, save, dimension ( n_max ) :: x_vec = (/ &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
     3.000D+00,  3.000D+00,  3.000D+00, &
    60.000D+00, 60.000D+00, 60.000D+00, &
     0.050D+00,  0.050D+00,  0.050D+00, &
     4.000D+00,  4.000D+00,  4.000D+00, &
     5.000D+00,  5.000D+00,  5.000D+00, &
     6.000D+00,  6.000D+00,  6.000D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    lambda = 0.0D+00
    df = 0
    cdf = 0.0D+00
  else
    x = x_vec(n_data)
    lambda = lambda_vec(n_data)
    df = df_vec(n_data)
    cdf = cdf_vec(n_data)
  end if

  return
end
