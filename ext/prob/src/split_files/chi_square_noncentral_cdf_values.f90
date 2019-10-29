subroutine chi_square_noncentral_cdf_values ( n_data, df, lambda, x, cdf )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NoncentralChiSquareDistribution [ df, lambda ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2004
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
!    Output, integer ( kind = 4 ) DF, the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) CDF, the noncentral chi CDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 28

  real ( kind = 8 ) cdf
  real ( kind = 8 ), save, dimension ( n_max ) :: cdf_vec = (/ &
    0.8399444269398261D+00, &
    0.6959060300435139D+00, &
    0.5350879697078847D+00, &
    0.7647841496310313D+00, &
    0.6206436532195436D+00, &
    0.4691667375373180D+00, &
    0.3070884345937569D+00, &
    0.2203818092990903D+00, &
    0.1500251895581519D+00, &
    0.3071163194335791D-02, &
    0.1763982670131894D-02, &
    0.9816792594625022D-03, &
    0.1651753140866208D-01, &
    0.2023419573950451D-03, &
    0.4984476352854074D-06, &
    0.1513252400654827D-01, &
    0.2090414910614367D-02, &
    0.2465021206048452D-03, &
    0.2636835050342939D-01, &
    0.1857983220079215D-01, &
    0.1305736595486640D-01, &
    0.5838039534819351D-01, &
    0.4249784402463712D-01, &
    0.3082137716021596D-01, &
    0.1057878223400849D+00, &
    0.7940842984598509D-01, &
    0.5932010895599639D-01, &
    0.2110395656918684D+00 /)
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
     10,  10,  10, &
      8 /)
  real ( kind = 8 ) lambda
  real ( kind = 8 ), save, dimension ( n_max ) :: lambda_vec = (/ &
      0.5D+00, &
      0.5D+00, &
      0.5D+00, &
      1.0D+00, &
      1.0D+00, &
      1.0D+00, &
      5.0D+00, &
      5.0D+00, &
      5.0D+00, &
     20.0D+00, &
     20.0D+00, &
     20.0D+00, &
     30.0D+00, &
     30.0D+00, &
     30.0D+00, &
      5.0D+00, &
      5.0D+00, &
      5.0D+00, &
      2.0D+00, &
      3.0D+00, &
      4.0D+00, &
      2.0D+00, &
      3.0D+00, &
      4.0D+00, &
      2.0D+00, &
      3.0D+00, &
      4.0D+00, &
      0.5D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
     3.000D+00, &
    60.000D+00, &
    60.000D+00, &
    60.000D+00, &
     0.050D+00, &
     0.050D+00, &
     0.050D+00, &
     4.000D+00, &
     4.000D+00, &
     4.000D+00, &
     5.000D+00, &
     5.000D+00, &
     5.000D+00, &
     6.000D+00, &
     6.000D+00, &
     6.000D+00, &
     5.000D+00 /)

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
