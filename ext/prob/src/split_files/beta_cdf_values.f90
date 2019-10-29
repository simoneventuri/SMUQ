subroutine beta_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! BETA_CDF_VALUES returns some values of the Beta CDF.
!
!  Discussion:
!
!    The incomplete Beta function may be written
!
!      BETA_INC(A,B,X) = integral (0 <= t <= X) T^(A-1) * (1-T)^(B-1) dT
!                      / integral (0 <= t <= 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    The incomplete Beta function is also sometimes called the
!    "modified" Beta function, or the "normalized" Beta function
!    or the Beta CDF (cumulative density function).
!
!    In Mathematica, the function can be evaluated by:
!
!      BETA[X,A,B] / BETA[A,B]
!
!    The function can also be evaluated by using the Statistics package:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = BetaDistribution [ a, b ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
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
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968,
!    LC: QA351.P38.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, the parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 45

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     5.5D+00, &
    10.0D+00, &
    10.0D+00, &
    10.0D+00, &
    10.0D+00, &
    20.0D+00, &
    20.0D+00, &
    20.0D+00, &
    20.0D+00, &
    20.0D+00, &
    30.0D+00, &
    30.0D+00, &
    40.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     1.0D+00, &
     2.0D+00, &
     3.0D+00, &
     4.0D+00, &
     5.0D+00, &
     1.30625D+00, &
     1.30625D+00, &
     1.30625D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     1.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     5.0D+00, &
     0.5D+00, &
     5.0D+00, &
     5.0D+00, &
    10.0D+00, &
     5.0D+00, &
    10.0D+00, &
    10.0D+00, &
    20.0D+00, &
    20.0D+00, &
    10.0D+00, &
    10.0D+00, &
    20.0D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     0.5D+00, &
     2.0D+00, &
     3.0D+00, &
     4.0D+00, &
     5.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
     2.0D+00, &
    11.7562D+00, &
    11.7562D+00, &
    11.7562D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.6376856085851985D-01, &
    0.2048327646991335D+00, &
    0.1000000000000000D+01, &
    0.0000000000000000D+00, &
    0.5012562893380045D-02, &
    0.5131670194948620D-01, &
    0.2928932188134525D+00, &
    0.5000000000000000D+00, &
    0.2800000000000000D-01, &
    0.1040000000000000D+00, &
    0.2160000000000000D+00, &
    0.3520000000000000D+00, &
    0.5000000000000000D+00, &
    0.6480000000000000D+00, &
    0.7840000000000000D+00, &
    0.8960000000000000D+00, &
    0.9720000000000000D+00, &
    0.4361908850559777D+00, &
    0.1516409096347099D+00, &
    0.8978271484375000D-01, &
    0.1000000000000000D+01, &
    0.5000000000000000D+00, &
    0.4598773297575791D+00, &
    0.2146816102371739D+00, &
    0.9507364826957875D+00, &
    0.5000000000000000D+00, &
    0.8979413687105918D+00, &
    0.2241297491808366D+00, &
    0.7586405487192086D+00, &
    0.7001783247477069D+00, &
    0.5131670194948620D-01, &
    0.1055728090000841D+00, &
    0.1633399734659245D+00, &
    0.2254033307585166D+00, &
    0.3600000000000000D+00, &
    0.4880000000000000D+00, &
    0.5904000000000000D+00, &
    0.6723200000000000D+00, &
    0.2160000000000000D+00, &
    0.8370000000000000D-01, &
    0.3078000000000000D-01, &
    0.1093500000000000D-01, &
    0.918884684620518D+00, &
    0.21052977489419D+00, &
    0.1824130512500673D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, &
    0.10D+00, &
    1.00D+00, &
    0.00D+00, &
    0.01D+00, &
    0.10D+00, &
    0.50D+00, &
    0.50D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    0.50D+00, &
    0.90D+00, &
    0.50D+00, &
    1.00D+00, &
    0.50D+00, &
    0.80D+00, &
    0.60D+00, &
    0.80D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.70D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.20D+00, &
    0.20D+00, &
    0.20D+00, &
    0.20D+00, &
    0.30D+00, &
    0.30D+00, &
    0.30D+00, &
    0.30D+00, &
    0.225609D+00, &
    0.0335568D+00, &
    0.0295222D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    b = 0.0D+00
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
