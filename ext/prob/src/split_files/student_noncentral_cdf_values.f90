subroutine student_noncentral_cdf_values ( n_data, df, lambda, x, fx )

!*****************************************************************************80
!
!! STUDENT_NONCENTRAL_CDF_VALUES returns values of the noncentral Student CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NoncentralStudentTDistribution [ df, lambda ]
!      CDF [ dist, x ]
!
!    Mathematica seems to have some difficulty computing this function
!    to the desired number of digits.
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
!    Output, integer ( kind = 4 ) DF, real ( kind = 8 ) LAMBDA, the parameters
!    of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 30

  integer ( kind = 4 ) df
  integer ( kind = 4 ), save, dimension ( n_max ) :: df_vec = (/ &
     1,  2,  3, &
     1,  2,  3, &
     1,  2,  3, &
     1,  2,  3, &
     1,  2,  3, &
    15, 20, 25, &
     1,  2,  3, &
    10, 10, 10, &
    10, 10, 10, &
    10, 10, 10 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.8975836176504333D+00, &
    0.9522670169D+00, &
    0.9711655571887813D+00, &
    0.8231218864D+00, &
    0.9049021510D+00, &
    0.9363471834D+00, &
    0.7301025986D+00, &
    0.8335594263D+00, &
    0.8774010255D+00, &
    0.5248571617D+00, &
    0.6293856597D+00, &
    0.6800271741D+00, &
    0.20590131975D+00, &
    0.2112148916D+00, &
    0.2074730718D+00, &
    0.9981130072D+00, &
    0.9994873850D+00, &
    0.9998391562D+00, &
    0.168610566972D+00, &
    0.16967950985D+00, &
    0.1701041003D+00, &
    0.9247683363D+00, &
    0.7483139269D+00, &
    0.4659802096D+00, &
    0.9761872541D+00, &
    0.8979689357D+00, &
    0.7181904627D+00, &
    0.9923658945D+00, &
    0.9610341649D+00, &
    0.8688007350D+00 /)
  real ( kind = 8 ) lambda
  real ( kind = 8 ), save, dimension ( n_max ) :: lambda_vec = (/ &
    0.0D+00, &
    0.0D+00, &
    0.0D+00, &
    0.5D+00, &
    0.5D+00, &
    0.5D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    2.0D+00, &
    2.0D+00, &
    2.0D+00, &
    4.0D+00, &
    4.0D+00, &
    4.0D+00, &
    7.0D+00, &
    7.0D+00, &
    7.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
     3.00D+00, &
    15.00D+00, &
    15.00D+00, &
    15.00D+00, &
     0.05D+00, &
     0.05D+00, &
     0.05D+00, &
     4.00D+00, &
     4.00D+00, &
     4.00D+00, &
     5.00D+00, &
     5.00D+00, &
     5.00D+00, &
     6.00D+00, &
     6.00D+00, &
     6.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    df = 0
    lambda = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    df = df_vec(n_data)
    lambda = lambda_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
