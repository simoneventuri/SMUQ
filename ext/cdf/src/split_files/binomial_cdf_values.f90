subroutine binomial_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! BINOMIAL_CDF_VALUES returns some values of the binomial CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 651-652.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) B, integer X, the
!    arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 17

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     2,  2,  2,  2, &
     2,  4,  4,  4, &
     4, 10, 10, 10, &
    10, 10, 10, 10, &
    10 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
    0.05D+00, 0.05D+00, 0.05D+00, 0.50D+00, &
    0.50D+00, 0.25D+00, 0.25D+00, 0.25D+00, &
    0.25D+00, 0.05D+00, 0.10D+00, 0.15D+00, &
    0.20D+00, 0.25D+00, 0.30D+00, 0.40D+00, &
    0.50D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.9025D+00, 0.9975D+00, 1.0000D+00, 0.2500D+00, &
    0.7500D+00, 0.3164D+00, 0.7383D+00, 0.9492D+00, &
    0.9961D+00, 0.9999D+00, 0.9984D+00, 0.9901D+00, &
    0.9672D+00, 0.9219D+00, 0.8497D+00, 0.6331D+00, &
    0.3770D+00 /)
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) x
  integer ( kind = 4 ), save, dimension ( n_max ) :: x_vec = (/ &
     0, 1, 2, 0, &
     1, 0, 1, 2, &
     3, 4, 4, 4, &
     4, 4, 4, 4, &
     4 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0.0D+00
    x = 0
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
