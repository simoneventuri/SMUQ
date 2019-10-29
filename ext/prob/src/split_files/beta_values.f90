subroutine beta_values ( n_data, x, y, fxy )

!*****************************************************************************80
!
!! BETA_VALUES returns some values of the Beta function.
!
!  Discussion:
!
!    Beta(X,Y) = ( Gamma(X) * Gamma(Y) ) / Gamma(X+Y)
!
!    Both X and Y must be greater than 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      Beta[X,Y]
!
!  Properties:
!
!    Beta(X,Y) = Beta(Y,X).
!    Beta(X,Y) = integral ( 0 <= T <= 1 ) T^(X-1) (1-T)^(Y-1) dT.
!    Beta(X,Y) = Gamma(X) * Gamma(Y) / Gamma(X+Y)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 2004
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
!    Output, real ( kind = 8 ) X, Y, the arguments of the function.
!
!    Output, real ( kind = 8 ) FXY, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 17

  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
    0.5000000000000000D+01, &
    0.2500000000000000D+01, &
    0.1666666666666667D+01, &
    0.1250000000000000D+01, &
    0.5000000000000000D+01, &
    0.2500000000000000D+01, &
    0.1000000000000000D+01, &
    0.1666666666666667D+00, &
    0.3333333333333333D-01, &
    0.7142857142857143D-02, &
    0.1587301587301587D-02, &
    0.2380952380952381D-01, &
    0.5952380952380952D-02, &
    0.1984126984126984D-02, &
    0.7936507936507937D-03, &
    0.3607503607503608D-03, &
    0.8325008325008325D-04 /)
  real ( kind = 8 ) fxy
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.2D+00, &
    0.4D+00, &
    0.6D+00, &
    0.8D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00, &
    5.0D+00, &
    6.0D+00, &
    6.0D+00, &
    6.0D+00, &
    6.0D+00, &
    6.0D+00, &
    7.0D+00 /)
  real ( kind = 8 ) y
  real ( kind = 8 ), save, dimension ( n_max ) :: y_vec = (/ &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    1.0D+00, &
    0.2D+00, &
    0.4D+00, &
    1.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00, &
    5.0D+00, &
    2.0D+00, &
    3.0D+00, &
    4.0D+00, &
    5.0D+00, &
    6.0D+00, &
    7.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    y = 0.0D+00
    fxy = 0.0D+00
  else
    x = x_vec(n_data)
    y = y_vec(n_data)
    fxy = b_vec(n_data)
  end if

  return
end
