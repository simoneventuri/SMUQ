subroutine gamma_inc_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! GAMMA_INC_VALUES returns some values of the incomplete Gamma function.
!
!  Discussion:
!
!    The (normalized) incomplete Gamma function P(A,X) is defined as:
!
!      PN(A,X) = 1/GAMMA(A) * Integral ( 0 <= T <= X ) T**(A-1) * exp(-T) dT.
!
!    With this definition, for all A and X,
!
!      0 <= PN(A,X) <= 1
!
!    and
!
!      PN(A,INFINITY) = 1.0
!
!    Mathematica can compute this value as
!
!      1 - GammaRegularized[A,X]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
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
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.1D+00,  0.1D+00,  0.1D+00,  0.5D+00, &
    0.5D+00,  0.5D+00,  1.0D+00,  1.0D+00, &
    1.0D+00,  1.1D+00,  1.1D+00,  1.1D+00, &
    2.0D+00,  2.0D+00,  2.0D+00,  6.0D+00, &
    6.0D+00, 11.0D+00, 26.0D+00, 41.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.7420263D+00, 0.9119753D+00, 0.9898955D+00, 0.2931279D+00, &
    0.7656418D+00, 0.9921661D+00, 0.0951626D+00, 0.6321206D+00, &
    0.9932621D+00, 0.0757471D+00, 0.6076457D+00, 0.9933425D+00, &
    0.0091054D+00, 0.4130643D+00, 0.9931450D+00, 0.0387318D+00, &
    0.9825937D+00, 0.9404267D+00, 0.4863866D+00, 0.7359709D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    3.1622777D-02, 3.1622777D-01, 1.5811388D+00, 7.0710678D-02, &
    7.0710678D-01, 3.5355339D+00, 0.1000000D+00, 1.0000000D+00, &
    5.0000000D+00, 1.0488088D-01, 1.0488088D+00, 5.2440442D+00, &
    1.4142136D-01, 1.4142136D+00, 7.0710678D+00, 2.4494897D+00, &
    1.2247449D+01, 1.6583124D+01, 2.5495098D+01, 4.4821870D+01 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
