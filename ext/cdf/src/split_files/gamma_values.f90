subroutine gamma_values ( n_data, x, fx )

!*****************************************************************************80
!
!! GAMMA_VALUES returns some values of the Gamma function.
!
!  Definition:
!
!    Gamma(Z) = Integral ( 0 <= T < Infinity) T**(Z-1) exp(-T) dT
!
!  Recursion:
!
!    Gamma(X+1) = X * Gamma(X)
!
!  Restrictions:
!
!    0 < X ( a software restriction).
!
!  Special values:
!
!    GAMMA(0.5) = sqrt(PI)
!
!    For N a positive integer, GAMMA(N+1) = N!, the standard factorial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2001
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
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 18

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    4.590845D+00,     2.218160D+00,     1.489192D+00,     1.164230D+00, &
    1.0000000000D+00, 0.9513507699D+00, 0.9181687424D+00, 0.8974706963D+00, &
    0.8872638175D+00, 0.8862269255D+00, 0.8935153493D+00, 0.9086387329D+00, &
    0.9313837710D+00, 0.9617658319D+00, 1.0000000000D+00, 3.6288000D+05, &
    1.2164510D+17,    8.8417620D+30 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.2D+00,  0.4D+00,  0.6D+00,  0.8D+00, &
    1.0D+00,  1.1D+00,  1.2D+00,  1.3D+00, &
    1.4D+00,  1.5D+00,  1.6D+00,  1.7D+00, &
    1.8D+00,  1.9D+00,  2.0D+00, 10.0D+00, &
   20.0D+00, 30.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
