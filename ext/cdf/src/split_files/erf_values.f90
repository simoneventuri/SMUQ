subroutine erf_values ( n_data, x, fx )

!*****************************************************************************80
!
!! ERF_VALUES returns some values of the ERF or "error" function.
!
!  Discussion:
!
!    ERF(X) = ( 2 / sqrt ( PI ) * integral ( 0 <= T <= X ) exp ( - T^2 ) dT
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

  integer ( kind = 4 ), parameter :: n_max = 21

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0000000000D+00, 0.1124629160D+00, 0.2227025892D+00, 0.3286267595D+00, &
    0.4283923550D+00, 0.5204998778D+00, 0.6038560908D+00, 0.6778011938D+00, &
    0.7421009647D+00, 0.7969082124D+00, 0.8427007929D+00, 0.8802050696D+00, &
    0.9103139782D+00, 0.9340079449D+00, 0.9522851198D+00, 0.9661051465D+00, &
    0.9763483833D+00, 0.9837904586D+00, 0.9890905016D+00, 0.9927904292D+00, &
    0.9953222650D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.0D+00, 0.1D+00, 0.2D+00, 0.3D+00, &
    0.4D+00, 0.5D+00, 0.6D+00, 0.7D+00, &
    0.8D+00, 0.9D+00, 1.0D+00, 1.1D+00, &
    1.2D+00, 1.3D+00, 1.4D+00, 1.5D+00, &
    1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, &
    2.0D+00 /)

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
