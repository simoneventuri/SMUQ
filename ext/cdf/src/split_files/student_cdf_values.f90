subroutine student_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! STUDENT_CDF_VALUES returns some values of the Student CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2001
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
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 13

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
    1, 2, 3, 4, &
    5, 2, 5, 2, &
    5, 2, 3, 4, &
    5 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.60D+00, 0.60D+00, 0.60D+00, 0.60D+00, &
    0.60D+00, 0.75D+00, 0.75D+00, 0.95D+00, &
    0.95D+00, 0.99D+00, 0.99D+00, 0.99D+00, &
    0.99D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.325D+00, 0.289D+00, 0.277D+00, 0.271D+00, &
    0.267D+00, 0.816D+00, 0.727D+00, 2.920D+00, &
    2.015D+00, 6.965D+00, 4.541D+00, 3.747D+00, &
    3.365D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
