subroutine poisson_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! POISSON_CDF_VALUES returns some values of the Poisson CDF.
!
!  Discussion:
!
!    CDF(X)(A) is the probability of at most X successes in unit time,
!    given that the expected mean number of successes is A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2001
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
!    30th Edition, CRC Press, 1996, pages 653-658.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, integer X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.02D+00, 0.10D+00, 0.10D+00, 0.50D+00, &
    0.50D+00, 0.50D+00, 1.00D+00, 1.00D+00, &
    1.00D+00, 1.00D+00, 2.00D+00, 2.00D+00, &
    2.00D+00, 2.00D+00, 5.00D+00, 5.00D+00, &
    5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, &
    5.00D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.980D+00, 0.905D+00, 0.995D+00, 0.607D+00, &
    0.910D+00, 0.986D+00, 0.368D+00, 0.736D+00, &
    0.920D+00, 0.981D+00, 0.135D+00, 0.406D+00, &
    0.677D+00, 0.857D+00, 0.007D+00, 0.040D+00, &
    0.125D+00, 0.265D+00, 0.441D+00, 0.616D+00, &
    0.762D+00 /)
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) x
  integer ( kind = 4 ), save, dimension ( n_max ) :: x_vec = (/ &
     0, 0, 1, 0, &
     1, 2, 0, 1, &
     2, 3, 0, 1, &
     2, 3, 0, 1, &
     2, 3, 4, 5, &
     6 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    x = 0
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
