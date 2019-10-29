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
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = PoissonDistribution [ a ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2004
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
!    Output, real ( kind = 8 ) A, the parameter of the function.
!
!    Output, integer ( kind = 4 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.02D+00, &
    0.10D+00, &
    0.10D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    1.00D+00, &
    1.00D+00, &
    1.00D+00, &
    1.00D+00, &
    2.00D+00, &
    2.00D+00, &
    2.00D+00, &
    2.00D+00, &
    5.00D+00, &
    5.00D+00, &
    5.00D+00, &
    5.00D+00, &
    5.00D+00, &
    5.00D+00, &
    5.00D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.9801986733067553D+00, &
    0.9048374180359596D+00, &
    0.9953211598395555D+00, &
    0.6065306597126334D+00, &
    0.9097959895689501D+00, &
    0.9856123220330293D+00, &
    0.3678794411714423D+00, &
    0.7357588823428846D+00, &
    0.9196986029286058D+00, &
    0.9810118431238462D+00, &
    0.1353352832366127D+00, &
    0.4060058497098381D+00, &
    0.6766764161830635D+00, &
    0.8571234604985470D+00, &
    0.6737946999085467D-02, &
    0.4042768199451280D-01, &
    0.1246520194830811D+00, &
    0.2650259152973617D+00, &
    0.4404932850652124D+00, &
    0.6159606548330631D+00, &
    0.7621834629729387D+00 /)
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
