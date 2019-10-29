subroutine bessel_i1_values ( n_data, x, fx )

!*****************************************************************************80
!
!! BESSEL_I1_VALUES returns some values of the I1 Bessel function.
!
!  Discussion:
!
!    The modified Bessel functions In(Z) and Kn(Z) are solutions of
!    the differential equation
!
!      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselI[1,x]
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

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0000000000000000D+00, &
    0.1005008340281251D+00, &
    0.2040267557335706D+00, &
    0.3137040256049221D+00, &
    0.4328648026206398D+00, &
    0.5651591039924850D+00, &
    0.7146779415526431D+00, &
    0.8860919814143274D+00, &
    0.1084810635129880D+01, &
    0.1317167230391899D+01, &
    0.1590636854637329D+01, &
    0.2516716245288698D+01, &
    0.3953370217402609D+01, &
    0.6205834922258365D+01, &
    0.9759465153704450D+01, &
    0.1538922275373592D+02, &
    0.2433564214245053D+02, &
    0.6134193677764024D+02, &
    0.3998731367825601D+03, &
    0.2670988303701255D+04 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.00D+00, &
    0.20D+00, &
    0.40D+00, &
    0.60D+00, &
    0.80D+00, &
    0.10D+01, &
    0.12D+01, &
    0.14D+01, &
    0.16D+01, &
    0.18D+01, &
    0.20D+01, &
    0.25D+01, &
    0.30D+01, &
    0.35D+01, &
    0.40D+01, &
    0.45D+01, &
    0.50D+01, &
    0.60D+01, &
    0.80D+01, &
    0.10D+02 /)

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
