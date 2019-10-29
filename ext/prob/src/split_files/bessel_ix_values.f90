subroutine bessel_ix_values ( n_data, nu, x, fx )

!*****************************************************************************80
!
!! BESSEL_IX_VALUES returns some values of the Ix Bessel function.
!
!  Discussion:
!
!    This set of data considers the less common case in which the
!    index of the Bessel function In is actually not an integer.
!    We may suggest this case by occasionally replacing the symbol
!    "In" by "Ix".
!
!    The modified Bessel functions In(Z) and Kn(Z) are solutions of
!    the differential equation
!
!      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselI[n,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
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
!    Output, real ( kind = 8 ) NU, the order of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 28

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.3592084175833614D+00,  &
    0.9376748882454876D+00,  &
    2.046236863089055D+00,   &
    3.053093538196718D+00,   &
    4.614822903407601D+00,   &
    26.47754749755907D+00,   &
    2778.784603874571D+00,   &
    4.327974627242893D+07,   &
    0.2935253263474798D+00,  &
    1.099473188633110D+00,   &
    21.18444226479414D+00,   &
    2500.906154942118D+00,   &
    2.866653715931464D+20,   &
    0.05709890920304825D+00, &
    0.3970270801393905D+00,  &
    13.76688213868258D+00,   &
    2028.512757391936D+00,   &
    2.753157630035402D+20,   &
    0.4139416015642352D+00,  &
    1.340196758982897D+00,   &
    22.85715510364670D+00,   &
    2593.006763432002D+00,   &
    2.886630075077766D+20,   &
    0.03590910483251082D+00, &
    0.2931108636266483D+00,  &
    11.99397010023068D+00,   &
    1894.575731562383D+00,   &
    2.716911375760483D+20 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) nu
  real ( kind = 8 ), save, dimension ( n_max ) :: nu_vec = (/ &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    0.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    1.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    2.50D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    1.25D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00, &
    2.75D+00 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     0.2D+00, &
     1.0D+00, &
     2.0D+00, &
     2.5D+00, &
     3.0D+00, &
     5.0D+00, &
    10.0D+00, &
    20.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00, &
     1.0D+00, &
     2.0D+00, &
     5.0D+00, &
    10.0D+00, &
    50.0D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    nu = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    nu = nu_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
