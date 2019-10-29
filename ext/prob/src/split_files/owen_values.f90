subroutine owen_values ( n_data, h, a, t )

!*****************************************************************************80
!
!! OWEN_VALUES returns some values of Owen's T function.
!
!  Discussion:
!
!    Owen's T function is useful for computation of the bivariate normal
!    distribution and the distribution of a skewed normal distribution.
!
!    Although it was originally formulated in terms of the bivariate
!    normal function, the function can be defined more directly as
!
!      T(H,A) = 1 / ( 2 * pi ) *
!        Integral ( 0 <= X <= A ) e^(-H^2*(1+X^2)/2) / (1+X^2) dX
!
!    In Mathematica, the function can be evaluated by:
!
!      fx = 1/(2*Pi) * Integrate [ E^(-h^2*(1+x^2)/2)/(1+x^2), {x,0,a} ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
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
!    Output, real ( kind = 8 ) H, a parameter.
!
!    Output, real ( kind = 8 ) A, the upper limit of the integral.
!
!    Output, real ( kind = 8 ) T, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 22

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.5000000000000000D+00, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.3000000000000000D+01, &
    0.1000000000000000D+02, &
    0.1000000000000000D+03 /)
  real ( kind = 8 ) h
  real ( kind = 8 ), save, dimension ( n_max ) :: h_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.5000000000000000D+00, &
    0.2500000000000000D+00, &
    0.2500000000000000D+00, &
    0.2500000000000000D+00, &
    0.2500000000000000D+00, &
    0.1250000000000000D+00, &
    0.1250000000000000D+00, &
    0.1250000000000000D+00, &
    0.1250000000000000D+00, &
    0.7812500000000000D-02, &
    0.7812500000000000D-02, &
    0.7812500000000000D-02, &
    0.7812500000000000D-02, &
    0.7812500000000000D-02, &
    0.7812500000000000D-02 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) t
  real ( kind = 8 ), save, dimension ( n_max ) :: t_vec = (/ &
    0.4306469112078537D-01, &
    0.6674188216570097D-01, &
    0.7846818699308410D-01, &
    0.7929950474887259D-01, &
    0.6448860284750376D-01, &
    0.1066710629614485D+00, &
    0.1415806036539784D+00, &
    0.1510840430760184D+00, &
    0.7134663382271778D-01, &
    0.1201285306350883D+00, &
    0.1666128410939293D+00, &
    0.1847501847929859D+00, &
    0.7317273327500385D-01, &
    0.1237630544953746D+00, &
    0.1737438887583106D+00, &
    0.1951190307092811D+00, &
    0.7378938035365546D-01, &
    0.1249951430754052D+00, &
    0.1761984774738108D+00, &
    0.1987772386442824D+00, &
    0.2340886964802671D+00, &
    0.2479460829231492D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    h = 0.0D+00
    a = 0.0D+00
    t = 0.0D+00
  else
    h = h_vec(n_data)
    a = a_vec(n_data)
    t = t_vec(n_data)
  end if

  return
end
