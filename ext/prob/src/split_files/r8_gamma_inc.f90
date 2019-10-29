function r8_gamma_inc ( p, x )

!*****************************************************************************80
!
!! R8_GAMMA_INC computes the incomplete Gamma function.
!
!  Discussion:
!
!    GAMMA_INC(P,       0) = 0,
!    GAMMA_INC(P,Infinity) = 1.
!
!    GAMMA_INC(P,X) = Integral ( 0 <= T <= X ) T^(P-1) EXP(-T) DT / GAMMA(P).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
!
!  Author:
!
!    Original FORTRAN77 version by B L Shea.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BL Shea,
!    Chi-squared and Incomplete Gamma Integral,
!    Algorithm AS239,
!    Applied Statistics,
!    Volume 37, Number 3, 1988, pages 466-473.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the exponent parameter.
!    0.0 < P.
!
!    Input, real ( kind = 8 ) X, the integral limit parameter.
!    If X is less than or equal to 0, the value is returned as 0.
!
!    Output, real ( kind = 8 ) R8_GAMMA_INC, the value of the function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: exp_arg_min = -88.0D+00
  real ( kind = 8 ), parameter :: overflow = 1.0D+37
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: plimit = 1000.0D+00
  real ( kind = 8 ) pn1
  real ( kind = 8 ) pn2
  real ( kind = 8 ) pn3
  real ( kind = 8 ) pn4
  real ( kind = 8 ) pn5
  real ( kind = 8 ) pn6
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) rn
  real ( kind = 8 ), parameter :: tol = 1.0D-07
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 1.0D+08

  r8_gamma_inc = 0.0D+00

  if ( p <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_INC - Fatal error!'
    write ( *, '(a)' ) '  Parameter P <= 0.'
    stop 1
  end if

  if ( x <= 0.0D+00 ) then
    r8_gamma_inc = 0.0D+00
    return
  end if
!
!  Use a normal approximation if PLIMIT < P.
!
  if ( plimit < p ) then
    pn1 = 3.0D+00 * sqrt ( p ) * ( ( x / p ) ** ( 1.0D+00 / 3.0D+00 ) &
      + 1.0D+00 / ( 9.0D+00 * p ) - 1.0D+00 )
    call normal_01_cdf ( pn1, cdf )
    r8_gamma_inc = cdf
    return
  end if
!
!  Is X extremely large compared to P?
!
  if ( xbig < x ) then
    r8_gamma_inc = 1.0D+00
    return
  end if
!
!  Use Pearson's series expansion.
!  (P is not large enough to force overflow in the log of Gamma.
!
  if ( x <= 1.0D+00 .or. x < p ) then

    arg = p * log ( x ) - x - lgamma ( p + 1.0D+00 )
    c = 1.0D+00
    r8_gamma_inc = 1.0D+00
    a = p

    do

      a = a + 1.0D+00
      c = c * x / a
      r8_gamma_inc = r8_gamma_inc + c

      if ( c <= tol ) then
        exit
      end if

    end do

    arg = arg + log ( r8_gamma_inc )

    if ( exp_arg_min <= arg ) then
      r8_gamma_inc = exp ( arg )
    else
      r8_gamma_inc = 0.0D+00
    end if

  else
!
!  Use a continued fraction expansion.
!
    arg = p * log ( x ) - x - lgamma ( p )
    a = 1.0D+00 - p
    b = a + x + 1.0D+00
    c = 0.0D+00
    pn1 = 1.0D+00
    pn2 = x
    pn3 = x + 1.0D+00
    pn4 = x * b
    r8_gamma_inc = pn3 / pn4

    do

      a = a + 1.0D+00
      b = b + 2.0D+00
      c = c + 1.0D+00
      pn5 = b * pn3 - a * c * pn1
      pn6 = b * pn4 - a * c * pn2

      if ( 0.0D+00 < abs ( pn6 ) ) then

        rn = pn5 / pn6

        if ( abs ( r8_gamma_inc - rn ) <= min ( tol, tol * rn ) ) then

          arg = arg + log ( r8_gamma_inc )

          if ( exp_arg_min <= arg ) then
            r8_gamma_inc = 1.0D+00 - exp ( arg )
          else
            r8_gamma_inc = 1.0D+00
          end if

          return

        end if

        r8_gamma_inc = rn

      end if

      pn1 = pn3
      pn2 = pn4
      pn3 = pn5
      pn4 = pn6
!
!  Rescale terms in continued fraction if terms are large.
!
      if ( overflow <= abs ( pn5 ) ) then
        pn1 = pn1 / overflow
        pn2 = pn2 / overflow
        pn3 = pn3 / overflow
        pn4 = pn4 / overflow
      end if

    end do

  end if

  return
end
