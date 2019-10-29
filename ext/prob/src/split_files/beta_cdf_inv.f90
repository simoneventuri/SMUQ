subroutine beta_cdf_inv ( cdf, p, q, x )

!*****************************************************************************80
!
!! BETA_CDF_INV computes the inverse of the incomplete Beta function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by GW Cran, KJ Martin, GE Thomas.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    GW Cran, KJ Martin, GE Thomas,
!    Remark AS R19 and Algorithm AS 109:
!    A Remark on Algorithms AS 63: The Incomplete Beta Integral
!    and AS 64: Inverse of the Incomplete Beta Integeral,
!    Applied Statistics,
!    Volume 26, Number 1, 1977, pages 111-114.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the Beta CDF.
!    0 <= CDF <= 1.
!
!    Input, real ( kind = 8 ) P, Q, the parameters of the incomplete
!    Beta function.
!
!    Output, real ( kind = 8 ) X, the argument of the incomplete
!    Beta function which produces the value CDF.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) SAE, the most negative decimal exponent
!    which does not cause an underflow.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) acu
  real ( kind = 8 ) adj
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) cdf
  real ( kind = 8 ) fpu
  real ( kind = 8 ) g
  real ( kind = 8 ) h
  integer ( kind = 4 ) iex
  logical indx
  real ( kind = 8 ) p
  real ( kind = 8 ) pp
  real ( kind = 8 ) prev
  real ( kind = 8 ) q
  real ( kind = 8 ) qq
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ), parameter :: sae = -37.0D+00
  real ( kind = 8 ) sq
  real ( kind = 8 ) t
  real ( kind = 8 ) tx
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) xin
  real ( kind = 8 ) y
  real ( kind = 8 ) yprev

  fpu = 10.0D+00 ** sae
  beta_log = lgamma ( p ) + lgamma ( q ) - lgamma ( p + q )
!
!  Test for admissibility of parameters.
!
  if ( p <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  P <= 0.0'
    return
  end if

  if ( q <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  Q <= 0.0'
    return
  end if

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0.0 or 1.0 < CDF.'
    return
  end if
!
!  Return immediately if the answer is easy to determine.
!
  if ( cdf == 0.0D+00  ) then
    x = 0.0D+00
    return
  else if ( cdf == 1.0D+00 ) then
    x = 1.0D+00
    return
  end if
!
!  Change tail if necessary.
!
  if ( 0.5D+00 < cdf ) then
    a = 1.0D+00 - cdf
    pp = q
    qq = p
    indx = .true.
  else
    a = cdf
    pp = p
    qq = q
    indx = .false.
  end if
!
!  Calculate the initial approximation.
!
  r = sqrt ( - log ( a * a ) )

  y = r - ( 2.30753D+00 + 0.27061D+00 * r ) &
    / ( 1.0D+00 + ( 0.99229D+00 + 0.04481D+00 * r ) * r )

  if ( 1.0D+00 < pp .and. 1.0D+00 < qq ) then

    r = ( y * y - 3.0D+00 ) / 6.0D+00
    s = 1.0D+00 / ( pp + pp - 1.0D+00 )
    t = 1.0D+00 / ( qq + qq - 1.0D+00 )
    h = 2.0D+00 / ( s + t )
    w = y * sqrt ( h + r ) / h - ( t - s ) &
    * ( r + 5.0D+00 / 6.0D+00 - 2.0D+00 / ( 3.0D+00 * h ) )
    x = pp / ( pp + qq * exp ( w + w ) )

  else

    r = qq + qq
    t = 1.0D+00 / ( 9.0D+00 * qq )
    t = r * ( 1.0D+00 - t + y * sqrt ( t ) ) ** 3

    if ( t <= 0.0D+00 ) then
      x = 1.0D+00 - exp ( ( log ( ( 1.0D+00 - a ) * qq ) &
        + beta_log ) / qq )
    else

      t = ( 4.0D+00 * pp + r - 2.0D+00 ) / t

      if ( t <= 1.0D+00 ) then
        x = exp ( ( log ( a * pp ) + beta_log ) / pp )
      else
        x = 1.0D+00 - 2.0D+00 / ( t + 1.0D+00 )
      end if

    end if

  end if
!
!  Solve for X by a modified Newton-Raphson method.
!
  r = 1.0D+00 - pp
  t = 1.0D+00 - qq
  yprev = 0.0D+00
  sq = 1.0D+00
  prev = 1.0D+00

  if ( x < 0.0001D+00 ) then
    x = 0.0001D+00
  end if

  if ( 0.9999D+00 < x ) then
    x = 0.9999D+00
  end if

  iex = max ( - 5.0D+00 / pp ** 2 - 1.0D+00 / a ** 0.2D+00 - 13.0D+00, sae )

  acu = 10.0D+00 ** iex

  do

    y = beta_inc ( pp, qq, x )

    xin = x
    y = ( y - a ) * exp ( beta_log + r * log ( xin ) &
      + t * log ( 1.0D+00 - xin ) )

    if ( y * yprev <= 0.0D+00 ) then
      prev = max ( sq, fpu )
    end if

    g = 1.0D+00

    do

      do

        adj = g * y
        sq = adj * adj

        if ( sq < prev ) then

          tx = x - adj

          if ( 0.0D+00 <= tx .and. tx <= 1.0D+00 ) then
            exit
          end if

        end if

        g = g / 3.0D+00

      end do

      if ( prev <= acu ) then
        if ( indx ) then
          x = 1.0D+00 - x
        end if
        return
      end if

      if ( y * y <= acu ) then
        if ( indx ) then
          x = 1.0D+00 - x
        end if
        return
      end if

      if ( tx /= 0.0D+00 .and. tx /= 1.0D+00 ) then
        exit
      end if

      g = g / 3.0D+00

    end do

    if ( tx == x ) then
      exit
    end if

    x = tx
    yprev = y

  end do

  if ( indx ) then
    x = 1.0D+00 - x
  end if

  return
end
