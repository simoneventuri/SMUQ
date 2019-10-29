subroutine cumfnc ( f, dfn, dfd, pnonc, cum, ccum )

!*****************************************************************************80
!
!! CUMFNC evaluates the cumulative noncentral F distribution.
!
!  Discussion:
!
!    This routine computes the noncentral F distribution with DFN and DFD
!    degrees of freedom and noncentrality parameter PNONC.
!
!    The series is calculated backward and forward from J = LAMBDA/2
!    (this is the term with the largest Poisson weight) until
!    the convergence criterion is met.
!
!    The sum continues until a succeeding term is less than EPS
!    times the sum or the sum is very small.  EPS is
!    set to 1.0D-4 in a data statement which can be changed.
!
!    The original version of this routine allowed the input values
!    of DFN and DFD to be negative (nonsensical) or zero (which
!    caused numerical overflow.)  I have forced both these values
!    to be at least 1.
!
!  Modified:
!
!    19 May 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.16, 26.6.17, 26.6.18, 26.6.20.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of freedom
!    in the numerator and denominator.  Both DFN and DFD must be positive,
!    and normally would be integers.  This routine requires that they
!    be no less than 1.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the noncentral F CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) adn
  real ( kind = 8 ) arg1
  real ( kind = 8 ) aup
  real ( kind = 8 ) b
  real ( kind = 8 ) betdn
  real ( kind = 8 ) betup
  real ( kind = 8 ) centwt
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) dnterm
  real ( kind = 8 ) dsum
  real ( kind = 8 ) dummy
  real ( kind = 8 ), parameter :: eps = 0.0001D+00
  real ( kind = 8 ) expon
  real ( kind = 8 ) f
  real ( kind = 8 ) gamma_log
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icent
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) prod
  real ( kind = 8 ) sum1
  real ( kind = 8 ) upterm
  real ( kind = 8 ) x
  real ( kind = 8 ) xmult
  real ( kind = 8 ) xnonc
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  if ( f <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if

  if ( dfn < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CUMFNC - Fatal error!'
    write ( *, '(a)' ) '  DFN < 1.'
    stop
  end if

  if ( dfd < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CUMFNC - Fatal error!'
    write ( *, '(a)' ) '  DFD < 1.'
    stop
  end if
!
!  Handle case in which the noncentrality parameter is essentially zero.
!
  if ( pnonc < 1.0D-10 ) then
    call cumf ( f, dfn, dfd, cum, ccum )
    return
  end if

  xnonc = pnonc /  2.0D+00
!
!  Calculate the central term of the Poisson weighting factor.
!
  icent = int ( xnonc )

  if ( icent == 0 ) then
    icent = 1
  end if
!
!  Compute central weight term.
!
  centwt = exp ( -xnonc + icent * log ( xnonc ) &
    - gamma_log ( real ( icent + 1, kind = 8  ) ) )
!
!  Compute central incomplete beta term.
!  Ensure that minimum of arg to beta and 1 - arg is computed accurately.
!
  prod = dfn * f
  dsum = dfd + prod
  yy = dfd / dsum

  if ( 0.5D+00 < yy ) then
    xx = prod / dsum
    yy = 1.0D+00 - xx
  else
    xx = 1.0D+00 - yy
  end if

  arg1 = 0.5D+00 * dfn + real ( icent, kind = 8 )
  call beta_inc ( arg1, 0.5D+00*dfd, xx, yy, betdn, dummy, ierr )

  adn = dfn / 2.0D+00 + real ( icent, kind = 8 )
  aup = adn
  b = dfd / 2.0D+00
  betup = betdn
  sum1 = centwt * betdn
!
!  Now sum terms backward from ICENT until convergence or all done.
!
  xmult = centwt
  i = icent
  dnterm = exp ( gamma_log ( adn + b ) &
    - gamma_log ( adn + 1.0D+00 ) &
    - gamma_log ( b ) + adn * log ( xx ) + b * log ( yy ) )

  do

    if ( i <= 0 ) then
      exit
    end if

    if ( sum1 < epsilon ( xmult * betdn ) .or. &
         xmult * betdn < eps * sum1 ) then
      exit
    end if

    xmult = xmult * ( real ( i, kind = 8 ) / xnonc )
    i = i - 1
    adn = adn - 1.0D+00
    dnterm = ( adn + 1.0D+00 ) / ( ( adn + b ) * xx ) * dnterm
    betdn = betdn + dnterm
    sum1 = sum1 + xmult * betdn

  end do

  i = icent + 1
!
!  Now sum forward until convergence.
!
  xmult = centwt

  if ( ( aup - 1.0D+00 + b ) == 0 ) then

    expon = - gamma_log ( aup ) - gamma_log ( b ) &
      + ( aup - 1.0D+00 ) * log ( xx ) + b * log ( yy )

  else

    expon = gamma_log ( aup - 1.0D+00 + b ) - gamma_log ( aup ) &
      - gamma_log ( b ) + ( aup - 1.0D+00 ) * log ( xx ) + b * log ( yy )

  end if
!
!  The fact that DCDFLIB blithely assumes that 1.0E+30 is a reasonable
!  value to plug into any function, and that G95 computes corresponding
!  function values of, say 1.0E-303, and then chokes with a floating point
!  error when asked to combine such a value with a reasonable floating
!  point quantity, has driven me to the following sort of check that
!  was last fashionable in the 1960's!
!
  if ( expon <= log ( epsilon ( expon ) ) ) then
    upterm = 0.0D+00
  else
    upterm = exp ( expon )
  end if

  do

    xmult = xmult * ( xnonc / real ( i, kind = 8 ) )
    i = i + 1
    aup = aup + 1.0D+00
    upterm = ( aup + b -  2.0D+00  ) * xx / ( aup - 1.0D+00 ) * upterm
    betup = betup - upterm
    sum1 = sum1 + xmult * betup

    if ( sum1 < epsilon ( xmult * betup ) .or. xmult * betup < eps * sum1 ) then
      exit
    end if

  end do

  cum = sum1
  ccum = 0.5D+00 + ( 0.5D+00 - cum )

  return
end
