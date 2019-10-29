subroutine cumchn ( x, df, pnonc, cum, ccum )

!*****************************************************************************80
!
!! CUMCHN evaluates the cumulative noncentral chi-square distribution.
!
!  Discussion:
!
!    This routine calculates the cumulative noncentral chi-square
!    distribution, i.e., the probability that a random variable
!    which follows the noncentral chi-square distribution, with
!    noncentrality parameter PNONC and continuous degrees of
!    freedom DF, is less than or equal to X.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.25.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the CDF and complementary
!    CDF of the noncentral chi-square distribution.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) EPS, the convergence criterion.  The sum
!    stops when a term is less than EPS * SUM.
!
!    Local, integer NTIRED, the maximum number of terms to be evaluated
!    in each sum.
!
!    Local, logical QCONV, is TRUE if convergence was achieved, that is,
!    the program did not stop on NTIRED criterion.
!
  implicit none

  real ( kind = 8 ) adj
  real ( kind = 8 ) ccum
  real ( kind = 8 ) centaj
  real ( kind = 8 ) centwt
  real ( kind = 8 ) chid2
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) dfd2
  real ( kind = 8 ) dg
  real ( kind = 8 ), parameter :: eps = 0.00001D+00
  real ( kind = 8 ) gamma_log
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icent
  integer ( kind = 4 ) iterb
  integer ( kind = 4 ) iterf
  real ( kind = 8 ) lcntaj
  real ( kind = 8 ) lcntwt
  real ( kind = 8 ) lfact
  integer ( kind = 4 ), parameter :: ntired = 1000
  real ( kind = 8 ) pcent
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) pterm
  logical qsmall
  real ( kind = 8 ) sum1
  real ( kind = 8 ) sumadj
  real ( kind = 8 ) term
  real ( kind = 8 ) wt
  real ( kind = 8 ) x
  real ( kind = 8 ) xnonc
  real ( kind = 8 ) xx

  qsmall ( xx ) = sum1 < 1.0D-20 .or. xx < eps * sum1
  dg(i) = df +  2.0D+00  * real ( i, kind = 8 )

  if ( x <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if
!
!  When the noncentrality parameter is (essentially) zero,
!  use cumulative chi-square distribution
!
  if ( pnonc <= 1.0D-10 ) then
    call cumchi ( x, df, cum, ccum )
    return
  end if

  xnonc = pnonc /  2.0D+00
!
!  The following code calculates the weight, chi-square, and
!  adjustment term for the central term in the infinite series.
!  The central term is the one in which the poisson weight is
!  greatest.  The adjustment term is the amount that must
!  be subtracted from the chi-square to move up two degrees
!  of freedom.
!
  icent = int ( xnonc )
  if ( icent == 0 ) then
    icent = 1
  end if

  chid2 = x /  2.0D+00
!
!  Calculate central weight term.
!
  lfact = gamma_log ( real ( icent + 1, kind = 8 ) )
  lcntwt = - xnonc + icent * log ( xnonc ) - lfact
  centwt = exp ( lcntwt )
!
!  Calculate central chi-square.
!
  call cumchi ( x, dg(icent), pcent, ccum )
!
!  Calculate central adjustment term.
!
  dfd2 = dg(icent) /  2.0D+00
  lfact = gamma_log ( 1.0D+00 + dfd2 )
  lcntaj = dfd2 * log ( chid2 ) - chid2 - lfact
  centaj = exp ( lcntaj )
  sum1 = centwt * pcent
!
!  Sum backwards from the central term towards zero.
!  Quit whenever either
!  (1) the zero term is reached, or
!  (2) the term gets small relative to the sum, or
!  (3) More than NTIRED terms are totaled.
!
  iterb = 0
  sumadj = 0.0D+00
  adj = centaj
  wt = centwt
  i = icent
  term = 0.0D+00

  do

    dfd2 = dg(i) /  2.0D+00
!
!  Adjust chi-square for two fewer degrees of freedom.
!  The adjusted value ends up in PTERM.
!
    adj = adj * dfd2 / chid2
    sumadj = sumadj + adj
    pterm = pcent + sumadj
!
!  Adjust Poisson weight for J decreased by one.
!
    wt = wt * ( i / xnonc )
    term = wt * pterm
    sum1 = sum1 + term
    i = i - 1
    iterb = iterb + 1

    if ( ntired < iterb .or. qsmall ( term ) .or. i == 0 ) then
      exit
    end if

  end do

  iterf = 0
!
!  Now sum forward from the central term towards infinity.
!  Quit when either
!    (1) the term gets small relative to the sum, or
!    (2) More than NTIRED terms are totaled.
!
  sumadj = centaj
  adj = centaj
  wt = centwt
  i = icent
!
!  Update weights for next higher J.
!
  do

    wt = wt * ( xnonc / ( i + 1 ) )
!
!  Calculate PTERM and add term to sum.
!
    pterm = pcent - sumadj
    term = wt * pterm
    sum1 = sum1 + term
!
!  Update adjustment term for DF for next iteration.
!
    i = i + 1
    dfd2 = dg(i) /  2.0D+00
    adj = adj * chid2 / dfd2
    sumadj = sumadj + adj
    iterf = iterf + 1

    if ( ntired < iterf .or. qsmall ( term ) ) then
      exit
    end if

  end do

  cum = sum1
  ccum = 0.5D+00 + ( 0.5D+00 - cum )

  return
end
