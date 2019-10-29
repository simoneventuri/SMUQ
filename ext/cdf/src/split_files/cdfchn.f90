subroutine cdfchn ( which, p, q, x, df, pnonc, status, bound )

!*****************************************************************************80
!
!! CDFCHN evaluates the CDF of the Noncentral Chi-Square.
!
!  Discussion:
!
!    This routine calculates any one parameter of the noncentral chi-square
!    distribution given values for the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter (PNONC).  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.25.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, DF and PNONC;
!    2: Calculate X from P, DF and PNONC;
!    3: Calculate DF from P, X and PNONC;
!    4: Calculate PNONC from P, X and DF.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of
!    the noncentral chi-square distribution.  If this is an input
!    value, it should lie in the range: [0, 1.0-1.0D-16).
!
!    Input/output, real ( kind = 8 ) Q, is generally not used by this
!    subroutine and is only included for similarity with other routines.
!    However, if P is to be computed, then a value will also be computed
!    for Q.
!
!    Input, real ( kind = 8 ) X, the upper limit of integration of the
!    noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be sought in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DF, the number of degrees of freedom
!    of the noncentral chi-square distribution.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be searched for in the range: [0,1.0D+4]
!
!    Output, integer ( kind = 4 ) STATUS, reports on the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if the answer appears to be lower than the lowest search bound;
!    2, if the answer appears to be higher than the greatest search bound.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol=1.0D-50
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf=1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tent4=1.0D+04
  real ( kind = 8 ), parameter :: tol=1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) x

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFCHN - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless DF is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( df <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DF is out of range.'
      return
    end if
  end if
!
!  Unless PNONC is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( pnonc < 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFCHN - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PNONC is out of range.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumchn ( x, df, pnonc, p, q )
    status = 0
!
!  Calculate X.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    x = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, x, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, x, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate DF.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    df = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, df, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, df, fx, qleft, qhi )

    end do

    if ( status == -1 )then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate PNONC.
!
  else if ( which == 4 ) then

    call dstinv ( 0.0D+00, tent4, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    pnonc = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, pnonc, fx, qleft, qhi )

    do while ( status == 1 )

      call cumchn ( x, df, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, pnonc, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = tent4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFCHN - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
