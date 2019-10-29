subroutine cdff ( which, p, q, f, dfn, dfd, status, bound )

!*****************************************************************************80
!
!! CDFF evaluates the CDF of the F distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The value of the cumulative F distribution is not necessarily
!    monotone in either degrees of freedom.  There thus may be two
!    values that provide a given CDF value.  This routine assumes
!    monotonicity and will find an arbitrary one of the two values.
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
!    1966, Formula 26.6.2.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN and DFD;
!    2: Calculate F from P, Q, DFN and DFD;
!    3: Calculate DFN from P, Q, F and DFD;
!    4: Calculate DFD from P, Q, F and DFN.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the F-density.  If it is an input value, it should lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the F-density.  If this is an input value, it should lie in the
!    range [0, +infinity).  If it is an output value, it will be searched
!    for in the range [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of
!    freedom of the numerator sum of squares.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the  range: [ 1.0D-300, 1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) bound_hi
  real ( kind = 8 ) bound_lo
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) f
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFF - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFF - Fatal error!'
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
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless F is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( f < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter F is out of range.'
      return
    end if
  end if
!
!  Unless DFN is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( dfn <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFN is out of range.'
      return
    end if
  end if
!
!  Unless DFD is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( dfd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFD is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+00 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFF - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumf ( f, dfn, dfd, p, q )
    status = 0
!
!  Calculate F.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    f = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, f, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, f, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if
!
!  Calculate DFN.
!
!  Note that, in the original calculation, the lower bound for DFN was 0.
!  Using DFN = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
  else if ( which == 3 ) then

    bound_lo = 1.0D+00
    bound_hi = inf

    call dstinv ( bound_lo, bound_hi, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfn = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfn, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, dfn, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = bound_lo
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_lo
        return
      else
        status = 2
        bound = bound_hi
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_hi
        return
      end if

    end if
!
!  Calculate DFD.
!
!  Note that, in the original calculation, the lower bound for DFD was 0.
!  Using DFD = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
  else if ( which == 4 ) then

    bound_lo = 1.0D+00
    bound_hi = inf

    call dstinv ( bound_lo, bound_hi, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfd = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfd, fx, qleft, qhi )

    do while ( status == 1 )

      call cumf ( f, dfn, dfd, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, dfd, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = bound_lo
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_lo
      else
        status = 2
        bound = bound_hi
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFF - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound_hi
      end if
    end if

  end if

  return
end
