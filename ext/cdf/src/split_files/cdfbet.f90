subroutine cdfbet ( which, p, q, x, y, a, b, status, bound )

!*****************************************************************************80
!
!! CDFBET evaluates the CDF of the Beta Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the beta distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly by code associated with the reference.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The beta density is proportional to t^(A-1) * (1-t)^(B-1).
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
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which of the next four
!    argument values is to be calculated from the others.
!    1: Calculate P and Q from X, Y, A and B;
!    2: Calculate X and Y from P, Q, A and B;
!    3: Calculate A from P, Q, X, Y and B;
!    4: Calculate B from P, Q, X, Y and A.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    chi-square distribution.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) Q, equals 1-P.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration
!    of the beta density.  If it is an input value, it should lie in
!    the range [0,1].  If it is an output value, it will be searched for
!    in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Y, equal to 1-X.  If it is an input
!    value, it should lie in the range [0,1].  If it is an output value,
!    it will be searched for in the range [0,1].
!
!    Input/output, real ( kind = 8 ) A, the first parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Input/output, real ( kind = 8 ) B, the second parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if X + Y /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) b
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) y

  status = 0
  bound = 0.0D+00

  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFBET - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      bound = 0.0D+00
      status = -2
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      bound = 1.0D+00
      status = -2
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      bound = 0.0D+00
      status = -3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      bound = 1.0D+00
      status = -3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
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
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    else if ( 1.0D+00 < x ) then
      bound = 1.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless Y is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( y < 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Y is out of range.'
      return
    else if ( 1.0D+00 < y ) then
      bound = 1.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Y is out of range.'
      return
    end if
  end if
!
!  Unless A is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( a <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter A is out of range.'
      return
    end if
  end if
!
!  Unless B is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( b <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  Input parameter B is out of range.'
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
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Check that X + Y = 1.
!
  if ( which /= 2 ) then
    if ( 3.0D+00 * epsilon ( x ) < abs ( ( x + y ) - 1.0D+00 ) ) then
      status = 4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFBET - Fatal error!'
      write ( *, '(a)' ) '  X + Y /= 1.'
      return
    end if
  end if
!
!  Compute P and Q.
!
  if ( which == 1 ) then

    call cumbet ( x, y, a, b, p, q )
    status = 0
!
!  Compute X and Y.
!
  else if ( which == 2 ) then

    call dstzr ( 0.0D+00, 1.0D+00, atol, tol )

    if ( p <= q ) then

      status = 0
      fx = 0.0D+00
      call dzror ( status, x, fx, xlo, xhi, qleft, qhi )
      y = 1.0D+00 - x

      do while ( status == 1 )

        call cumbet ( x, y, a, b, cum, ccum )
        fx = cum - p
        call dzror ( status, x, fx, xlo, xhi, qleft, qhi )
        y = 1.0D+00 - x

      end do

    else

      status = 0
      fx = 0.0D+00
      call dzror ( status, y, fx, xlo, xhi, qleft, qhi )
      x = 1.0D+00 - y

      do while ( status == 1 )

        call cumbet ( x, y, a, b, cum, ccum )
        fx = ccum - q
        call dzror ( status, y, fx, xlo, xhi, qleft, qhi )
        x = 1.0D+00 - y

      end do

    end if

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = 1.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Compute A.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    a = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, a, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbet ( x, y, a, b, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, a, fx, qleft, qhi )

    end do

    if ( status == -1 ) then

      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if

    end if
!
!  Compute B.
!
  else if ( which == 4 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    b = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, b, fx, qleft, qhi )

    do while ( status == 1 )

      call cumbet ( x, y, a, b, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, b, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFBET - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
