subroutine cdfpoi ( which, p, q, s, xlam, status, bound )

!*****************************************************************************80
!
!! CDFPOI evaluates the CDF of the Poisson distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Poisson distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
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
!    1966, Formula 26.4.21.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from S and XLAM;
!    2: Calculate A from P, Q and XLAM;
!    3: Calculate XLAM from P, Q and S.
!
!    Input/output, real ( kind = 8 ) P, the cumulation from 0 to S of the
!    Poisson density.  Whether this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) S, the upper limit of cumulation of
!    the Poisson CDF.  If this is an input value, it should lie in
!    the range: [0, +infinity).  If it is an output value, it will be
!    searched for in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) XLAM, the mean of the Poisson
!    distribution.  If this is an input value, it should lie in the range
!    [0, +infinity).  If it is an output value, it will be searched for
!    in the range: [0,1E300].
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
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) s
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which
  real ( kind = 8 ) xlam

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFPOI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
    return
  end if

  if ( 3 < which ) then
    bound = 3.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFPOI - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 3.'
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
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
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
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless S is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( s < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter S is out of range.'
      return
    end if
  end if
!
!  Unless XLAM is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( xlam < 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  Input parameter XLAM is out of range.'
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
      write ( *, '(a)' ) 'CDFPOI - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    call cumpoi ( s, xlam, p, q )
    status = 0
!
!  Calculate S.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    s = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, s, fx, qleft, qhi )

    do while ( status == 1 )

      call cumpoi ( s, xlam, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, s, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate XLAM.
!
  else if ( which == 3 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    xlam = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, xlam, fx, qleft, qhi )

    do while ( status == 1 )

      call cumpoi ( s, xlam, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      call dinvr ( status, xlam, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFPOI - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end
