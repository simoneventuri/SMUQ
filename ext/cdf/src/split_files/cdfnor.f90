subroutine cdfnor ( which, p, q, x, mean, sd, status, bound )

!*****************************************************************************80
!
!! CDFNOR evaluates the CDF of the Normal distribution.
!
!  Discussion:
!
!    A slightly modified version of ANORM from SPECFUN
!    is used to calculate the cumulative standard normal distribution.
!
!    The rational functions from pages 90-95 of Kennedy and Gentle
!    are used as starting values to a Newton iteration which
!    compute the inverse standard normal.  Therefore no searches are
!    necessary for any parameter.
!
!    For X < -15, the asymptotic expansion for the normal is used  as
!    the starting value in finding the inverse standard normal.
!
!    The normal density is proportional to
!    exp ( - 0.5D+00 * (( X - MEAN)/SD)**2)
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
!    1966, Formula 26.2.12.
!
!    William Cody,
!    Algorithm 715:
!    SPECFUN - A Portable FORTRAN Package of
!    Special Function Routines and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, pages 22-32, 1993.
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, MEAN and SD;
!    2: Calculate X from P, Q, MEAN and SD;
!    3: Calculate MEAN from P, Q, X and SD;
!    4: Calculate SD from P, Q, X and MEAN.
!
!    Input/output, real ( kind = 8 ) P, the integral from -infinity to X
!    of the Normal density.  If this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Normal density.
!
!    Input/output, real ( kind = 8 ) MEAN, the mean of the Normal density.
!
!    Input/output, real ( kind = 8 ) SD, the standard deviation of the
!    Normal density.  If this is an input value, it should lie in the
!    range (0,+infinity).
!
!    Output, integer ( kind = 4 ) STATUS, the status of the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if answer appears to be lower than lowest search bound;
!    2, if answer appears to be higher than greatest search bound;
!    3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ) bound
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) mean
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) sd
  integer ( kind = 4 ) status
  integer ( kind = 4 ) which
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  status = 0

  if ( which < 1 ) then
    status = -1
    bound = 1.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNOR - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  else if ( 4 < which ) then
    status = -1
    bound = 4.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFNOR - Fatal error!'
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
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
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
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
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
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if

  if ( which /= 4 ) then
    if ( sd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFNOR - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SD is out of range.'
      return
    end if
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    z = ( x - mean ) / sd
    call cumnor ( z, p, q )
!
!  Calculate X.
!
  else if ( which == 2 ) then

    z = dinvnr ( p, q )
    x = sd * z + mean
!
!  Calculate MEAN.
!
  else if ( which == 3 ) then

    z = dinvnr ( p, q )
    mean = x - sd * z
!
!  Calculate SD.
!
  else if ( which == 4 ) then

    z = dinvnr ( p, q )
    sd = ( x - mean ) / z

  end if

  return
end
