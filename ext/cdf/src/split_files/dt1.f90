function dt1 ( p, q, df )

!*****************************************************************************80
!
!! DT1 computes an approximate inverse of the cumulative T distribution.
!
!  Discussion:
!
!    This routine returns the inverse of the T distribution function, that is,
!    the integral from 0 to INVT of the T density is P.  This is an
!    initial approximation.
!
!    Thanks to Charles Katholi for pointing out that the RESHAPE
!    function should not use a range in the "SHAPE" field (0:4,4),
!    but simply the number of rows and columns (5,4), JVB, 04 May 2006.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the value whose inverse from the
!    T distribution CDF is desired, and the value (1-P).
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of the
!    T distribution.
!
!    Output, real ( kind = 8 ) DT1, the approximate value of X for which
!    the T density CDF with DF degrees of freedom has value P.
!
  implicit none

  real ( kind = 8 ), dimension(0:4,4) :: coef = reshape ( (/ &
       1.0D+00,     1.0D+00,    0.0D+00,   0.0D+00,  0.0D+00, &
       3.0D+00,    16.0D+00,    5.0D+00,   0.0D+00,  0.0D+00, &
     -15.0D+00,    17.0D+00,   19.0D+00,   3.0D+00,  0.0D+00, &
    -945.0D+00, -1920.0D+00, 1482.0D+00, 776.0D+00, 79.0D+00/), (/ 5, 4 /) )
  real ( kind = 8 ), parameter, dimension ( 4 ) :: denom = (/ &
    4.0D+00, 96.0D+00, 384.0D+00, 92160.0D+00 /)
  real ( kind = 8 ) denpow
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) df
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) dt1
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter, dimension ( 4 ) :: ideg = (/ 1, 2, 3, 4 /)
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) sum1
  real ( kind = 8 ) term
  real ( kind = 8 ) x
  real ( kind = 8 ) xp
  real ( kind = 8 ) xx

  x = abs ( dinvnr ( p, q ) )
  xx = x * x

  sum1 = x
  denpow = 1.0D+00
  do i = 1, 4
    term = eval_pol ( coef(0,i), ideg(i), xx ) * x
    denpow = denpow * df
    sum1 = sum1 + term / ( denpow * denom(i) )
  end do

  if ( 0.5D+00 <= p ) then
    xp = sum1
  else
    xp = -sum1
  end if

  dt1 = xp

  return
end
