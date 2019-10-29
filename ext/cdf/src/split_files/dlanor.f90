function dlanor ( x )

!*****************************************************************************80
!
!! DLANOR evaluates the logarithm of the asymptotic Normal CDF.
!
!  Discussion:
!
!    This routine computes the logarithm of the cumulative normal distribution
!    from abs ( x ) to infinity for  5 <= abs ( X ).
!
!    The relative error at X = 5 is about 0.5D-5.
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.2.12.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value at which the Normal CDF is to be
!    evaluated.  It is assumed that 5 <= abs ( X ).
!
!    Output, real ( kind = 8 ) DLANOR, the logarithm of the asymptotic
!    Normal CDF.
!
  implicit none

  real ( kind = 8 ) alnrel
  real ( kind = 8 ) approx
  real ( kind = 8 ), save, dimension ( 0:11 ) :: coef = (/ &
    -1.0D+00,  3.0D+00,  -15.0D+00,  105.0D+00,  -945.0D+00,  &
    10395.0D+00, -135135.0D+00,  2027025.0D+00,  -34459425.0D+00, &
    654729075.0D+00, -13749310575D+00,  316234143225.0D+00 /)
  real ( kind = 8 ) correc
  real ( kind = 8 ), parameter :: dlsqpi = 0.91893853320467274177D+00
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) dlanor
  real ( kind = 8 ) x
  real ( kind = 8 ) xx
  real ( kind = 8 ) xx2

  xx = abs ( x )

  if ( abs ( x ) < 5.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DLANOR - Fatal error!'
    write ( *, '(a)' ) '  The argument X is too small.'
  end if

  approx = - dlsqpi - 0.5D+00 * x**2 - log ( abs ( x ) )

  xx2 = xx * xx
  correc = eval_pol ( coef, 11, 1.0D+00 / xx2 ) / xx2
  correc = alnrel ( correc )

  dlanor = approx + correc

  return
end
