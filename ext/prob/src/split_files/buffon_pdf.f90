subroutine buffon_pdf ( a, l, pdf )

!*****************************************************************************80
!
!! BUFFON_PDF evaluates the Buffon PDF.
!
!  Discussion:
!
!    In the Buffon needle experiment, we suppose that the plane has been
!    ruled by vertical lines with a spacing of A units, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    Because of the various symmetries, we may assume that this eye of
!    this needle lands in the first infinite strip, and we may further
!    assume that its Y coordinate is 0.  Thus, we have
!    the eye as (X1,Y1) with 0 <= X1 <= A and Y1 = 0.
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2.
!
!    The probability of a crossing on a single trial is
!
!      P(A,L) = ( 2 * L ) / ( PI * A )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the horizontal spacing between the
!    vertical grid lines.  0 <= A.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!
!    Output, real ( kind = 8 ) PDF, the Buffon PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) l
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793238462643D+00

  if ( a < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BUFFON_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input A < 0.'
    stop 1
  else if ( a == 0.0D+00 ) then
    pdf = 1.0D+00
    return
  end if

  if ( l < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BUFFON_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input L < 0.'
    stop 1
  else if ( l == 0.0D+00 ) then
    pdf = 0.0D+00
    return
  end if

  pdf = ( 2.0D+00 * l ) / ( r8_pi * a )

  return
end
