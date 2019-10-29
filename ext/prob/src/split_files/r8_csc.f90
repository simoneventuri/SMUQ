function r8_csc ( theta )

!*****************************************************************************80
!
!! R8_CSC returns the cosecant of X.
!
!  Discussion:
!
!    CSC ( THETA ) = 1.0 / SIN ( THETA )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) THETA, the angle, in radians, whose
!    cosecant is desired.  It must be the case that SIN ( THETA ) is not zero.
!
!    Output, real ( kind = 8 ) R8_CSC, the cosecant of THETA.
!
  implicit none

  real ( kind = 8 ) r8_csc
  real ( kind = 8 ) theta

  r8_csc = sin ( theta )

  if ( r8_csc == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CSC - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R8_CSC undefined for THETA = ', theta
    stop 1
  end if

  r8_csc = 1.0D+00 / r8_csc

  return
end
