function dipole_check ( a, b )

!*****************************************************************************80
!
!! DIPOLE_CHECK checks the parameters of the Dipole CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!    is interesting, and -1.0 <= B <= 1.0.
!
!    Output, logical DIPOLE_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical dipole_check

  if ( b < -1.0D+00 .or. 1.0D+00 < b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIPOLE_CHECK - Warning!'
    write ( *, '(a)' ) '  -1.0D+00 <= B <= 1.0D+00 is required.'
    write ( *, '(a,g14.6)' ) '  The input B = ', b
    dipole_check = .false.
    return
  end if

  dipole_check = .true.

  return
end
