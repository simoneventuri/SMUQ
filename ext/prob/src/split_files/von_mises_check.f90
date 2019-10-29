function von_mises_check ( a, b )

!*****************************************************************************80
!
!! VON_MISES_CHECK checks the parameters of the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, logical VON_MISES_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  logical von_mises_check

  if ( a < - r8_pi .or. r8_pi < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VON_MISES_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < -PI or PI < A.'
    von_mises_check = .false.
    return
  end if

  if ( b < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VON_MISES_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < 0.0'
    von_mises_check = .false.
    return
  end if

  von_mises_check = .true.

  return
end
