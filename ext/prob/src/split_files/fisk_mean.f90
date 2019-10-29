subroutine fisk_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! FISK_MEAN returns the mean of the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_csc

  if ( c <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISK_MEAN - Fatal error!'
    write ( *, '(a)' ) '  No mean defined for C <= 1.0'
    stop 1
  end if

  mean = a + r8_pi * ( b / c ) * r8_csc ( r8_pi / c )

  return
end
