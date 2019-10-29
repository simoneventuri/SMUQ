subroutine fisk_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! FISK_VARIANCE returns the variance of the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) g
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_csc
  real ( kind = 8 ) variance

  if ( c <= 2.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISK_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  No variance defined for C <= 2.0'
    stop 1
  end if

  g = r8_pi / c

  variance = b * b &
    * ( 2.0D+00 * g * r8_csc ( 2.0D+00 * g ) - ( g * r8_csc ( g ) ) ** 2 )

  return
end
