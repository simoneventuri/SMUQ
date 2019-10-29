subroutine anglit_pdf ( x, pdf )

!*****************************************************************************80
!
!! ANGLIT_PDF evaluates the Anglit PDF.
!
!  Discussion:
!
!    PDF(X) = sin ( 2 * X + PI / 2 ) for -PI/4 <= X <= PI/4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( x <= - 0.25D+00 * r8_pi .or. 0.25D+00 * r8_pi <= x ) then
    pdf = 0.0D+00
  else
    pdf = sin ( 2.0D+00 * x + 0.25D+00 * r8_pi )
  end if

  return
end
