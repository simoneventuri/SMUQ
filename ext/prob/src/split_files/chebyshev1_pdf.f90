subroutine chebyshev1_pdf ( x, pdf )

!*****************************************************************************80
!
!! CHEBYSHEV1_PDF evaluates the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( x < -1.0D+00 .or. 1.0D+00 < x ) then
    pdf = 0.0D+00
  else
    pdf = 1.0D+00 / r8_pi / sqrt ( 1.0D+00 - x * x )
  end if

  return
end
