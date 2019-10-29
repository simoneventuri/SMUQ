subroutine uniform_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! UNIFORM_01_PDF evaluates the Uniform 01 PDF.
!
!  Discussion:
!
!    PDF(X) = 1 for 0 <= X <= 1
!           = 0 otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
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
  real ( kind = 8 ) x

  if ( x < 0.0D+00 .or. 1.0D+00 < x ) then
    pdf = 0.0D+00
  else
    pdf = 1.0D+00
  end if

  return
end
