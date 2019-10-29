subroutine exponential_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! EXPONENTIAL_01_PDF evaluates the Exponential 01 PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - X )
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = exp ( - x )
  end if

  return
end
