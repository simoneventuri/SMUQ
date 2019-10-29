subroutine power_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! POWER_PDF evaluates the Power PDF.
!
!  Discussion:
!
!    PDF(A;X) = (A/B) * (X/B)^(A-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall/CRC, 2000, pages 152-153.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= B.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 .or. b < x ) then
    pdf = 0.0D+00
  else
    pdf = ( a / b ) * ( x / b ) ** ( a - 1.0D+00 )
  end if

  return
end
