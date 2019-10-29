subroutine triangular_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! TRIANGULAR_PDF evaluates the Triangular PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 4 * ( X - A ) / ( B - A )^2 for A <= X <= (A+B)/2
!               = 4 * ( B - X ) / ( B - A )^2 for (A+B)/2 <= X <= B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x <= a ) then
    pdf = 0.0D+00
  else if ( x <= 0.5D+00 * ( a + b ) ) then
    pdf = 4.0D+00 * ( x - a ) / ( b - a ) ** 2
  else if ( x <= b ) then
    pdf = 4.0D+00 * ( b - x ) / ( b - a ) ** 2
  else
    pdf = 0.0D+00
  end if

  return
end
