subroutine cosine_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! COSINE_PDF evaluates the Cosine PDF.
!
!  Discussion:
!
!    The cosine PDF can be thought of as being applied to points on
!    a circle.
!
!    PDF(A,B;X) = ( 1 / ( 2 * PI * B ) ) * COS ( ( X - A ) / B )
!    for A - PI * B <= X <= A + PI * B
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x < a - r8_pi * b ) then
    pdf = 0.0D+00

  else if ( x <= a + r8_pi * b ) then

    y = ( x - a ) / b

    pdf = 1.0D+00 / ( 2.0D+00 * r8_pi * b ) * cos ( y )

  else if ( a + r8_pi * b < x ) then

    pdf = 0.0D+00

  end if

  return
end
