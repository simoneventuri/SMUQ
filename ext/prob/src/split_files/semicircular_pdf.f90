subroutine semicircular_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! SEMICIRCULAR_PDF evaluates the Semicircular PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = ( 2 / ( B * PI ) ) * SQRT ( 1 - ( ( X - A ) / B )^2 )
!    for A - B <= X <= A + B
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

  if ( x < a - b ) then

    pdf = 0.0D+00

  else if ( x <= a + b ) then

    y = ( x - a ) / b

    pdf = 2.0D+00 / ( b * r8_pi ) * sqrt ( 1.0D+00 - y ** 2 )

  else if ( a + b < x ) then

    pdf = 0.0D+00

  end if

  return
end
