subroutine half_normal_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! HALF_NORMAL_PDF evaluates the Half Normal PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!      sqrt ( 2 / PI ) * ( 1 / B ) * exp ( - 0.5D+00 * ( ( X - A ) / B )^2 )
!
!    for A <= X
!
!    The Half Normal PDF is a special case of both the Chi PDF and the
!    Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
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

  if ( x <= a ) then

    pdf = 0.0D+00

  else

    y = ( x - a ) / b

    pdf = sqrt ( 2.0D+00 / r8_pi ) * ( 1.0D+00 / b ) * exp ( - 0.5D+00 * y * y )

  end if

  return
end
