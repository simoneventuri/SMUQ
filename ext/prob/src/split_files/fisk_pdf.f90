subroutine fisk_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! FISK_PDF evaluates the Fisk PDF.
!
!  Discussion:
!
!    The Fisk PDF is also known as the Log Logistic PDF.
!
!    The formula for the PDF is:
!
!    PDF(A,B,C;X) =
!      ( C / B ) * ( ( X - A ) / B )^( C - 1 ) /
!      ( 1 + ( ( X - A ) / B )^C )^2
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
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= a ) then

    pdf = 0.0D+00

  else

    y = ( x - a ) / b

    pdf = ( c / b ) * y ** ( c - 1.0D+00 ) / ( 1.0D+00 + y ** c ) ** 2

  end if

  return
end
