subroutine gamma_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! GAMMA_PDF evaluates the Gamma PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = exp ( - ( X - A ) / B ) * ( ( X - A ) / B )^(C-1)
!      / ( B * GAMMA ( C ) )
!
!    GAMMA_PDF(A,B,C;X), where C is an integer, is the Erlang PDF.
!    GAMMA_PDF(A,B,1;X) is the Exponential PDF.
!    GAMMA_PDF(0,2,C/2;X) is the Chi Squared PDF with C degrees of freedom.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
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
!    A controls the location of the peak;  A is often chosen to be 0.0.
!    B is the "scale" parameter; 0.0 < B, and is often 1.0.
!    C is the "shape" parameter; 0.0 < C, and is often 1.0.
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

    pdf = y ** ( c - 1.0D+00 ) / ( b * gamma ( c ) * exp ( y ) )

  end if

  return
end
