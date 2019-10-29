subroutine pearson_05_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! PEARSON_05_PDF evaluates the Pearson 5 PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = A^B * ( X - C )^(-B-1)
!      * exp ( - A / ( X - C ) ) / Gamma ( B )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    C < X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x <= c ) then
    pdf = 0.0D+00
  else
    pdf = ( a ** b ) * ( x - c ) ** ( - b - 1.0D+00 ) &
      * exp ( - a / ( x - c ) ) / gamma ( b )
  end if

  return
end
