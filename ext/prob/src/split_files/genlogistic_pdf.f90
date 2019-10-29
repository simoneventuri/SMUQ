subroutine genlogistic_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! GENLOGISTIC_PDF evaluates the Generalized Logistic PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( C / B ) * exp ( ( A - X ) / B ) /
!      ( ( 1 + exp ( ( A - X ) / B ) )^(C+1) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
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

  y = ( x - a ) / b

  pdf = ( c / b ) * exp ( - y ) / ( 1.0D+00 + exp ( - y ) ) ** ( c + 1.0D+00 )

  return
end
