subroutine extreme_values_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! EXTREME_VALUES_PDF evaluates the Extreme Values PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!      ( 1 / B ) * exp ( ( A - X ) / B ) * exp ( - exp ( ( A - X ) / B  ) ).
!
!    The Extreme Values PDF is also known as the Fisher-Tippet PDF
!    and the Log-Weibull PDF.
!
!    The special case A = 0 and B = 1 is the Gumbel PDF.
!
!    The Extreme Values PDF is the limiting distribution for the
!    smallest or largest value in a large sample drawn from
!    any of a great variety of distributions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
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
  real ( kind = 8 ) x

  pdf = ( 1.0D+00 / b ) * exp ( ( a - x ) / b - exp ( ( a - x ) / b ) )

  return
end
