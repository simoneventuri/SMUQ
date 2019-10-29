subroutine weibull_discrete_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_PDF evaluates the discrete Weibull PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = ( 1 - A )^X^B - ( 1 - A )^(X+1)^B.
!
!    WEIBULL_DISCRETE_PDF(A,1;X) = GEOMETRIC_PDF(A;X)
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters that define the PDF.
!    0 <= A <= 1,
!    0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x < 0 ) then
    pdf = 0.0D+00
  else
    pdf = ( 1.0D+00 - a ) ** ( x ** b ) - ( 1.0D+00 - a ) ** ( ( x + 1 ) ** b )
  end if

  return
end
