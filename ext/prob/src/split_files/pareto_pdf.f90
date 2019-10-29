subroutine pareto_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! PARETO_PDF evaluates the Pareto PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B * A^B / X^(B+1).
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < a ) then
    pdf = 0.0D+00
  else
    pdf = b * ( a ** b ) / x ** ( b + 1.0D+00 )
  end if

  return
end
