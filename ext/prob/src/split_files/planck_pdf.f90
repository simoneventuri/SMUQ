subroutine planck_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! PLANCK_PDF evaluates the Planck PDF.
!
!  Discussion:
!
!    The Planck PDF has the form
!
!      PDF(A,B;X) = A^(B+1) * X^B / ( exp ( A * X ) - 1 ) / K
!
!    where K is the normalization constant, and has the value
!
!      K = Gamma ( B + 1 ) * Zeta ( B + 1 ).
!
!    The original Planck distribution governed the frequencies in
!    blackbody radiation at a given temperature T, and has the form
!
!      PDF(A;X) = K * X^3 / ( exp ( A * X ) - 1 )
!
!    with
!
!      K = 15 / PI^4.
!
!    Thus, in terms of the Planck PDF, the original Planck distribution
!    has A = 1, B = 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) k
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_zeta
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    k = gamma ( b + 1.0D+00 ) * r8_zeta ( b + 1.0D+00 )
    pdf = a ** ( b + 1.0D+00 ) * x ** b / ( exp ( a * x ) - 1.0D+00 ) / k
  end if

  return
end
