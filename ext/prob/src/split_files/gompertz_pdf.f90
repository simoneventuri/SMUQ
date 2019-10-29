subroutine gompertz_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! GOMPERTZ_PDF evaluates the Gompertz PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B * A^X / exp ( B * ( A^X - 1 ) / log ( A ) )
!
!    for
!
!      0.0 <= X
!      1.0 <  A
!      0.0 <  B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then

    pdf = 0.0D+00

  else if ( 1.0D+00 < a ) then

    pdf = exp ( log ( b ) + x * log ( a ) &
      - ( b / log ( a ) ) * ( a**x - 1.0D+00 ) )

  end if

  return
end
