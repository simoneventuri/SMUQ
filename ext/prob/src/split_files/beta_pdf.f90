subroutine beta_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! BETA_PDF evaluates the Beta PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!      PDF(A,B;X) = X^(A-1) * (1-X)^(B-1) / BETA(A,B).
!
!    A = B = 1 yields the Uniform distribution on [0,1].
!    A = B = 1/2 yields the Arcsin distribution.
!        B = 1 yields the power function distribution.
!    A = B -> Infinity tends to the Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) x

  if ( x < 0.0D+00 .or. 1.0D+00 < x ) then
    pdf = 0.0D+00
  else
    pdf = x ** ( a - 1.0D+00 ) * ( 1.0D+00 - x ) ** ( b - 1.0D+00 ) &
      / r8_beta ( a, b )
  end if

  return
end
