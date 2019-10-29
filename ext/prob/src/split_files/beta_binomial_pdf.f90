subroutine beta_binomial_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! BETA_BINOMIAL_PDF evaluates the Beta Binomial PDF.
!
!  Discussion:
!
!    The PDF is defined as:
!
!      PDF(A,B,C;X) = Beta(A+X,B+C-X)
!        / ( (C+1) * Beta(X+1,C-X+1) * Beta(A,B) )  for 0 <= X <= C.
!
!    This PDF can be reformulated as:
!
!      The beta binomial probability density function for X successes
!      out of N trials is
!
!      PDF2(X)( N, MU, THETA ) =
!        C(N,X) * Product ( 0 <= R <= X - 1 ) ( MU + R * THETA )
!               * Product ( 0 <= R <= N - X - 1 ) ( 1 - MU + R * THETA )
!               / Product ( 0 <= R <= N - 1 )  ( 1 + R * THETA )
!
!      where
!
!        C(N,X) is the combinatorial coefficient;
!        MU is the expectation of the underlying Beta distribution;
!        THETA is a shape parameter.
!
!      A THETA value of 0 ( or A+B --> +oo ) results in the binomial
!      distribution:
!
!        PDF2(X) ( N, MU, 0 ) = C(N,X) * MU^X * ( 1 - MU )^(N-X)
!
!    Given A, B, C for PDF, then the equivalent PDF2 has:
!
!      N     = C
!      MU    = A / ( A + B )
!      THETA = 1 / ( A + B )
!
!    Given N, MU, THETA for PDF2, the equivalent PDF has:
!
!      A = MU / THETA
!      B = ( 1 - MU ) / THETA
!      C = N
!
!    BETA_BINOMIAL_PDF(1,1,C;X) = UNIFORM_DISCRETE_PDF(0,C-1;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_beta
  integer ( kind = 4 ) x

  if ( x < 0 ) then

    pdf = 0.0D+00

  else if ( x <= c ) then

    pdf = r8_beta ( a + real ( x, kind = 8 ), b + real ( c - x, kind = 8 ) ) &
      / ( real ( c + 1, kind = 8 ) &
      * r8_beta ( real ( x + 1, kind = 8 ), &
      real ( c - x + 1, kind = 8 ) ) * r8_beta ( a, b ) )

  else if ( c < x ) then

    pdf = 0.0D+00

  end if

  return
end
