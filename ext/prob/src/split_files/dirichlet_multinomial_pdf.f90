subroutine dirichlet_multinomial_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! DIRICHLET_MULTINOMIAL_PDF evaluates a Dirichlet Multinomial PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Comb(A,B,X) * ( Gamma(C_Sum) / Gamma(C_Sum+A) )
!      Product ( 1 <= I <= B ) Gamma(C(I)+X(I)) / Gamma(C(I))
!
!    where:
!
!      Comb(A,B,X) is the multinomial coefficient C( A; X(1), X(2), ..., X(B) ),
!      C_Sum = Sum ( 1 <= I <= B ) C(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kenneth Lange,
!    Mathematical and Statistical Methods for Genetic Analysis,
!    Springer, 1997, page 45.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X(B); X(I) counts the number of occurrences of
!    outcome I, out of the total of A trials.
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of different possible outcomes on
!    one trial.
!
!    Input, real ( kind = 8 ) C(B); C(I) is the Dirichlet parameter associated
!    with outcome I.
!
!    Output, real ( kind = 8 ) PDF, the value of the Dirichlet multinomial PDF.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  real ( kind = 8 ) c_sum
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ) pdf_log
  integer ( kind = 4 ) x(b)

  c_sum = sum ( c(1:b) )

  pdf_log = &
    - lgamma ( c_sum + real ( a, kind = 8 ) ) &
    + lgamma ( c_sum ) &
    + lgamma ( real ( a + 1, kind = 8 ) )

  do i = 1, b
    pdf_log = pdf_log &
      + lgamma ( c(i) + real ( x(i), kind = 8 ) ) &
      - lgamma ( c(i) ) &
      - lgamma ( real ( x(i) + 1, kind = 8 ) )
  end do

  pdf = exp ( pdf_log )

  return
end
