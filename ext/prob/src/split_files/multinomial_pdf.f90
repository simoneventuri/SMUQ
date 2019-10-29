subroutine multinomial_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! MULTINOMIAL_PDF computes a Multinomial PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Comb(A,B,X) * Product ( 1 <= I <= B ) C(I)^X(I)
!
!    where Comb(A,B,X) is the multinomial coefficient
!      C( A; X(1), X(2), ..., X(B) )
!
!    PDF(A,B,C;X) is the probability that in A trials there
!    will be exactly X(I) occurrences of event I, whose probability
!    on one trial is C(I), for I from 1 to B.
!
!    As soon as A or B gets large, the number of possible X's explodes,
!    and the probability of any particular X can become extremely small.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 December 1999
!
!  Author:
!
!    John Burkardt
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
!    Input, real ( kind = 8 ) C(B); C(I) is the probability of outcome I on
!    any one trial.
!
!    Output, real ( kind = 8 ) PDF, the value of the multinomial PDF.
!
  implicit none

  integer ( kind = 4 ) b

  integer ( kind = 4 ) a
  real ( kind = 8 ) c(b)
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ) pdf_log
  integer ( kind = 4 ) x(b)
!
!  To try to avoid overflow, do the calculation in terms of logarithms.
!  Note that Gamma(A+1) = A factorial.
!
  pdf_log = lgamma ( real ( a + 1, kind = 8 ) )

  do i = 1, b
    pdf_log = pdf_log + x(i) * log ( c(i) ) &
      - lgamma ( real ( x(i) + 1, kind = 8 ) )
  end do

  pdf = exp ( pdf_log )

  return
end
