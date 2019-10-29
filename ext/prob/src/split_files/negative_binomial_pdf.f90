subroutine negative_binomial_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_PDF evaluates the Negative Binomial PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = C(X-1,A-1) * B^A * ( 1 - B )^(X-A)
!
!    PDF(A,B;X) is the probability that the A-th success will
!    occur on the X-th trial, given that the probability
!    of a success on a single trial is B.
!
!    The Negative Binomial PDF is also known as the Pascal PDF or
!    the "Polya" PDF.
!
!    NEGATIVE_BINOMIAL_PDF(1,B;X) = GEOMETRIC_PDF(B;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of trials.
!    A <= X.
!
!    Input, integer ( kind = 4 ) A, the number of successes required.
!    0 <= A <= X, normally.
!
!    Input, real ( kind = 8 ) B, the probability of a success on a single trial.
!    0.0 < B <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i4_choose
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x < a ) then

    pdf = 0.0D+00

  else

    cnk = i4_choose ( x - 1, a - 1 )

    pdf = real ( cnk, kind = 8 ) * b ** a * ( 1.0D+00 - b ) ** ( x - a )

  end if

  return
end
