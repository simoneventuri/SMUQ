subroutine binomial_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! BINOMIAL_PDF evaluates the Binomial PDF.
!
!  Discussion:
!
!    PDF(A,B;X) is the probability of exactly X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    The formula is:
!
!      PDF(A,B;X) = C(N,X) * B^X * ( 1.0D+00 - B )^( A - X )
!
!    Binomial_PDF(1,B;X) = Bernoulli_PDF(B;X).
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
!    Input, integer ( kind = 4 ) X, the desired number of successes.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
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

  if ( a < 1 ) then

    pdf = 0.0D+00

  else if ( x < 0 .or. a < x ) then

    pdf = 0.0D+00

  else if ( b == 0.0D+00 ) then

    if ( x == 0 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if

  else if ( b == 1.0D+00 ) then

    if ( x == a ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if

  else

    cnk = i4_choose ( a, x )

    pdf = real ( cnk, kind = 8 ) * b ** x * ( 1.0D+00 - b ) ** ( a - x )

  end if

  return
end
