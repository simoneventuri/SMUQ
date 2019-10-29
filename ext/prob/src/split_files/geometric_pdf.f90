subroutine geometric_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! GEOMETRIC_PDF evaluates the Geometric PDF.
!
!  Discussion:
!
!    PDF(A;X) = A * ( 1 - A )^(X-1)
!
!    PDF(A;X) is the probability that exactly X Bernoulli trials, each
!    with probability of success A, will be required to achieve
!    a single success.
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
!    0 < X
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
!
!  Special cases.
!
  if ( x < 1 ) then

    pdf = 0.0D+00

  else if ( a == 0.0D+00 ) then

    pdf = 0.0D+00

  else if ( a == 1.0D+00 ) then

    if ( x == 1 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if

  else

    pdf = a * ( 1.0D+00 - a ) ** ( x - 1 )

  end if

  return
end
