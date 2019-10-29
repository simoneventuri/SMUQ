subroutine bernoulli_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! BERNOULLI_PDF evaluates the Bernoulli PDF.
!
!  Discussion:
!
!    PDF(A;X) = A^X * ( 1 - A )^( X - 1 )
!
!    X = 0 or 1.
!
!    The Bernoulli PDF describes the simple case in which a single trial
!    is carried out, with two possible outcomes, called "success" and
!    "failure"; the probability of success is A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of successes on a single trial.
!    X = 0 or 1.
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

  if ( x < 0 ) then
    pdf = 0.0D+00
  else if ( x == 0 ) then
    pdf = 1.0D+00 - a
  else if ( x == 1 ) then
    pdf = a
  else
    pdf = 0.0D+00
  end if

  return
end
