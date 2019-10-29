subroutine poisson_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! POISSON_PDF evaluates the Poisson PDF.
!
!  Discussion:
!
!    PDF(A;X) = EXP ( - A ) * A^X / X!
!
!    PDF(A;X) is the probability that the number of events observed
!    in a unit time period will be X, given the expected number
!    of events in a unit time.
!
!    The parameter A is the expected number of events per unit time.
!
!    The Poisson PDF is a discrete version of the Exponential PDF.
!
!    The time interval between two Poisson events is a random
!    variable with the Exponential PDF.
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
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) x

  if ( x < 0 ) then
    pdf = 0.0D+00
  else
    pdf = exp ( - a ) * a ** x / r8_factorial ( x )
  end if

  return
end
